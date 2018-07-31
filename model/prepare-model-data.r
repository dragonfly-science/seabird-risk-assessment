library(data.table)
library(sp)
library(sf)

source('../functions.r')

sppdem          <- fread('../input-data/demographic-parameters-processed.csv')
distributions   <- fread('../derived-data/gridded-distribution-by-species.csv')
all_taxa        <- fread('../input-data/species-list.csv')
capts           <- fread('../input-data/observed-captures.csv')
effort_total    <- fread('../input-data/fishing-effort-total.csv')
effort_observed <- fread('../input-data/fishing-effort-observed.csv')

source('../parameters.r')

effort_observed[, type := 'observed']
effort_total[, type := 'total']
effort <- rbind(effort_observed, effort_total)
effort[, quarter := ((month-1) %/% 3)+1]

spp <- all_taxa[base_species == T, taxa]

setnames(distributions, tolower(names(distributions)))
setnames(distributions, c('species_code'), c('code'))
distributions[all_taxa, taxa := i.taxa, on = 'code']


## * Get overlap

## ** Get all bird densities
grid <- read_sf('../input-data/southern-hemisphere-5-degree.shp')
gdt <- as.data.table(grid)
setnames(gdt, tolower(names(gdt)))

distributions[gdt, area := i.area / 1e6, on = 'grid_id']

sp=spp[1]
birddensities <- rbindlist(lapply(spp, function(sp) {
    dt <- distributions[taxa == sp]
    if (dt[, abs(sum(density * area) - 1)] > 10^-6)
        stop('Distribution for `', basename(sp), '` does not appear to be normalised')
    return(dt)
}))


## * Calculate all overlaps
species=spp[6]
overlaps <- rbindlist(lapply(spp, function(species) {
    dens <- birddensities[taxa == species]
    effort_density <- merge(effort, dens[, .(grid_id, density)], by = c('grid_id'), all.x=T, all.y=F)
    effort_density[is.na(density), density := 0]
    effort_density[, overlap := density * effort]
    if (nrow(effort_density) == 0)
        stop('No overlap between fisheries and ', species)
    effort_density[, taxa := species]
    return(effort_density[, .(taxa, type, fishery_group, year, quarter, month, grid_id, effort, density, overlap)])
}))


## * Add observed captures
capts[, type := 'observed']

overlaps <- merge(overlaps, capts[, -c('species', 'species_group'), with=F],
                 on = c('taxa', 'type', 'fishery_group', 'year', 'month', 'grid_id'),
                 all.x = T, all.y = F, sort = F)

## * Add species code and group
overlaps[all_taxa, `:=`(species = i.code,
                        species_group = i.vul_group), on = 'taxa']

## * Turn species, species group and fishery group to factor

allspp <- setdiff(sort(unique(overlaps$species)), all_taxa[base_species==F, code]) # Species
overlaps[, species := factor(species, levels = c(allspp, all_taxa[base_species==F, code]))]

allspg <- setdiff(sort(unique(overlaps$species_group)), '') # Species groups
overlaps[, species_group := factor(species_group, levels = allspg)]

allfg <- sort(unique(overlaps$fishery_group))
overlaps[, fishery_group := factor(fishery_group, levels = allfg)]

overlaps[, overlap := overlap / 1e6]


## * Aggregate data

## ** For estimation
agg_vul <- overlaps[type == 'observed' & year %in% years_estimation,
                  .(grid_id = 0, month = 0, captures = sum(captures), alive = sum(alive), dead = sum(dead), overlap = sum(overlap)),
                  .(species, species_group, fishery_group)]

## ** For predictions
nyears_pred <- length(years_prediction)
agg_pred <- rbind(
    ## *** to compare predictions vs. observations (sum over years)
    overlaps[type == 'observed' & year %in% years_estimation,
             .(role = 'obs_vs_pred',
               grid_id=0, month=0, overlap=sum(overlap), captures=sum(captures), alive=sum(alive), dead=sum(dead)),
             .(species, species_group, fishery_group)],
    ## *** by month, not spatial (annual mean)
    overlaps[type == 'total' & year %in% years_prediction,
             .(role = 'month1_spatial0', grid_id=0, overlap = sum(overlap) / nyears_pred),
             .(species, species_group, fishery_group, month)],
    ## *** spatial, no month (annual mean)
    overlaps[type == 'total' & year %in% years_prediction,
             .(role = 'month0_spatial1', month=0,   overlap = sum(overlap) / nyears_pred),
             .(species, species_group, fishery_group, grid_id)],
    fill=T)



## * Model inputs

## ** Species and groups

N_SPECIES  <- length(allspp)
N_SPECIES_VUL_GROUP <- length(allspg)
N_FISHERY_GROUP <- length(allfg)

## ** Total populations

NTOT_MEAN <- NTOT_SD <- NTOT_LOG_MEAN <- NTOT_LOG_SD <- NTOT_MIN <- NTOT_MAX <- SA_BETA_ALPHA <- SA_BETA_BETA <- rep(0, N_SPECIES)
for (i in 1:N_SPECIES) {
    s <- allspp[i]
    NTOT_MEAN[i] <- sppdem[species == s & parameter == 'population_total', mean]
    NTOT_SD[i]   <- sppdem[species == s & parameter == 'population_total', sd]
    NTOT_LOG_MEAN[i] <- sppdem[species == s & parameter == 'population_total', mean_log]
    NTOT_LOG_SD[i]   <- sppdem[species == s & parameter == 'population_total', sd_log]
    NTOT_MIN[i] <- sppdem[species == s & parameter == 'population_total', min]
    NTOT_MAX[i] <- sppdem[species == s & parameter == 'population_total', max]
}

## ** Intercept
BETA0 <- 0.25


## ** P_observable

binlik <- function(p, n=30, k=2)
    ## Binomial likelihood function
    return(choose(n, k) * p^k * (1-p)^(n-k))

binsample <- function(n=30, k=2, ns=4000, ns0=1e7, ...) {
    ## Draw binomial sample
    x0 <- runif(ns0)
    y0 <- binlik(x0, n, k)
    keep <- runif(ns0) < y0
    x <- x0[keep]
    x <- sample(x, ns, ...)
    return(x)
}
## from Brothers 2010: out of 6000 birds taking baits, 176 caught and 85 retrieved
p_obs <- binsample(176, 85)

POBS_CAUGHT <- 176
POBS_RETRIEVED <- 85

betapars <- estBetaParams(mean(p_obs), var(p_obs))
POBS_BETA_ALPHA <- betapars$alpha
POBS_BETA_BETA  <- betapars$beta

## ** Unidentified captures

unidents <- capts[species %in% all_taxa[base_species == F, code] & year %in% years_estimation]
unid <- unidents[
  , .(captures = sum(captures), alive = sum(alive), dead = sum(dead))
  , .(species, fishery_group)][order(species, fishery_group)]
                                                            
DEAD_UNIDENT_O <- unid$dead
LIVE_UNIDENT_O <- unid$alive

## *** Sort data and index groups to sum over in model
setorder(agg_vul, fishery_group, species_group, species)
idx <- agg_vul[, .(start=min(.I), end=max(.I)), .(fishery_group)][order(fishery_group)]
START_F_O <- idx$start
END_F_O   <- idx$end


## ** Overlaps, kills

## *** Observed for estimation
N_ROW_O <- nrow(agg_vul)
OVERLAP_O <- agg_vul[, overlap]
LIVE_CAPTURES_O <- agg_vul[, alive]
DEAD_CAPTURES_O <- agg_vul[, dead]
SPECIES_O <- agg_vul[, as.numeric(species)]
SPECIES_GROUP_O <- agg_vul[, as.numeric(species_group)]
FISHERY_GROUP_O <- agg_vul[, as.numeric(fishery_group)]

## *** Total for prediction
N_ROW_T <- nrow(agg_pred)
OVERLAP_T <- agg_pred[, overlap]
SPECIES_T <- agg_pred[, as.numeric(species)]
SPECIES_GROUP_T <- as.numeric(agg_pred[, species_group])
FISHERY_GROUP_T <- agg_pred[, as.numeric(fishery_group)]

## ** Base levels
BASE_LEVEL_G <- ifelse(allspg == agg_vul[, .(ov=sum(overlap)), species_group][order(-ov)][1, species_group], 1L, 0L)
BASE_LEVEL_F <- ifelse(allfg  == agg_vul[, .(ov=sum(overlap)), fishery_group][order(-ov)][1, fishery_group], 1L, 0L)




## * For stan 
NTOT <- exp(NTOT_LOG_MEAN)

DUMMYVAR_SPECIES_GROUP_O <- model.matrix(data=data.frame(y=OVERLAP_O, x=as.factor(SPECIES_GROUP_O)), y~x-1)
DUMMYVAR_FISHERY_GROUP_O <- model.matrix(data=data.frame(y=OVERLAP_O, x=as.factor(FISHERY_GROUP_O)), y~x-1)
DUMMYVAR_INTERACTION_O <- model.matrix(data=data.frame(y=OVERLAP_O, x1=as.factor(SPECIES_GROUP_O), x2=as.factor(FISHERY_GROUP_O)), y~x1:x2-1)
N_INTERACTION_CAT <- ncol(DUMMYVAR_INTERACTION_O)
DUMMYVAR_ALL_CATEGORIES_O <-  model.matrix(data=data.frame(y=OVERLAP_O, x1=as.factor(SPECIES_GROUP_O), x2=as.factor(FISHERY_GROUP_O)), y~x1+x2)
N_ALL_CATEGORIES <- ncol(DUMMYVAR_ALL_CATEGORIES_O)

DUMMYVAR_SPECIES_GROUP_T <- model.matrix(data=data.frame(y=OVERLAP_T, x=as.factor(SPECIES_GROUP_T)), y~x-1)
DUMMYVAR_FISHERY_GROUP_T <- model.matrix(data=data.frame(y=OVERLAP_T, x=as.factor(FISHERY_GROUP_T)), y~x-1)
DUMMYVAR_INTERACTION_T <- model.matrix(data=data.frame(y=OVERLAP_T, x1=as.factor(SPECIES_GROUP_T), x2=as.factor(FISHERY_GROUP_T)), y~x1:x2-1)

DUMMYVAR_ALL_CATEGORIES_T <-  model.matrix(data=data.frame(y=OVERLAP_T, x1=as.factor(SPECIES_GROUP_T), x2=as.factor(FISHERY_GROUP_T)), y~x1+x2)


FIXED_LEVEL_G    <- which(BASE_LEVEL_G == 1L)
NOTFIXED_LEVEL_G <- which(BASE_LEVEL_G != 1L)
LEVEL_G  <- c(FIXED_LEVEL_G, NOTFIXED_LEVEL_G)

FIXED_LEVEL_F    <- which(BASE_LEVEL_F == 1L)
NOTFIXED_LEVEL_F <- which(BASE_LEVEL_F != 1L)
LEVEL_F  <- c(FIXED_LEVEL_F, NOTFIXED_LEVEL_F)


## * Save

save.image(file='modeldata.rdata')


save(N_SPECIES, N_SPECIES_VUL_GROUP, N_FISHERY_GROUP, POBS_BETA_ALPHA, POBS_BETA_BETA,
     NTOT_MEAN, NTOT_SD, NTOT_LOG_MEAN, NTOT_LOG_SD, NTOT_MIN, NTOT_MAX, POBS_CAUGHT, POBS_RETRIEVED,
     BETA0,
     LIVE_CAPTURES_O, DEAD_CAPTURES_O,
     N_ROW_O, N_ROW_T,
     FISHERY_GROUP_O, FISHERY_GROUP_T, SPECIES_O, SPECIES_T,
     SPECIES_GROUP_O, OVERLAP_O, 
     SPECIES_GROUP_T, OVERLAP_T,
     START_F_O, END_F_O, DEAD_UNIDENT_O, LIVE_UNIDENT_O,
     BASE_LEVEL_G, BASE_LEVEL_F,
     NTOT, DUMMYVAR_SPECIES_GROUP_O, DUMMYVAR_FISHERY_GROUP_O, DUMMYVAR_INTERACTION_O,
     N_INTERACTION_CAT, DUMMYVAR_SPECIES_GROUP_T, DUMMYVAR_FISHERY_GROUP_T,
     N_ALL_CATEGORIES, DUMMYVAR_ALL_CATEGORIES_O,
     DUMMYVAR_ALL_CATEGORIES_T,
     DUMMYVAR_INTERACTION_T,
     LEVEL_G, LEVEL_F,
     file = 'data.rdata')

