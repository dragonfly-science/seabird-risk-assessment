library(coda)
library(data.table)
library(parallel)
library(stringr)

options(scipen = 100)

load('../modeldata.rdata', v=F)
load('model-results.rdata', v=F)


## * Turn samples to long form
mcmc <- rbindlist(lapply(1:length(codasamples), function(ch) {
    d <- data.table(codasamples[[ch]])
    d[, chain := ch]
    d[, ch_sample := 1:.N]
    return(melt(d, id.vars = c('chain', 'ch_sample')))
}))

setorder(mcmc, variable, chain, ch_sample)

mcmc[, sample := 1:.N, variable]
## mcmc[, vartype := sub('\\[.*\\]', '', variable)]
## print(mcmc[, .N, vartype])

## * Get meaning out of the parameters

agg_pred[, id := 1:.N]
agg_vul[,  id := 1:.N]

get_meaning <- function(vars, covariate = NULL) {
    ## ** Returns the covariates given the variable name
    ## ** If `covariate` is NULL, returns all the covariates as data.table, otherwise returns a vector of single given covariate

    unvars <- unique(vars)
    
    if (!is.null(covariate) & length(covariate) > 1)  stop('get_meaning only accepts a single covariate to be specified')
    maxdim <- max(str_count(unvars, ',')) + 1
    vartype <- sub('\\[.*\\]', '', unvars)
    ind1 <- rep(NA_integer_, length.out = length(unvars))
    singleidx <- grepl('.*\\[([0-9]+)\\].*', unvars)
    ind1[singleidx] <- as.numeric(sub('.*\\[([0-9]+)\\].*', '\\1', unvars[singleidx]))
    ind2 <- rep(NA_integer_, length.out = length(unvars))
    if (maxdim > 1) {
        firstidx  <- grepl('\\[[0-9]+,', unvars)
        ind1[firstidx] <- as.numeric(sub('.*\\[([0-9]+),.*', '\\1', unvars[firstidx]))
        secondidx <- grepl('\\[[0-9]+,[0-9]+\\]', unvars)
        ind2[secondidx] <- as.numeric(sub('.*,([0-9]+)\\].*', '\\1', unvars[secondidx]))
    }
    dt <- data.table(unvars, vartype, ind1, ind2)
    
    dt[vartype %in% c('apf_t', 'observable_captures_t', 'observable_captures_ident_t', 'incidents_t'),
       c('fishery_group', 'species_group', 'species', 'role', 'grid_id', 'month') :=
           agg_pred[match(ind1, id), .(fishery_group, species_group, species, role, grid_id, month)]]

    dt[vartype %in% c('dead_captures_o', 'live_captures_o', 'loglik_dead_o', 'loglik_live_o',
                      'q_g_o'),
       c('fishery_group', 'species_group', 'species', 'role', 'grid_id', 'month') :=
           agg_vul[match(ind1, id), .(fishery_group, species_group, species, role, grid_id, month)]]

    dt[vartype %in% c('dead_unident_o', 'live_unident_o', 'loglik_unident_dead', 'loglik_unident_live',
                      'p_live_cap', 'q_f', 'q_f0'),
       fishery_group := allfg[ind1]]

    dt[vartype %in% c('ntot', 'ntot_prior'),
       species := allspp[ind1]]

    dt[vartype %in% c('q_g', 'q_g0', 'p_survive_cap'),
       species_group := allspg[ind1]]

    dt[vartype %in% c('eps'), `:=`(species_group = allspg[ind1],
                                   fishery_group = allfg[ind2])]

    dummy_levels <- data.table(expand.grid(species_group = levels(agg_pred[, species_group]),
                                          fishery_group = levels(agg_pred[, fishery_group])))
    dummy_levels[, id := 1:.N]
    dt[vartype %in% c('q_gf', 'q_gf0'),
       c('species_group', 'fishery_group') := dummy_levels[match(ind1, id), .(species_group, fishery_group)]]
       

    
    if (dt[grep('\\[', unvars), any(apply(.SD, 1, function(x) all(is.na(x)))), .SDcols = c('ind1', 'ind2')])
        stop('Some indices not parsed properly')

    nocovs <- dt[, apply(.SD, 1, function(x) all(is.na(x))), .SDcols = setdiff(names(dt), c('unvars', 'vartype', 'ind1', 'ind2'))]
    
    if (any(nocovs & dt[, !(vartype %in% c('p_observable', 'lp__', grep('^log_lik', vartype, val=T)))])) {
        print( dt[nocovs==T & !(vartype %in% c('p_observable', 'lp__', grep('^log_lik', vartype, val=T))), .N, vartype] )
        warning('Some variables with no associated covariates')
    }
    
    dt <- dt[match(vars, unvars)]
    setnames(dt, 'unvars', 'variable')
    
    if (!is.null(covariate)) {
        return(dt[, get(covariate)])
    } else return(dt)
    
}


vars <- levels(mcmc$variable)
vars <- vars[!grepl('_s$', sub('\\[.*\\]', '', vars))]

mc_attributes <- get_meaning(vars)
mc_attributes[grid_id %in% 0, grid_id := NA]
mc_attributes[month   %in% 0, month   := NA]

mcsumm <- mcmc[, .(mean = mean(value),
                  lcl = quantile(value, 0.025, names=F),
                  ucl = quantile(value, 0.975, names=F)),
              .(variable)]
mcsumm <- merge(mcsumm, mc_attributes, on = 'variable')

save(mcmc, mc_attributes, mcsumm, file='mcmc-results.rdata')
saveRDS(mcsumm, file='mcmc-results-summary.rds')

