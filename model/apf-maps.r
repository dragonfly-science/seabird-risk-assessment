library(data.table)
library(ggplot2)
library(RColorBrewer)
library(sf)

source('../../functions.r')
source('../../parameters.r')

spplist  <- fread('../../input-data/species-list.csv')
setkey(spplist, code)
nspp <- nrow(spplist)

load('mcmc-results.rdata')
mcsumm <- readRDS('mcmc-results-summary.rds')

grid <- read_sf('../../input-data/southern-hemisphere-5-degree.shp')
world <- read_sf('../../input-data/world.shp')


## * APF by species

mcsel <- mcmc[variable %in% mc_attributes[role == 'month0_spatial1' & vartype == 'apf_t', variable]]
mcsel <- merge(mcsel, mc_attributes, by = 'variable', all.x=T, all.y=F)
apf_sp <- mcsel[, .(value = sum(value)), .(species, grid_id, sample)][
  , .(mean     = mean(value),
      median   = median(value),
      sd       = sd(value),
      lcl      = quantile(value, 0.025, names=F),
      ucl      = quantile(value, 0.975, names=F),
      nsamples = .N), .(code = species, grid_id)]

apf_sp <- apf_sp[spplist[base_species == T], on = 'code'][, .(common_name = upper1st(common_name), code, grid_id, mean , sd, lcl, ucl)]
apf_sp

spp <- spplist[base_species == T]
for (i in 1:nrow(spp)) {
    apf1 <- apf_sp[code == spp[i, code]]
    sp_apf <- apf1[match(grid$GRID_ID, grid_id), mean]
    sp_apf[sp_apf == 0] <- NA
    grid[[spp[i, code]]] <- sp_apf
}

grid2 <- sf::st_transform(grid, "+proj=laea +y_0=0 +lon_0=179 +lat_0=-90 +ellps=WGS84 +no_defs")
world2 <- sf::st_transform(world, "+proj=laea +y_0=0 +lon_0=179 +lat_0=-90 +ellps=WGS84 +no_defs")
w2 <- st_as_sf(raster::crop(as(world, 'Spatial'), raster::extent(as(grid, 'Spatial'))))
w2 <- sf::st_transform(w2, "+proj=laea +y_0=0 +lon_0=179 +lat_0=-90 +ellps=WGS84 +no_defs")


sp='DQS'
for (sp in spp$code) {
    cat(sp, '\n')
    grid2$sp <- grid2[[sp]]
    g <- ggplot() +
        geom_sf(data = grid2, aes(fill = sp), colour = '#CCCCCC', size = 0.1, na.rm=T) +
        geom_sf(data = w2, fill = '#AAAAAA', colour = NA, size = 0.1, na.rm=T) +
        scale_fill_gradientn(name = 'APF', colours = brewer.pal(9, 'BuPu'), na.value=NA, limits = c(0, NA)) +
        theme_void() +
        theme(legend.position=c(0.95, 0.15))
    ggsave(sprintf('assets/apf-map-%s.png', sp), width = 7, height = 7)
}
