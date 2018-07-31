library(data.table)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(rgdal)

dir.create('figs', showWarnings=F)

dens <- fread('../derived-data/gridded-distribution-by-species.csv')

pops <- fread('../input-data/demographic-parameters-processed.csv')[parameter == 'population_total']
pops <- rbind(pops, pops[species %in% c('DIP', 'DIQ'), .(species = 'ROY', mean=sum(mean))], fill=T)
setkey(pops, species)

spplist <- fread('../input-data/species-list.csv')

grid <- read_sf('../input-data/southern-hemisphere-5-degree.shp')
world <- read_sf('../input-data/world.shp')

w <- as(world, 'Spatial')
wf <- fortify(w)
g <- as(grid, 'Spatial')
gf <- fortify(g)


## * Gridded distributions

gridsp <- as(grid, 'Spatial')
gridsp <- spChFIDs(gridsp, as.character(gridsp$GRID_ID))
gf <- fortify(gridsp, id='GRID_ID')

worldsp <- as(world, 'Spatial')
wf <- fortify(worldsp)

make_map <- function(griddens) {
        gridsp[['density']] <- NA_real_
        gridsp[['density']] <- griddens[
            match(gridsp$GRID_ID, GRID_ID), pops[sp, mean] * density / 1e6]
        gridsp[is.na(gridsp$density), 'density'] <- 0
        gf$density <- gridsp$density[match(gf$id, as.character(gridsp$GRID_ID))]
        brks <- pretty(gridsp$density)

        g <- ggplot() +
            geom_polygon(data = gf, aes(x = long, y = lat, fill = density, group=group),
                         colour = '#CCCCCC', size = 0.1, na.rm=T) +
            geom_polygon(data = wf, aes(x = long, y = lat, group=group),
                         fill = '#AAAAAA', colour = NA, size = 0.1, na.rm=T) +
            scale_fill_gradientn(name = expression(paste("Density\n(birds / km"^2, ")")),
                                 colours = brewer.pal(9, 'BuPu'), na.value=NA, limits = c(0, max(brks)),
                                 trans = 'sqrt', breaks = brks) +
            theme_void() +
            theme(legend.position=c(0.92, 0.85),
                  legend.margin = margin(5, 2, 2, 2, unit='mm'),
                  legend.background=element_rect(fill = '#00000011'),
                  legend.title.align = 0,
                  legend.text.align = 0,
                  legend.key.width = unit(0.5, 'cm')) +
            coord_map("ortho", orientation = c(-90, 178, 0))
        return(g)
}

spp <- dens[, unique(species_code)]
sp=spp[2]
for (sp in spp) {
    cat(sp, '\n')
    griddens <- dens[species_code == sp]
    g <- make_map(griddens)
    ggsave(sprintf('figs/gridded-distribution-%s.png', sp), width = 7, height = 7)
}
