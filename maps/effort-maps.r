library(data.table)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(rgdal)

dir.create('figs', showWarnings=F)

source('../parameters.r')

eff.obs <- fread('../input-data/fishing-effort-observed.csv')
eff.tot <- fread('../input-data/fishing-effort-total.csv')
eff.obs[, quarter := ((month-1) %/% 3)+1]
eff.tot[, quarter := ((month-1) %/% 3)+1]

eff.obs <- eff.obs[, .(effort = sum(effort)), .(fishery_group, quarter, grid_id)]
eff.tot <- eff.tot[year %in% years_prediction, .(effort = sum(effort) / length(years_prediction)),
                  .(fishery_group, quarter, grid_id)]

setnames(eff.obs, 'grid_id', 'GRID_ID')
setnames(eff.tot, 'grid_id', 'GRID_ID')

eff.obs[, effort := effort / 1000]
eff.tot[, effort := effort / 1000]

grid <- read_sf('../input-data/southern-hemisphere-5-degree.shp')
world <- read_sf('../input-data/world.shp')

w <- as(world, 'Spatial')
wf <- fortify(w)
g <- as(grid, 'Spatial')
gf <- fortify(g)


## * Gridded distributions

gridfg <- as(grid, 'Spatial')
gridfg <- spChFIDs(gridfg, as.character(gridfg$GRID_ID))
gf <- fortify(gridfg, id='GRID_ID')

worldfg <- as(world, 'Spatial')
wf <- fortify(worldfg)

make_map <- function(griddens) {
        gridfg[['density']] <- NA_real_
        gridfg[['density']] <- griddens[
            match(gridfg$GRID_ID, GRID_ID), effort]
        gridfg[is.na(gridfg$density), 'density'] <- 0
        gf$density <- gridfg$density[match(gf$id, as.character(gridfg$GRID_ID))]
        brks <- pretty(gridfg$density)

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

fgs <- c(unique(eff.obs$fishery_group), 'ALL')
fg=fgs[2]
efftype='obs'
for (efftype in c('tot', 'obs')) {
    eff <- get(sprintf('eff.%s', efftype))
    for (fg in fgs) {
        cat(fg, '\n')
        quart=2
        for (quart in 1:4) {
            cat('\tquarter', quart, '\n')
            if (fg != 'ALL') {
                griddens <- eff[fishery_group == fg & quarter == quart]
            } else griddens <- eff[quarter == quart, .(fishery_group = fg, effort = sum(effort)), .(GRID_ID)]
            g <- make_map(griddens)
            ggsave(sprintf('figs/gridded-distribution_%s-effort_%s_quarter%i.png', efftype, fg, quart), width = 7, height = 7)
        }
    }
}
