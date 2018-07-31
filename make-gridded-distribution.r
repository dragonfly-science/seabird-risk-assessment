library(data.table)
library(sf)

shps <- dir('input-data/species-distributions', '*.shp', full.names=T)

## * One single file
file = shps[1]
densities <- rbindlist(lapply(shps, function(file) {
    shp <- st_read(file, quiet=T)
    dshp <- as.data.table(shp)
    setnames(dshp, 'species_co', 'species_code')
    return(dshp[, .(grid_id, species_code, taxa, density)])
}))

densities[, density := density * 1e6]
setnames(densities, 'grid_id', 'GRID_ID')

fwrite(densities, 'derived-data/gridded-distribution-by-species.csv')
