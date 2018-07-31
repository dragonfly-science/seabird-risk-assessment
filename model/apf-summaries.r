library(data.table)
library(ggplot2)
library(xtable)

source('../../functions.r')
source('../../parameters.r')

spplist  <- fread('../../input-data/species-list.csv')
setkey(spplist, code)
nspp <- nrow(spplist)

load('mcmc-results.rdata')


## * APF by species

mcsel <- mcmc[variable %in% mc_attributes[role == 'month1_spatial0' & vartype == 'apf_t', variable]]
mcsel <- merge(mcsel, mc_attributes, by = 'variable', all.x=T, all.y=F)
apf_sp <- mcsel[, .(value = sum(value)), .(species, sample)][
  , .(mean     = mean(value),
      median   = median(value),
      sd       = sd(value),
      lcl      = quantile(value, 0.025, names=F),
      ucl      = quantile(value, 0.975, names=F),
      nsamples = .N), .(code = species)]
apf_sp <- apf_sp[spplist[base_species == T], on = 'code'][, .(common_name = upper1st(common_name), code, mean , sd, lcl, ucl)]
setorder(apf_sp, -mean)

fwrite(apf_sp, 'assets/apf_by_species.csv')
