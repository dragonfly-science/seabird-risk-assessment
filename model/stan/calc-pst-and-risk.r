library(data.table)

source('../../functions.r')

load('mcmc-results.rdata', v=T)

spplist <- fread('../../input-data/species-list.csv')[base_species == T]
setkeyv(spplist, 'code')

sppdem <- fread('../../input-data/demographic-parameters-processed.csv')

## * PST
if ('ntot' %in% mc_attributes$vartype) {
    cat('\tCalculating PST...\n')
    
    ntots <- mcmc[variable %in% mc_attributes[vartype == 'ntot', variable]]
    ntots[mc_attributes, species := i.species, on = 'variable']

    n_samples <- max(mcmc$sample)

    spp <- spplist$code

    pstfun <- function(sp1) {

        age_first_breeding <- sppdem[species == sp1 & parameter == 'age_first_breeding', rnorm(n_samples, mean, sd)]
        survival_optimal   <- sppdem[species == sp1 & parameter == 'survival_optimal',   rlnorm(n_samples, mean_log, sd_log)]
        population_total   <- ntots[species == sp1, value]
        
        rmax_nl <- Rmax_NL(survival_optimal, age_first_breeding)

        pst_nl <- 0.5 * population_total * rmax_nl

        return(data.table(species = sp1, sample = 1:n_samples, pst_nl = pst_nl,
                          rmax_nl = rmax_nl, population_total = population_total))
    }
    pst_samples <- rbindlist(lapply(spp, pstfun))


    ## * Add overall nu correction ##
    nu <- 0.5
    pst_samples[, pst_nlc := pst_nl * nu]

    ## Save
    saveRDS(pst_samples, file = 'pst-samples.rds')

    pstsumm <- pst_samples[, .(mean = mean(pst_nlc),
                              lcl = quantile(pst_nlc, 0.025, names = F),
                              ucl = quantile(pst_nlc, 0.975, names = F)), species]

    fwrite(pstsumm, 'pst-samples-summary.csv')

    
} else {
    pst_samples <- NULL
    if (any(file.exists(c('pst-samples-summary.csv', 'pst-samples.rds')))) {
        file.remove(c('pst-samples-summary.csv', 'pst-samples.rds'))
    }
}


## * Risk ratio

if (!is.null(pst_samples)) {
    cat('\tCalculating risk ratios...\n')

    ## * APF
    mcsel <- mcmc[variable %in% mc_attributes[role == 'month1_spatial0' & vartype == 'apf_t', variable]]
    mcsel <- merge(mcsel, mc_attributes, by = 'variable', all.x=T, all.y=F)
    apf <- mcsel[, .(apf = sum(value)), .(species, sample)]

    ## * Risk
    risk_ratio <- merge(pst_samples, apf, by = c('species', 'sample'))
    risk_ratio[, riskratio := apf / pst_nlc]

    saveRDS(risk_ratio, 'risk-ratios.rds')

    ## * Summarise
    risk_summ <- risk_ratio[, .(mean = mean(riskratio),
                               sd   = sd(riskratio),
                               lcl  = quantile(riskratio, 0.025, names = F),
                               ucl  = quantile(riskratio, 0.975, names = F)), species]
    risk_summ <- merge(spplist[, .(code, taxa, common_name)], risk_summ, by.x = 'code', by.y = 'species')
    fwrite(risk_summ, 'risk-ratios-summary.csv')

} else {
    cat('No PST was calculated. Skipping calculation of risk\n')
    if (any(file.exists(c('risk-ratios.rds', 'risk-ratios-summary.csv')))) {
        file.remove(c('risk-ratios.rds', 'risk-ratios-summary.csv'))
    }
}
    
