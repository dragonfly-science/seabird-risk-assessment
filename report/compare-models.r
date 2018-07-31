library(data.table)
library(ggplot2)
library(xtable)

source('../functions.r')
source('../labs.r')

models <- basename(rownames(subset(file.info(dir('../model', full.names=T)), isdir==T)))

## * APF by species
cat('\nAPF by species\n')
mod=models[1]
model_comparison <- rbindlist(lapply(models, function(mod) {

    cat('\t', mod, '\n')
    load(sprintf('../model/%s/mcmc-results.rdata', mod))

    comp='apf-by-species'
    comps <- rbindlist(lapply(c('apf-by-species', 'vulnerabilities'), function(comp) {
        if (comp == 'apf-by-species') {
            mcsel <- mcmc[variable %in% mc_attributes[vartype == 'apf_t' & role == 'month1_spatial0', variable]]
            mcsel <- merge(mcsel, mc_attributes, by = 'variable', all.x=T, all.y=F)
            mcsel[, byvar1 := as.character(species)]
            mcsel[, byvar2 := '']
        } else if (comp == 'vulnerabilities') {
            mcsel <- mcmc[variable %in% mc_attributes[vartype %in% c('q0', 'q_f', 'q_g', 'eps'), variable]]
            mcsel <- merge(mcsel, mc_attributes, by = 'variable', all.x=T, all.y=F)
            mcsel[, byvar1 := ifelse(vartype == 'q0', '',
                              ifelse(vartype == 'q_f', as.character(fishery_group),
                              ifelse(vartype == 'q_g', as.character(species_group),
                              ifelse(vartype == 'eps', as.character(species_group), ''))))]
            mcsel[, byvar2 := ifelse(vartype == 'eps', as.character(fishery_group), '')]
        } else stop('Unknown comparison')
        res <- mcsel[, .(value = sum(value)), .(vartype, byvar1, byvar2, sample)][
          , .(comparison = comp,
              model    = mod,
              mean     = mean(value),
              median   = median(value),
              sd       = sd(value),
              lcl      = quantile(value, 0.025, names=F),
              ucl      = quantile(value, 0.975, names=F),
              nsamples = .N), .(vartype, byvar1, byvar2)]
        res <- res[
          , .(comparison, model, vartype, byvar1, byvar1_lab = labs[byvar1], byvar2, byvar2_lab = labs[byvar2],
              mean , sd, lcl, ucl)]
        res[, comparison := comp]
        setorder(res, -mean)
        return(res)
    }))
    return(comps)
}))

setorder(model_comparison, comparison, vartype, byvar1, byvar2, model)

fwrite(model_comparison, 'assets/model_comparison.csv')

