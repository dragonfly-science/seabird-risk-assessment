library(data.table)
library(ggplot2)
library(xtable)

source('../../functions.r')
source('../../parameters.r')
source('../../labs.r')
load('../data.rdata')

spplist  <- fread('../../input-data/species-list.csv')
setkey(spplist, code)
nspp <- nrow(spplist)
sppdem  <- fread('../../input-data/demographic-parameters-processed.csv')
capts <- fread('../../input-data/observed-captures.csv')

mcsumm <- readRDS('mcmc-results-summary.rds')
setorder(mcsumm, vartype, ind1, ind2)

## * Observed captures

cat('\n* Observed captures\n\n')

if (all(c('live_unident_o', 'dead_unident_o', 'live_captures_o', 'dead_captures_o') %in% mcsumm$vartype)) {
    live_unidents  <- cbind(mcsumm[vartype == 'live_unident_o'],  obs = LIVE_UNIDENT_O)
    dead_unidents  <- cbind(mcsumm[vartype == 'dead_unident_o'],  obs = DEAD_UNIDENT_O)
    live_obs_capts <- cbind(mcsumm[vartype == 'live_captures_o'], obs = LIVE_CAPTURES_O)
    dead_obs_capts <- cbind(mcsumm[vartype == 'dead_captures_o'], obs = DEAD_CAPTURES_O)

    pred_vs_obs <- rbind(live_unidents, dead_unidents, live_obs_capts, dead_obs_capts, fill = T)
    pred_vs_obs[, out := (obs < lcl | obs > ucl)]
    cat(sprintf('\n\tNumber of strata with observed value outside 95%% c.i. = %i out of %i strata (%0.2f%%)\n',
                pred_vs_obs[, sum(out)], nrow(pred_vs_obs), 100 * pred_vs_obs[, mean(out)]))

    pred_vs_obs[, lab := labs[vartype]]
    g <- ggplot(pred_vs_obs, aes(x = obs, y = mean, ymin = lcl, ymax = ucl, colour = lab)) +
        geom_errorbar() +
        geom_point() +
        geom_abline(intercept=0, slope=1) +
        scale_x_continuous(trans = 'log1p') +
        scale_y_continuous(trans = 'log1p') +
        scale_colour_discrete(name = NULL) +
        theme_minimal() +
        theme(legend.position=c(0.8, 0.15)) +
        labs(x = 'Observed number of captures', y = 'Predicted number of captures')
    ggsave('assets/observed-vs-predicted-captures.png', width = 7, height = 5)

    fwrite(pred_vs_obs, 'assets/observed-vs-predicted-captures.csv')
} else {
    if (any(file.exists(c('assets/observed-vs-predicted-captures.png', 'assets/observed-vs-predicted-captures.csv')))) {
        file.remove(c('assets/observed-vs-predicted-captures.png', 'assets/observed-vs-predicted-captures.csv'))
    }
}


## * Vulnerabilities

cat('\n* Vulnerabilities\n\n')

load('../../test-data/vulnerabilities.rdata', v=F)

if ('q0' %in% mcsumm$vartype) {
    q0  <- cbind(par = 'q0', mcsumm[vartype == 'q0'], vulnerability = vulnerabilities$q0)
} else q0  <- NULL

if ('q_f' %in% mcsumm$vartype) {
    qf  <- cbind(par = 'q_f',
                merge(mcsumm[vartype == 'q_f'], vulnerabilities$q_f, by.x = 'fishery_group', by.y = 'fgroup'))
} else if ('q_f0' %in% mcsumm$vartype) {
    qf  <- cbind(par = 'q_f',
                merge(mcsumm[vartype == 'q_f0'], vulnerabilities$q_f, by.x = 'fishery_group', by.y = 'fgroup'))
} else qf <- NULL

if ('q_g' %in% mcsumm$vartype) {
    qg  <- cbind(par = 'q_g',
                merge(mcsumm[vartype == 'q_g'],  vulnerabilities$q_g, by.x = 'species_group', by.y = 'sgroup'))
} else if ('q_g0' %in% mcsumm$vartype) {
    qg  <- cbind(par = 'q_g',
                merge(mcsumm[vartype == 'q_g0'],  vulnerabilities$q_g, by.x = 'species_group', by.y = 'sgroup'))
} else qg <- NULL

if ('eps' %in% mcsumm$vartype) {
    qfg <- cbind(par = 'q_fg',
                merge(mcsumm[vartype == 'eps'], vulnerabilities$q_fg[, .(fgroup, sgroup, vulnerability = vulnerability)],
                      by.x = c('fishery_group', 'species_group'), by.y = c('fgroup', 'sgroup'), all=T))
} else if ('q_gf0' %in% mcsumm$vartype) {
    qfg <- cbind(par = 'q_fg',
                merge(mcsumm[vartype == 'q_gf0'], vulnerabilities$q_fg[, .(fgroup, sgroup, vulnerability = vulnerability)],
                      by.x = c('fishery_group', 'species_group'), by.y = c('fgroup', 'sgroup'), all=T))
} else qfg <- NULL

vuls <- rbind(q0, qf, qg, qfg)

if (!is.null(vuls)) {

    vuls[, out := (vulnerability < lcl | vulnerability > ucl)]
    cat(sprintf('\n\tNumber of strata with observed value outside 95%% c.i. = %i out of %i strata (%0.2f%%)\n',
                vuls[, sum(out)], nrow(vuls), 100 * vuls[, mean(out)]))
    vuls[, lab := labs[par]]
    vuls[, lab := factor(lab, levels = c('Intercept', 'Fishery group', 'Species group', 'Species group x Fishery group'))]

    g <- ggplot(vuls, aes(x = vulnerability, y = mean, ymin = lcl, ymax = ucl, colour = lab)) +
        geom_errorbar() +
        geom_point() +
        geom_abline(intercept=0, slope=1) +
        scale_x_continuous(trans = 'log1p') +
        scale_y_continuous(trans = 'log1p') +
        scale_color_discrete(name = NULL) +
        theme_minimal() +
        theme(legend.position=c(0.18, 0.87)) +
        labs(x = 'Simulated vulnerability', y = 'Estimated vulnerability')
    ggsave('assets/observed-vs-predicted-vulnerabilities.png', width = 7, height = 5)

    fwrite(vuls, 'assets/observed-vs-predicted-vulnerabilities.csv')

} else {
    if (any(file.exists(c('assets/observed-vs-predicted-vulnerabilities.png', 'assets/observed-vs-predicted-vulnerabilities.csv')))) {
        file.remove(c('assets/observed-vs-predicted-vulnerabilities.png', 'assets/observed-vs-predicted-vulnerabilities.csv'))
    }
}


## * P(live cap)

cat('\n* P(live cap)\n\n')
source('../../test-data/parameters.r')
if ('p_live_cap' %in% mcsumm$vartype) {
    
    palives <- cbind(mcsumm[vartype == 'p_live_cap'], obs = p_alive)
    palives[, out := (obs < lcl | obs > ucl)]
    cat(sprintf('\n\tNumber of strata with observed value outside 95%% c.i. = %i out of %i strata (%0.2f%%)\n',
                palives[, sum(out)], nrow(palives), 100 * palives[, mean(out)]))

    fwrite(palives, 'assets/observed-vs-predicted-p_alive.csv')

} else {
    if (file.exists('assets/observed-vs-predicted-p_alive.csv')) {
        file.remove('assets/observed-vs-predicted-p_alive.csv')
    }
}


## * Compare total incidents with generated ones

cat('\n* Incidents\n\n')
if ('incidents_t' %in% mcsumm$vartype) {
    
    incidents <- mcsumm[vartype == 'incidents_t' & role == 'obs_vs_pred']

    gens <- fread('../../test-data/overlaps-with-captures.csv')
    gens <- gens[year %in% years_estimation,
                .(obs = sum(incidents_tot)),
                .(fishery_group, species_group, species)]

    incidents <- merge(incidents, gens, by = c('fishery_group', 'species_group', 'species'))
    incidents[, out := (obs < lcl | obs > ucl)]
    cat(sprintf('\n\tNumber of strata with observed value outside 95%% c.i. = %i out of %i strata (%0.2f%%)\n',
                incidents[, sum(out)], nrow(incidents), 100 * incidents[, mean(out)]))

    g <- ggplot(incidents, aes(x = obs, y = mean, ymin = lcl, ymax = ucl, colour = species)) +
        geom_errorbar() +
        geom_point() +
        geom_abline(intercept=0, slope=1) +
        scale_x_continuous(trans = 'log1p') +
        scale_y_continuous(trans = 'log1p') +
        scale_color_discrete(name = 'Species') +
        theme_minimal() +
        labs(x = 'Simulated incidents', y = 'Estimated incidents')
    ggsave('assets/observed-vs-predicted-incidents.png', width = 7, height = 5)

    fwrite(incidents, 'assets/observed-vs-predicted-incidents.csv')

} else {
    if (any(file.exists(c('assets/observed-vs-predicted-incidents.csv', 'assets/observed-vs-predicted-incidents.png')))) {
        file.remove(c('assets/observed-vs-predicted-incidents.csv', 'assets/observed-vs-predicted-incidents.png'))
    }
}



## * Other parameters

load('mcmc-results.rdata', v=T)
nsamples <- max(mcmc$sample)

if (any(c('p_identified', 'p_live_cap', 'p_observable', 'p_survive_cap') %in% mcsumm$vartype)) {
    
    mc <- mcmc[grep('^p_', variable, perl=T)]
    mc <- merge(mc, mc_attributes, by = c('variable'), all.x=T, all.y=F)
    mc[, lab1 := sprintf('%s%s', ifelse(is.na(fishery_group), '', as.character(fishery_group)),
                         ifelse(is.na(species_group), '', as.character(species_group)))]
    mc[, type := 'Posterior']
    
    priors <- rbindlist(lapply(unique(mc$variable), function(v) {
        if (grepl('p_observable', v)) {
            data.table(variable = v, sample = 1:1e5, value = rbeta(1e5, POBS_BETA_ALPHA, POBS_BETA_BETA))
        } else {
            data.table(variable = v, sample = 1:1e5, value = rbeta(1e5, 1, 1))
        }
    }))
    priors[, type := 'Prior']
    priors[unique(mc[, .(variable, lab1, vartype)]), `:=`(lab1 = i.lab1, vartype = i.vartype), on = 'variable']

    mc <- rbind(mc[, .(variable, vartype, lab1, type, sample, value)],
               priors[, .(variable, vartype, lab1, type, sample, value)])
    
    mc[vartype == 'p_identified',  `:=`(real = p_identified, label = 'P(Identified)')]
    mc[vartype == 'p_live_cap',    `:=`(real = p_alive, label = sprintf('P(Live capture) - %s', lab1))]
    mc[vartype == 'p_observable',  `:=`(real = p_observable, label = 'P(Observable)')]
    mc[vartype == 'p_survive_cap', `:=`(label = sprintf('P(Survive capture) - %s', lab1))]

    mc[, type := factor(type, levels = c('Prior', 'Posterior'))]
    
    g <- ggplot(mc) +
        geom_density(aes(x = value, y = ..scaled.., fill = type), alpha = 0.6, size = 0.1, colour = 'black') +
        facet_wrap(~ label, scales = 'free_y') +
        geom_vline(aes(xintercept = real), colour = "#E31A1C") +
        scale_x_continuous(limits = c(0, 1)) +
        scale_fill_manual(name = NULL, values = c(Prior = 'grey70', Posterior = "#238B45")) +
        labs(x = 'Probability', y = 'Density') +
        theme_minimal() +
        theme(panel.grid.major.y=element_blank(),
              panel.grid.minor.y=element_blank())

    ggsave('assets/comparison-with-actual_probabilities.png', width = 9, height = 7)

}


## * Population sizes

if ('ntot' %in% mcsumm$vartype) {
    
    load('../modeldata.rdata', v=T)
    ntots <- rbindlist(lapply(1:length(allspp), function(i) {
        data.table(sp = allspp[i], type = 'Prior', sample = 1:1e5,
                   pop = rlnorm(1e5, NTOT_LOG_MEAN[i], NTOT_LOG_SD[i]))
    }))
    mc <- mcmc[grep('^ntot', variable, perl=T)]
    mc <- merge(mc, mc_attributes, by = c('variable'), all.x=T, all.y=F)
    ntots <- rbind(ntots,
                  mc[, .(sp = allspp[ind1], type = 'Posterior', sample = 1:max(mc$sample), pop = value)])
    ntots[, spname := labs[sp]]
    ntots[, type := factor(type, levels = c('Prior', 'Posterior'))]
    
    ntots_means <- fread('../../input-data/demographic-parameters-processed.csv')[
        parameter == 'population_total' & species != 'ROY']
    setnames(ntots_means, 'species', 'sp')
    ntots_means[, spname := labs[sp]]
    ntots_means[, type := 'Value used for the simulations']

    g <- ggplot(ntots) +
        geom_density(aes(pop, fill = type), alpha=0.6, size = 0.1, colour = 'black') +
        geom_vline(aes(xintercept = mean, colour = type), data = ntots_means) +
        scale_fill_manual(name = NULL, values = c(Prior = 'grey50', Posterior = "#238B45")) +
        scale_colour_manual(name = NULL, values = c('Value used for the simulations' = "#E31A1C")) +
        facet_wrap(~ upper1st(spname), scales = 'free') +
        labs(x = 'Total population (individuals)', y = 'Density') +
        theme_minimal() +
        theme(panel.grid.major.y=element_blank(),
              panel.grid.minor.y=element_blank(),
              legend.position=c(0.85, 0.15),
              legend.spacing=unit(0,'mm'),
              axis.text.y = element_text(size = 5))
    
    ggsave('assets/comparison-with-actual_populations.png', width = 9, height = 7)

}

