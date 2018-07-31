library(data.table)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

type <- Sys.getenv('RUNLENGTH')
if (type == '') type <- 'quick'

nchains <- 3

## * Data
model_data <- new.env()
load('../data.rdata', envir = model_data)

## Different BETA0 between JAGS and Stan
assign('BETA0', get('BETA0', model_data)^-0.5, envir = model_data)


## * Run model
fit <- stan(file       = 'model.stan', 
           model_name = "SRAmodel",
           pars       = c('apf_t', 'q_t', 'q_o', 'q0', 'q_f0', 'q_g0', 'q_gf0',
                          'p_identified', 'p_live_cap', 'p_observable', 'p_survive_cap',
                          'live_captures_o', 'dead_captures_o', 'live_unident_o', 'dead_unident_o',
                          'incidents_t', 'ntot'),
           include    = TRUE, ## TRUE to only store parameters in `pars`
           data       = model_data, 
           iter       = 500,
           control    = list(max_treedepth = 10),
           chains     = nchains,
           diagnostic_file='diag.txt',
           verbose    = F,
           seed       = 3859285)


codasamples <- As.mcmc.list(fit)
mcmc <- data.table(do.call('rbind', codasamples))


## * Summarise samples
mcsumm <- rbindlist(lapply(names(mcmc), function(x) data.table(var = x,
                                                       mean = mean(mcmc[[x]]),
                                                       lcl  = quantile(mcmc[[x]], 0.025, names=F),
                                                       ucl  = quantile(mcmc[[x]], 0.975, names=F),
                                                       n    = length(mcmc[[x]]))))
mcsumm[, vartype := sub('\\[.*\\]', '', var)]
mcsumm[grep('.*\\[([0-9]+)\\].*', var), ind := as.numeric(sub('.*\\[([0-9]+)\\].*', '\\1', var))]

save(codasamples, mcsumm, fit,
     file = 'model-results.rdata')
