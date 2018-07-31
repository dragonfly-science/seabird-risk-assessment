## * Utilities
upper1st <- function(x) {
    if (is.factor(x))
        x0 <- levels(x) else x0 <- as.character(x)
    nc <- nchar(x0)
    x0[nc > 1] <- sprintf('%s%s', toupper(substr(x0[nc > 1], 1, 1)), substr(x0[nc > 1], 2, nchar(x0[nc > 1])))
    x0[nc == 1] <- toupper(x0[nc == 1])
    if (is.factor(x))
        levels(x) <- x0 else x <- x0
    return(x)
}

## * Statistical
estBetaParams <- function(mu, var) {
    ## Estimate alpha and beta of beta distribution from mean and variance
    ## from http://stats.stackexchange.com/questions/12232/calculating-the-parameters-of-a-beta-distribution-using-the-mean-and-variance
    alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
    beta <- alpha * (1 / mu - 1)
    return(params = list(alpha = alpha, beta = beta))
}


## * For PST
## formula from Neil & Lebreton 2008
funNL<- function(x,a,s) (exp((a+s/(x-s))^-1)-x)^2
lmax_nl <- function(a,s) return(optimise(funNL, c(1,2), a=a, s=s, tol=1e-10)$minimum)
Lmax_nl <- function(a,s,usemc=F) {
    if (usemc) {
        library(parallel)
        return(mcmapply(lmax_nl, a, s))
    } else return(mapply(lmax_nl, a, s))
}
Rmax_NL <- function(s,a,...) return(Lmax_nl(a,s,...)-1)
