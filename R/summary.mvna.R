summary.mvna <- function(object, level = 0.95,
                         var.type = c("aalen", "greenwood"),
                         ci.fun = c("log", "linear", "arcsin"),
                         ...) {
    if (!inherits(object, "mvna")) {
        stop("'object' must be of class 'mvna'")
    }
    if (level <= 0 || level > 1) {
        stop("The significance levels of the 2 sided confidence intervals\nmust be between 0 and 1")
    }
    var.type <- match.arg(var.type)
    ci.fun <- match.arg(ci.fun)

    ## we just need the information relative to the transitions
    temp <- object[match(paste(object$trans[, 1], object$trans[, 2]),
                         names(object))]

    zzz <- lapply(temp, function(x) {
        na <- x$na
        var <- x[, grep(var.type, names(x))]
        time <- x$time
        
        z <- qnorm(level + (1 - level) / 2)
        switch(ci.fun,
               "log" = {
                   upper <- na * exp((z * sqrt(var)) / na)
                   lower <- na * exp((-z * sqrt(var)) / na)
               },
               "linear" = {
                   upper <- na + z * sqrt(var)
                   lower <- na - z * sqrt(var)
               },
               "arcsin" = {
                   upper <- -2 * log(sin(pmax(0, asin(exp(-na / 2)) -
                                             (1 / 2) * z * sqrt(var) *
                                             (exp(na) - 1)^(-1 / 2))))
                   lower <- 2 * log(sin(pmin(pi / 2, asin(exp(-na / 2)) +
                                             (1 / 2) * z * sqrt(var) *
                                             (exp(na) - 1)^(- 1 / 2))))
               })
        upper[is.nan(upper)] <- 0
        lower[is.nan(lower)] <- 0

        ## ind <- object$n.risk[, as.character(object$trans[count, 1])] != 0
##         n.risk <- object$n.risk[ind, as.character(object$trans[count, 1])]
##         n.event <- object$n.event[as.character(object$trans[count, 1]),
##                                   as.character(object$trans[count, 2]),
##                                   ind]

        data.frame(time, na, var, lower, upper)
    })
    zzz
}
