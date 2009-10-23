print.summary.mvna <- function(x, ...) {

    if (!inherits(x, "summary.mvna"))
        stop("'x' must be of class 'summary.mvna'")

    time <- x[[1]]$time
    qtime <- quantile(time, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))
    ind <- findInterval(qtime, time, all.inside = TRUE)
    namen <- strsplit(names(x), split = " ")
    
    for (i in seq_along(x)) {
        cat(paste("Transition", namen[[i]][1], "->", namen[[i]][2], "\n", sep = " "))
        print(x[[i]][ind, ], row.names = FALSE)
        cat("\n")
    }
    
    invisible()
    
}

