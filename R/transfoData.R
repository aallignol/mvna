mvna2tdc <- function(data, state.names, tra, cens.name,
                     cov.states, reversible = TRUE) {
    
    colnames(tra) <- rownames(tra) <- state.names
    t.from <- lapply(1:dim(tra)[2], function(i) {
        rep(rownames(tra)[i], sum(tra[i, ]))
    })
    t.from <- unlist(t.from)
    t.to <- lapply(1:dim(tra)[2], function(i) {
        colnames(tra)[tra[i, ]==TRUE]
    })
    t.to <- unlist(t.to)
    trans <- data.frame(from=t.from, to=t.to)
    namen <- paste(trans[, 1], trans[, 2])

    absorb <- setdiff(levels(trans$to), levels(trans$from))
    transient <- unique(state.names[!(state.names %in% absorb)])

### If not, put counting process like
    if ("time" %in% names(data)) {
        data <- data[order(data$id, data$time), ]
        idd <- as.integer(factor(data$id))
        entree <- double(length(data$time))
        masque <- rbind(1, apply(as.matrix(idd), 2, diff))
        entree <- c(0, data$time[1:(length(data$time) - 1)]) * (masque == 0)
        data <- data.frame(id = data$id, from = data$from,
                           to = data$to, entry = entree, exit = data$time)
        if (sum(data$entry < data$exit) != nrow(data))
            stop("Exit time from a state must be > entry time")
    } else {
        if (sum(data$entry < data$exit) != nrow(data))
            stop("Exit time from a state must be > entry time")
    }
    
### Get rid of these annoying factors, keeping the original
### state.numbers (won't be really possible, especially if
### the state.names are words)
### Found another way: create indices with what's interesting, and then get rid of the factors
###
### New problem: What's going on if we decide that actually the covariate
###              has to keep its level even when we observe a transition
    ind.cens <- data$to == cens.name
    ind.absorb <- data$to %in% absorb
    ind.c <- data$from %in% cov.states
    ind.cov <- matrix(FALSE, nrow = nrow(data), ncol = length(cov.states))
    ind.cov[ind.c, match(data$from[ind.c], cov.states)] <- TRUE

### Let's create the data set now
    id <- data$id
    ## status <- as.integer(levels(data$to))[data$to] * ind.absorb
    status <- integer(nrow(data))
    status[ind.absorb] <- absorb[match(data$to, absorb, nomatch = 0)]
    colnames(ind.cov) <- paste("cov", cov.states, sep = ".")
    ind.cov <- ind.cov * 1
    new.data <- data.frame(id, data$entry, data$exit, ind.cov, status)
    colnames(new.data) <- c("id", "entry", "exit",
                            paste("cov", cov.states, sep = "."),
                            "status")

    new.data
}
