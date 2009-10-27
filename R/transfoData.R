mvna2tdc <- function(data, state.names, tra, cens.name, cov.states, ...) {

    if (!is.data.frame(data))
        stop("Argument 'data' must be a data.frame")
    if (!(xor(sum(c("id", "from", "to", "time") %in% names(data)) != 4,
              sum(c("id", "from", "to", "entry", "exit") %in% names(data)) != 5)))
        stop("'data' must contain the right variables")
    if (nrow(tra) != ncol(tra))
        stop("Argument 'tra' must be a quadratic  matrix.")
    if (sum(diag(tra)) > 0)
        stop("transitions into the same state are not allowed")
        if (nrow(tra) != length(state.names)) {
        stop("The row number of 'tra' must be equal to the number of states.")
    }
    if (!is.logical(tra)) {
        stop("'tra' must be a matrix of logical values, which describes the possible transitions.")
    }
    if (length(state.names) != length(unique(state.names))) {
        stop("The state names must be unique.")
    }
    if (!(is.null(cens.name))) {
        if (cens.name %in% state.names) {
            stop("The name of the censoring variable just is a name of the model states.")
        }
    }

    ### transitions
    colnames(tra) <- rownames(tra) <- state.names
    t.from <- lapply(1:dim(tra)[2], function(i) {
        rep(rownames(tra)[i], sum(tra[i, ]))
    })
    t.from <- unlist(t.from)
    t.to <- lapply(1:dim(tra)[2], function(i) {
        colnames(tra)[tra[i, ] == TRUE]
    })
    t.to <- unlist(t.to)
    trans <- data.frame(from = t.from, to = t.to)
    namen <- paste(trans[, 1], trans[, 2])
    ## test on transitions
##     test <- unique(paste(data$from, data$to))
##     if (!(is.null(cens.name))) {
##         ref <- c(paste(trans$from, trans$to), paste(unique(trans$from), cens.name))
##     }
##     else { ref <- paste(trans$from, trans$to) }
##     if (!(all(test %in% ref)==TRUE))
##         stop("There is undefined transitions in the data set")
##     if (sum(as.character(data$from)==as.character(data$to)) > 0)
##         stop("Transitions into the same state are not allowed")
##     if (!(all(ref %in% test) == TRUE))
##         warning("You may have specified more possible transitions than actually present in the data")
    
    ## need absorbing and transient states
    absorb <- setdiff(levels(trans$to), levels(trans$from))
    transient <- unique(state.names[!(state.names %in% absorb)])
    if (!all(cov.states %in% transient)) {
        warning("All transient states are not time-dependent covariates?")
    }

    ### data.frame transformation
##     data$from <- as.factor(data$from)
##     data$to <- as.factor(data$to)
##     if (!(is.null(cens.name))) {
##         data$from <- factor(data$from, levels = c(cens.name, state.names), ordered = TRUE)
##         levels(data$from) <- 0:length(state.names)
##         data$to <- factor(data$to, levels = c(cens.name, state.names), ordered = TRUE)
##         levels(data$to) <- 0:length(state.names)
##     }
##     else{
##         data$from <- factor(data$from, levels = state.names, ordered = TRUE)
##         levels(data$from) <- 1:length(state.names)
##         data$to <- factor(data$to, levels = state.names, ordered = TRUE)
##         levels(data$to) <- 1:length(state.names)
##     } # censoring is 0 now!!!
    
    ## if not, put like counting process data
    if ("time" %in% names(data)) {
        data <- data[order(data$id, data$time), ]
        idd <- as.integer(data$id)
        entree <- double(length(data$time))
        masque <- rbind(1, apply(as.matrix(idd), 2, diff))
        entree <- c(0, data$time[1:(length(data$time) - 1)]) * (masque == 0)
        data <- data.frame(id = data$id, from = data$from,
                           to = data$to, entry = entree, exit = data$time)
        if (sum(data$entry < data$exit) != nrow(data))
            stop("Exit time from a state must be > entry time")
    } else {
        data <- data[order(data$id, data$entry), ]
        if (sum(data$entry < data$exit) != nrow(data))
            stop("Exit time from a state must be > entry time")
    }
    
    ## Let's start the real work (without any loop, just for fun!)
    status <- as.numeric(data$to %in% absorb) * data$to
    status
   
}
    
    
