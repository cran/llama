predNames = c("algorithm", "score")
vbs <-
function(data=NULL) {
    if(is.null(data)) {
        stop("Need data to determine virtual best!")
    }
    return(lapply(data$data$best, function(l) { setNames(data.frame(as.table(sort(table(l), decreasing=T))), predNames) }))
}

singleBestByCount <-
function(data=NULL) {
    if(is.null(data)) {
        stop("Need data to determine single best!")
    }
    best = breakBestTies(data)
    preds = rep.int(names(sort(table(best), decreasing=T)[1]), length(data$data$best))
    return(lapply(preds, function(l) { setNames(data.frame(table(l)), predNames) }))
}

singleBestByPar <-
function(data=NULL, factor=10) {
    if(is.null(data)) {
        stop("Need data to determine single best!")
    }
    if(is.null(data$success)) {
        stop("Need successes to compute PAR scores.")
    }

    best = names(sort(sapply(data$performance, function(x) { sum(parscores(data, list(predictions=replicate(length(data$data$best), setNames(data.frame(table(x)), predNames), simplify=F)), factor=factor)) })))[1]
    preds = rep.int(best, length(data$data$best))
    return(lapply(preds, function(l) { setNames(data.frame(table(l)), predNames) }))
}

singleBestBySuccesses <-
function(data=NULL) {
    if(is.null(data)) {
        stop("Need data to determine single best!")
    }
    if(is.null(data$success)) {
        stop("Need successes to compute successes.")
    }

    best = names(sort(sapply(data$performance, function(x) { sum(successes(data, list(predictions=replicate(length(data$data$best), setNames(data.frame(table(x)), predNames), simplify=F)))) }), decreasing=T))[1]
    preds = rep.int(best, length(data$data$best))
    return(lapply(preds, function(l) { setNames(data.frame(table(l)), predNames) }))
}

singleBest <-
function(data=NULL) {
    if(is.null(data)) {
        stop("Need data to determine single best!")
    }

    best = names(sort(sapply(data$performance, function(x) { sum(subset(data$data, select=x)) }), decreasing=!data$minimize))[1]
    preds = rep.int(best, length(data$data$best))
    return(lapply(preds, function(l) { setNames(data.frame(table(l)), predNames) }))
}

breakBestTies <-
function(data=NULL, fold=NULL) {
    if(is.null(data)) {
        stop("Need data to break ties!")
    }

    if(is.null(fold)) {
        perfs = subset(data$data, select=data$performance)
    } else {
        perfs = subset(data$train[[fold]], select=data$performance)
    }
    order = names(sort(sapply(perfs, sum), decreasing=!data$minimize))
    optfun = if(data$minimize) { which.min } else { which.max }
    best = factor(apply(perfs[order], 1, function(x) { order[optfun(unlist(x))] }))
    names(best) = NULL
    return(best)
}