predNames = c("algorithm", "score")
vbs <-
function(data=NULL) {
    if(is.null(data)) {
        stop("Need data to determine virtual best!")
    }
    return(lapply(data$data$best, function(l) { setNames(data.frame(as.table(sort(table(l), decreasing=T))), predNames) }))
}

singleBest <-
function(data=NULL) {
    if(is.null(data)) {
        stop("Need data to determine single best!")
    }
    preds = rep.int(names(sort(table(data$data$best), decreasing=T)[1]), length(data$data$best))
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

    best = names(sort(sapply(data$performance, function(x) { sum(parscores(data, replicate(length(data$data$best), setNames(data.frame(table(x)), predNames), simplify=F), factor=factor)) })))[1]
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

    best = names(sort(sapply(data$performance, function(x) { sum(successes(data, replicate(length(data$data$best), setNames(data.frame(table(x)), predNames), simplify=F))) }), decreasing=T))[1]
    preds = rep.int(best, length(data$data$best))
    return(lapply(preds, function(l) { setNames(data.frame(table(l)), predNames) }))
}
