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
