parscores <-
function(data=NULL, predictions=NULL, factor=10) {
    if(is.null(data$success)) {
        stop("Need successes to compute PAR scores.")
    }
    post = identity
    if(length(data$test) == 0) {
        data$test = list(data$data)
        predictions = list(predictions)
        post = unlist
    }
    return(post(lapply(1:length(data$test), function(i) {
        sapply(1:nrow(data$test[[i]]), function(j) {
            perfs = subset(data$test[[i]][j,], T, data$performance)
            successes = subset(data$test[[i]][j,], T, data$success)
            score = as.numeric(perfs[which(data$performance == predictions[[i]][j])])
            if(!as.logical(successes[which(data$performance == predictions[[i]][j])])) {
                score = score * factor
            }
            score
            })
        })))
}
