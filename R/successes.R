successes <-
function(data=NULL, predictions=NULL) {
    if(is.null(data$success)) {
        stop("Need successes to compute successes.")
    }
    post = identity
    if(length(data$test) == 0) {
        data$test = list(data$data)
        predictions = list(predictions)
        post = unlist
    }
    return(post(lapply(1:length(data$test), function(i) {
        sapply(1:nrow(data$test[[i]]), function(j) {
            successes = subset(data$test[[i]][j,], T, data$success)
            as.logical(successes[which(data$performance == predictions[[i]][j])])
            })
        })))
}
