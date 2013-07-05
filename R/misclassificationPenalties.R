misclassificationPenalties <-
function(data=NULL, predictions=NULL, minimize=T) {
    optfun = if(minimize) { min } else { max }
    post = identity
    if(length(data$test) == 0) {
        data$test = list(data$data)
        predictions = list(predictions)
        post = unlist
    }
    return(post(lapply(1:length(data$test), function(i) {
        sapply(1:nrow(data$test[[i]]), function(j) {
            perfs = subset(data$test[[i]][j,], T, data$performance)
            as.numeric(abs(perfs[which(data$performance == predictions[[i]][[j]]$algorithm[1])] - optfun(perfs)))
            })
        })))
}
