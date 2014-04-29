misclassificationPenalties <-
function(data=NULL, model=NULL) {
    if(is.null(data) || is.null(model)) {
        stop("Need both data and model to calculate misclassification penalties!")
    }

    if(is.function(model)) {
        # vbs or a model predictor
        if(length(data$test) > 0) {
            # we've passed in splits
            stop("The model you've passed in only works on the full data.")
        }
        predictions = model(data)
    } else {
        # it's a model
        predictions = model$predictions
    }

    if(length(data$test) == 0) {
        data$test = list(data$data)
        predictions = list(predictions)
    }

    optfun = if(data$minimize) { min } else { max }

    return(unlist(lapply(1:length(data$test), function(i) {
        sapply(1:nrow(data$test[[i]]), function(j) {
            perfs = subset(data$test[[i]][j,], T, data$performance)
            as.numeric(abs(perfs[which(data$performance == predictions[[i]][[j]]$algorithm[1])] - optfun(perfs)))
            })
        })))
}
