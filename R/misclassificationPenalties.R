misclassificationPenalties <-
function(data=NULL, model=NULL) {
    if(is.null(data) || is.null(model)) {
        stop("Need both data and model to calculate misclassification penalties!")
    }

    if(is.function(model)) {
        # vbs or a model predictor
        if(length(data$test) > 0) {
            predictions = lapply(data$test, function(x) {
                data$data = x
                model(data)
            })
        } else {
            predictions = model(data)
        }
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
                chosen = which(data$performance == predictions[[i]][[j]]$algorithm[1])
                if(length(chosen) == 0) {
                    NA
                } else {
                    as.numeric(abs(perfs[chosen] - optfun(perfs)))
                }
            })
        })))
}
