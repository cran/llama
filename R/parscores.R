parscores <-
function(data=NULL, model=NULL, factor=10, timeout=NULL) {
    if(is.null(data) || is.null(model)) {
        stop("Need both data and model to calculate PAR scores!")
    }
    if(is.null(data$success)) {
        stop("Need successes to calculate PAR scores!")
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

    if(is.null(timeout)) {
        # if timeout value wasn't given, assume maximum from data set
        timeout = max(subset(data$data, T, data$performance))
    }
    return(unlist(lapply(1:length(data$test), function(i) {
        sapply(1:nrow(data$test[[i]]), function(j) {
            perfs = subset(data$test[[i]][j,], T, data$performance)
            successes = subset(data$test[[i]][j,], T, data$success)
            if(is.null(data$cost)) {
                costs = 0
            } else {
                if(is.null(data$costGroups)) {
                    costs = sum(subset(data$test[[i]][j,], T, data$features))
                } else {
                    # figure out which feature groups are being used
                    usedGroups = subset(data$cost, sapply(data$cost, function(x) { length(intersect(data$costGroups[[x]], data$features)) > 0 }))
                    costs = sum(subset(data$test[[i]][j,], T, usedGroups))
                }
            }
            chosen = which(data$performance == predictions[[i]][[j]]$algorithm[1])
            score = as.numeric(perfs[chosen]) + costs
            if(!as.logical(successes[chosen]) || score > timeout) {
                score = timeout * factor
            }
            score
        })
    })))
}
