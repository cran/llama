parscores <-
function(data=NULL, model=NULL, factor=10, timeout=NULL, addCosts=TRUE) {
    if(is.null(data) || is.null(model)) {
        stop("Need both data and model to calculate PAR scores!")
    }
    if(is.null(data$success)) {
        stop("Need successes to calculate PAR scores!")
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

    if(is.null(timeout)) {
        # if timeout value wasn't given, assume maximum from data set
        timeout = max(subset(data$data, T, data$performance))
    }
    return(unlist(lapply(1:length(data$test), function(i) {
        sapply(1:nrow(data$test[[i]]), function(j) {
            perfs = subset(data$test[[i]][j,], T, data$performance)
            successes = subset(data$test[[i]][j,], T, data$success)
            if(!addCosts || is.null(data$cost)) {
                costs = 0
            } else {
                if(is.null(data$costGroups)) {
                    # take only costs for features used in the model
                    costs = sum(subset(data$test[[i]][j,], T, intersect(data$cost, sapply(data$features, function(x) { paste(x, "cost", sep="_") }))))
                } else {
                    # figure out which feature groups are being used
                    usedGroups = subset(data$cost, sapply(data$cost, function(x) { length(intersect(data$costGroups[[x]], data$features)) > 0 }))
                    costs = sum(subset(data$test[[i]][j,], T, usedGroups))
                }
            }
            chosen = which(data$performance == predictions[[i]][[j]]$algorithm[1])
            if(length(chosen) == 0) {
                score = NA
            } else {
                score = as.numeric(perfs[chosen]) + costs
                if(!as.logical(successes[chosen]) || score > timeout) {
                    score = timeout * factor
                }
            }
            score
        })
    })))
}
