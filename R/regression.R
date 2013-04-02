regression <-
function(regressor=NULL, data=NULL, pre=function(x, y=NULL) { list(features=x) }, combine=min, stack=F, expand=identity) {
    if(is.null(regressor)) {
        stop("No regressor given!")
    }
    if(is.null(data) || length(data$train) == 0 || length(data$test) == 0) {
        stop("Need data with train/test split!")
    }

    predictions = foreach(i = 1:length(data$train), .combine=append) %dopar% {
        trf = pre(subset(data$train[[i]], T, data$features))
        tsf = pre(subset(data$test[[i]], T, data$features), trf$meta)

        trainpredictions = matrix(nrow=nrow(trf$features), ncol=length(data$performance))
        performancePredictions = matrix(nrow=nrow(tsf$features), ncol=length(data$performance))
        for (j in 1:length(data$performance)) {
            model = regressor(data$train[[i]][[data$performance[j]]]~., data=trf$features)
            if(stack) {
                trainpredictions[,j] = predict(model, trf$features)
            }
            performancePredictions[,j] = predict(model, tsf$features)
        }

        if(stack) {
            combinedmodel = combine(data$train[[i]]$best~., data=data.frame(expand(trainpredictions)))
            combinedpredictions = as.character(predict(combinedmodel, data.frame(expand(performancePredictions))))
        } else {
            combinedpredictions = apply(performancePredictions, 1, function(x) { data$performance[head(which(x %in% combine(x)), 1)] })
        }
        return(list(combinedpredictions))
    }

    fs = pre(subset(data$data, T, data$features))
    models = lapply(1:length(data$performance), function(i) {
        return(regressor(data$data[[data$performance[i]]]~., data=fs$features))
    })
    if(stack) {
        trainpredictions = matrix(nrow=nrow(fs$features), ncol=length(data$performance))
        for (i in 1:length(data$performance)) {
            trainpredictions[,i] = predict(models[[i]], fs$features)
        }
        combinedmodel = combine(data$data$best~., data=data.frame(expand(trainpredictions)))
    }

    return(list(predictions=predictions, predictor=function(x) {
        tsf = pre(subset(x, T, data$features), fs$meta)

        performancePredictions = matrix(nrow=nrow(tsf$features), ncol=length(data$performance))
        for (i in 1:length(data$performance)) {
            performancePredictions[,i] = predict(models[[i]], tsf$features)
        }

        if(stack) {
            combinedpredictions = as.character(predict(combinedmodel, data.frame(expand(performancePredictions))))
        } else {
            combinedpredictions = apply(performancePredictions, 1, function(x) { data$performance[head(which(x %in% combine(x)), 1)] })
        }
        return(combinedpredictions)
    }))
}
