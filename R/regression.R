regression <-
function(regressor=NULL, data=NULL, pre=function(x, y=NULL) { list(features=x) }, combine=NULL, expand=identity) {
    if(is.null(regressor)) {
        stop("No regressor given!")
    }
    if(is.null(data) || length(data$train) == 0 || length(data$test) == 0) {
        stop("Need data with train/test split!")
    }

    totalBests = breakBestTies(data)
    predictions = parallelMap(function(i) {
        trf = pre(subset(data$train[[i]], T, data$features))
        tsf = pre(subset(data$test[[i]], T, data$features), trf$meta)

        trainpredictions = matrix(nrow=nrow(trf$features), ncol=length(data$performance))
        performancePredictions = matrix(nrow=nrow(tsf$features), ncol=length(data$performance))
        for (j in 1:length(data$performance)) {
            model = regressor(data$train[[i]][[data$performance[j]]]~., data=trf$features)
            if(!is.null(combine)) {
                trainpredictions[,j] = predict(model, trf$features)
            }
            performancePredictions[,j] = predict(model, tsf$features)
        }

        if(!is.null(combine)) {
            trainBests = breakBestTies(data, i)
            combinedmodel = combine(trainBests~., data=data.frame(expand(trainpredictions)))
            preds = as.character(predict(combinedmodel, data.frame(expand(performancePredictions))))
            combinedpredictions = lapply(preds, function(l) { setNames(data.frame(table(l)), predNames) })
        } else {
            if(data$minimize) {
                decreasing = F
            } else {
                decreasing = T
            }
            combinedpredictions = apply(performancePredictions, 1, function(x) { setNames(data.frame(data$performance[sort.list(x, decreasing=decreasing)], sort(x, decreasing=decreasing)), predNames) })
        }
        return(list(combinedpredictions))
    }, 1:length(data$train), simplify=T)

    fs = pre(subset(data$data, T, data$features))
    models = lapply(1:length(data$performance), function(i) {
        # evaluate the predictions to put them into the environment -- this is only so that the tests don't break
        data$data[[data$performance[i]]]
        return(regressor(data$data[[data$performance[i]]]~., data=fs$features))
    })
    if(!is.null(combine)) {
        trainpredictions = matrix(nrow=nrow(fs$features), ncol=length(data$performance))
        for (i in 1:length(data$performance)) {
            trainpredictions[,i] = predict(models[[i]], fs$features)
        }
        combinedmodel = combine(totalBests~., data=data.frame(expand(trainpredictions)))
    }

    return(list(predictions=predictions, models=models, predictor=function(x) {
        tsf = pre(subset(x, T, data$features), fs$meta)
        performancePredictions = matrix(nrow=nrow(tsf$features), ncol=length(data$performance))
        for (i in 1:length(data$performance)) {
            performancePredictions[,i] = predict(models[[i]], tsf$features)
        }

        if(!is.null(combine)) {
            preds = as.character(predict(combinedmodel, data.frame(expand(performancePredictions))))
            combinedpredictions = lapply(preds, function(l) { setNames(data.frame(table(l)), predNames) })
        } else {
            if(data$minimize) {
                decreasing = F
            } else {
                decreasing = T
            }
            combinedpredictions = apply(performancePredictions, 1, function(x) { setNames(data.frame(data$performance[sort.list(x, decreasing=decreasing)], sort(x, decreasing=decreasing)), predNames) })
        }
        return(combinedpredictions)
    }))
}
