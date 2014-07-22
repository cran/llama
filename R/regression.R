regression <-
function(regressor=NULL, data=NULL, pre=function(x, y=NULL) { list(features=x) }, combine=NULL, expand=identity) {
    if(is.null(regressor)) {
        stop("No regressor given!")
    }
    if(is.null(data) || length(data$train) == 0 || length(data$test) == 0) {
        stop("Need data with train/test split!")
    }

    totalBests = data.frame(target=factor(breakBestTies(data), levels=data$performance))
    predictions = parallelMap(function(i) {
        trf = pre(subset(data$train[[i]], T, data$features))
        tsf = pre(subset(data$test[[i]], T, data$features), trf$meta)
        trp = subset(data$train[[i]], T, data$performance)
        trw = abs(apply(trp, 1, max) - apply(trp, 1, min))

        trainpredictions = data.frame(row.names=1:nrow(trf$features))
        performancePredictions = data.frame(row.names=1:nrow(tsf$features))
        for (j in 1:length(data$performance)) {
            task = makeRegrTask(id="regression", target="target", data=cbind(data.frame(target=data$train[[i]][[data$performance[j]]]), trf$features))
            model = train(regressor, task = task)
            if(!is.null(combine)) {
                trainpredictions[,j] = predict(model, newdata=trf$features)$data$response
            }
            performancePredictions[,j] = predict(model, newdata=tsf$features)$data$response
        }

        if(!is.null(combine)) {
            trainBests = data.frame(target=factor(breakBestTies(data, i), levels=data$performance))
            if(hasProperties(combine, "weights")) {
                task = makeClassifTask(id="regression", target="target", weights=trw, data=cbind(trainBests, trf$features, data.frame(expand(trainpredictions))))
            } else {
                task = makeClassifTask(id="regression", target="target", data=cbind(trainBests, trf$features, data.frame(expand(trainpredictions))))
            }
            combinedmodel = train(combine, task = task)
            preds = predict(combinedmodel, newdata=cbind(tsf$features, data.frame(expand(performancePredictions))))$data$response
            combinedpredictions = lapply(preds, function(l) { setNames(data.frame(as.table(sort(table(l), decreasing=T))), predNames) })
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
    fp = subset(data$data, T, data$performance)
    fw = abs(apply(fp, 1, max) - apply(fp, 1, min))
    models = lapply(1:length(data$performance), function(i) {
        task = makeRegrTask(id="regression", target="target", data=cbind(data.frame(target=data$data[[data$performance[i]]]), fs$features))
        return(train(regressor, task = task))
    })
    if(!is.null(combine)) {
        trainpredictions = data.frame(row.names=1:nrow(fs$features))
        for (i in 1:length(data$performance)) {
            trainpredictions[,i] = predict(models[[i]], newdata=fs$features)$data$response
        }
        if(hasProperties(combine, "weights")) {
            task = makeClassifTask(id="regression", target="target", weights=fw, data=cbind(totalBests, fs$features, data.frame(expand(trainpredictions))))
        } else {
            task = makeClassifTask(id="regression", target="target", data=cbind(totalBests, fs$features, data.frame(expand(trainpredictions))))
        }
        combinedmodel = train(combine, task = task)
    }

    return(list(predictions=predictions, models=models, predictor=function(x) {
        tsf = pre(subset(x, T, data$features), fs$meta)
        performancePredictions = data.frame(row.names=1:nrow(tsf$features))
        for (i in 1:length(data$performance)) {
            performancePredictions[,i] = predict(models[[i]], newdata=tsf$features)$data$response
        }

        if(!is.null(combine)) {
            preds = predict(combinedmodel, newdata=cbind(tsf$features, data.frame(expand(performancePredictions))))$data$response
            combinedpredictions = lapply(preds, function(l) { setNames(data.frame(as.table(sort(table(l), decreasing=T))), predNames) })
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
