regressionPairs <-
function(regressor=NULL, data=NULL, pre=function(x, y=NULL) { list(features=x) }, combine=NULL) {
    if(is.null(regressor)) {
        stop("No regressor given!")
    }
    if(is.null(data) || length(data$train) == 0 || length(data$test) == 0) {
        stop("Need data with train/test split!")
    }

    totalBests = data.frame(target=factor(breakBestTies(data), levels=data$performance))
    combns = combn(data$performance, 2)
    predictions = parallelMap(function(i) {
        trf = pre(subset(data$train[[i]], T, data$features))
        tsf = pre(subset(data$test[[i]], T, data$features), trf$meta)
        trp = subset(data$train[[i]], T, data$performance)
        trw = abs(apply(trp, 1, max) - apply(trp, 1, min))

        trainpredictions = matrix(nrow=nrow(trf$features), ncol=ncol(combns))
        pairpredictions = matrix(nrow=nrow(tsf$features), ncol=ncol(combns))
        for (j in 1:ncol(combns)) {
            values = data.frame(target=data$train[[i]][[combns[1,j]]] - data$train[[i]][[combns[2,j]]])
            task = makeRegrTask(id="regression", target="target", data=cbind(values, trf$features), fixup.data="quiet")
            model = train(regressor, task = task)
            if(!is.null(combine)) { # only do this if we need it
                trainpredictions[,j] = predict(model, newdata=trf$features)$data$response
            }
            pairpredictions[,j] = predict(model, newdata=tsf$features)$data$response
        }

        if(!is.null(combine)) {
            trainBests = data.frame(target=factor(breakBestTies(data, i), levels=data$performance))
            if(hasProperties(combine, "weights")) {
                task = makeClassifTask(id="regression", target="target", weights=trw, data=cbind(trainBests, trf$features, data.frame(trainpredictions)), fixup.data="quiet")
            } else {
                task = makeClassifTask(id="regression", target="target", data=cbind(trainBests, trf$features, data.frame(trainpredictions)), fixup.data="quiet")
            }
            combinedmodel = train(combine, task = task)
            preds = predict(combinedmodel, newdata=cbind(tsf$features, data.frame(pairpredictions)))$data$response
            combinedpredictions = lapply(preds, function(l) { setNames(data.frame(table(l)), predNames) })
        } else {
            if(data$minimize) {
                sign = function(d) { return(-d); }
            } else {
                sign = function(d) { return(d); }
            }
            combinedpredictions = apply(pairpredictions, 1, function(row) {
                performanceSums = sapply(data$performance, function(p) {
                    sum(sapply(1:ncol(combns), function(k) {
                        if(p == combns[1,k]) {
                            sign(row[k])
                        } else if(p == combns[2,k]) {
                            sign(-row[k])
                        } else {
                            0
                        }
                    }))
                })
                setNames(data.frame(data$performance[sort.list(performanceSums, decreasing=T)], sort(performanceSums, decreasing=T), row.names=NULL), predNames)
            })
        }
        return(list(combinedpredictions))
    }, 1:length(data$train), simplify=T)

    fs = pre(subset(data$data, T, data$features))
    fp = subset(data$data, T, data$performance)
    fw = abs(apply(fp, 1, max) - apply(fp, 1, min))
    models = lapply(1:ncol(combns), function(i) {
        values = data.frame(target=data$data[[combns[1,i]]] - data$data[[combns[2,i]]])
        task = makeRegrTask(id="regression", target="target", data=cbind(values, fs$features), fixup.data="quiet")
        return(train(regressor, task = task))
    })
    if(!is.null(combine)) {
        trainpredictions = matrix(nrow=nrow(fs$features), ncol=ncol(combns))
        for(i in 1:ncol(combns)) {
            trainpredictions[,i] = predict(models[[i]], newdata=fs$features)$data$response
        }
        if(hasProperties(combine, "weights")) {
            task = makeClassifTask(id="regression", target="target", weights=fw, data=cbind(totalBests, fs$features, data.frame(trainpredictions)), fixup.data="quiet")
        } else {
            task = makeClassifTask(id="regression", target="target", data=cbind(totalBests, fs$features, data.frame(trainpredictions)), fixup.data="quiet")
        }
        combinedmodel = train(combine, task = task)
    }

    return(list(predictions=predictions, models=models, predictor=function(x) {
        tsf = pre(subset(x, T, data$features), fs$meta)
        pairpredictions = matrix(nrow=nrow(tsf$features), ncol=ncol(combns))
        for(i in 1:ncol(combns)) {
            pairpredictions[,i] = predict(models[[i]], newdata=tsf$features)$data$response
        }
        if(!is.null(combine)) {
            preds = predict(combinedmodel, newdata=cbind(tsf$features, data.frame(pairpredictions)))$data$response
            combinedpredictions = lapply(preds, function(l) { setNames(data.frame(table(l)), predNames) })
        } else {
            if(data$minimize) {
                sign = function(d) { return(-d); }
            } else {
                sign = function(d) { return(d); }
            }
            combinedpredictions = apply(pairpredictions, 1, function(row) {
                performanceSums = sapply(data$performance, function(p) {
                    sum(sapply(1:ncol(combns), function(k) {
                        if(p == combns[1,k]) {
                            sign(row[k])
                        } else if(p == combns[2,k]) {
                            sign(-row[k])
                        } else {
                            0
                        }
                    }))
                })
                setNames(data.frame(data$performance[sort.list(performanceSums, decreasing=T)], sort(performanceSums, decreasing=T), row.names=NULL), predNames)
            })
        }
        return(combinedpredictions)
    }))
}
