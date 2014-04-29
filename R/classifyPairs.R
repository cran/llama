classifyPairs <-
function(classifier=NULL, data=NULL, pre=function(x, y=NULL) { list(features=x) }, combinator=NULL) {
    if(is.null(classifier)) {
        stop("No classifier given!")
    }
    if(is.null(data) || length(data$train) == 0 || length(data$test) == 0) {
        stop("Need data with train/test split!")
    }

    totalBests = breakBestTies(data)
    combns = combn(data$performance, 2)
    predictions = parallelMap(function(i) {
        trf = pre(subset(data$train[[i]], T, data$features))
        tsf = pre(subset(data$test[[i]], T, data$features), trf$meta)

        trainpredictions = matrix(nrow=nrow(trf$features), ncol=ncol(combns))
        pairpredictions = matrix(nrow=nrow(tsf$features), ncol=ncol(combns))
        for (j in 1:ncol(combns)) {
            if(data$minimize) {
                cmp = function(x, y) {
                    sapply(data$train[[i]][[x]] < data$train[[i]][[y]], function(z) { if(z) { x } else { y } })
                }
            } else {
                cmp = function(x, y) {
                    sapply(data$train[[i]][[x]] > data$train[[i]][[y]], function(z) { if(z) { x } else { y } })
                }
            }
            labels = factor(cmp(combns[1,j], combns[2,j]))
            model = classifier(labels~., data=trf$features)
            if(is.function(combinator)) { # only do this if we need it
                trainpredictions[,j] = as.character(predict(model, trf$features))
            }
            pairpredictions[,j] = as.character(predict(model, tsf$features))
        }

        if(is.function(combinator)) {
            trainBests = breakBestTies(data, i)
            combinedmodel = combinator(trainBests~., data=data.frame(trainpredictions))
            preds = as.character(predict(combinedmodel, data.frame(pairpredictions)))
            combinedpredictions = lapply(preds, function(l) { setNames(data.frame(table(l)), predNames) })
        } else {
            combinedpredictions = apply(pairpredictions, 1, function(l) { setNames(data.frame(as.table(sort(table(l), decreasing=T))), predNames) })
        }
        return(list(combinedpredictions))
    }, 1:length(data$train), simplify=T)

    fs = pre(subset(data$data, T, data$features))
    models = lapply(1:ncol(combns), function(i) {
        if(data$minimize) {
            cmp = function(x, y) {
                sapply(data$data[[x]] < data$data[[y]], function(z) { if(z) { x } else { y } })
            }
        } else {
            cmp = function(x, y) {
                sapply(data$data[[x]] > data$data[[y]], function(z) { if(z) { x } else { y } })
            }
        }
        labels = factor(cmp(combns[1,i], combns[2,i]))
        return(classifier(labels~., data=fs$features))
    })
    if(is.function(combinator)) {
        trainpredictions = matrix(nrow=nrow(fs$features), ncol=ncol(combns))
        for(i in 1:ncol(combns)) {
            trainpredictions[,i] = as.character(predict(models[[i]], fs$features))
        }
        combinedmodel = combinator(totalBests~., data=data.frame(trainpredictions))
    }

    return(list(predictions=predictions, models=models, predictor=function(x) {
        tsf = pre(subset(x, T, data$features), fs$meta)
        pairpredictions = matrix(nrow=nrow(tsf$features), ncol=ncol(combns))
        for(i in 1:ncol(combns)) {
            pairpredictions[,i] = as.character(predict(models[[i]], tsf$features))
        }
        if(is.function(combinator)) {
            preds = as.character(predict(combinedmodel, data.frame(pairpredictions)))
            combinedpredictions = lapply(preds, function(l) { setNames(data.frame(table(l)), predNames) })
        } else {
            combinedpredictions = apply(pairpredictions, 1, function(l) { setNames(data.frame(as.table(sort(table(l), decreasing=T))), predNames) })
        }
        return(combinedpredictions)
    }))
}
