classifyPairs <-
function(classifier=NULL, data=NULL, pre=function(x, y=NULL) { list(features=x) }, combinator=NULL) {
    if(is.null(classifier)) {
        stop("No classifier given!")
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

        trainpredictions = data.frame(row.names=1:nrow(trf$features))
        pairpredictions = data.frame(row.names=1:nrow(tsf$features))
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
            labels = data.frame(target=factor(cmp(combns[1,j], combns[2,j]), levels=data$performance))
            if(hasProperties(classifier, "weights")) {
                task = makeClassifTask(id="classifyPairs", target="target", weights=abs(data$train[[i]][[combns[1,j]]] - data$train[[i]][[combns[2,j]]]), data=cbind(labels, trf$features), fixup.data="quiet")
            } else {
                task = makeClassifTask(id="classifyPairs", target="target", data=cbind(labels, trf$features), fixup.data="quiet")
            }
            model = train(classifier, task = task)
            if(!is.null(combinator)) { # only do this if we need it
                trainpredictions[,j] = predict(model, newdata=trf$features)$data$response
            }
            pairpredictions[,j] = predict(model, newdata=tsf$features)$data$response
        }

        if(!is.null(combinator)) {
            trainBests = data.frame(target=factor(breakBestTies(data, i), levels=data$performance))
            if(hasProperties(combinator, "weights")) {
                task = makeClassifTask(id="classifyPairs", target="target", weights=trw, data=cbind(trainBests, trf$features, lapply(data.frame(trainpredictions), factor, levels=data$performance)), fixup.data="quiet")
            } else {
                task = makeClassifTask(id="classifyPairs", target="target", data=cbind(trainBests, trf$features, lapply(data.frame(trainpredictions), factor, levels=data$performance)), fixup.data="quiet")
            }
            combinedmodel = train(combinator, task = task)
            preds = predict(combinedmodel, newdata=cbind(tsf$features, lapply(data.frame(pairpredictions), factor, levels=data$performance)))$data$response
            combinedpredictions = lapply(preds, function(l) { setNames(data.frame(as.table(sort(table(l), decreasing=T))), predNames) })
        } else {
            combinedpredictions = apply(pairpredictions, 1, function(l) { setNames(data.frame(as.table(sort(table(l), decreasing=T))), predNames) })
        }
        return(list(combinedpredictions))
    }, 1:length(data$train), simplify=T)

    fs = pre(subset(data$data, T, data$features))
    fp = subset(data$data, T, data$performance)
    fw = abs(apply(fp, 1, max) - apply(fp, 1, min))
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
        labels = data.frame(target=factor(cmp(combns[1,i], combns[2,i]), levels=data$performance))
        if(hasProperties(classifier, "weights")) {
            task = makeClassifTask(id="classifyPairs", target="target", weights=abs(data$data[[combns[1,i]]] - data$data[[combns[2,i]]]), data=cbind(labels, fs$features), fixup.data="quiet")
        } else {
            task = makeClassifTask(id="classifyPairs", target="target", data=cbind(labels, fs$features), fixup.data="quiet")
        }
        return(train(classifier, task = task))
    })
    if(!is.null(combinator)) {
        trainpredictions = data.frame(row.names=1:nrow(fs$features))
        for(i in 1:ncol(combns)) {
            trainpredictions[,i] = predict(models[[i]], newdata=fs$features)$data$response
        }
        if(hasProperties(combinator, "weights")) {
            task = makeClassifTask(id="classifyPairs", target="target", weights=fw, data=cbind(totalBests, fs$features, lapply(data.frame(trainpredictions), factor, levels=data$performance)), fixup.data="quiet")
        } else {
            task = makeClassifTask(id="classifyPairs", target="target", data=cbind(totalBests, fs$features, lapply(data.frame(trainpredictions), factor, levels=data$performance)), fixup.data="quiet")
        }
        combinedmodel = train(combinator, task = task)
    }

    return(list(predictions=predictions, models=models, predictor=function(x) {
        tsf = pre(subset(x, T, data$features), fs$meta)
        pairpredictions = data.frame(row.names=1:nrow(tsf$features))
        for(i in 1:ncol(combns)) {
            pairpredictions[,i] = predict(models[[i]], newdata=tsf$features)$data$response
        }
        if(!is.null(combinator)) {
            preds = predict(combinedmodel, newdata=cbind(tsf$features, lapply(data.frame(pairpredictions), factor, levels=data$performance)))$data$response
            combinedpredictions = lapply(preds, function(l) { setNames(data.frame(as.table(sort(table(l), decreasing=T))), predNames) })
        } else {
            combinedpredictions = apply(pairpredictions, 1, function(l) { setNames(data.frame(as.table(sort(table(l), decreasing=T))), predNames) })
        }
        return(combinedpredictions)
    }))
}
