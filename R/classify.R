classify <-
function(classifier=NULL, data=NULL, pre=function(x, y=NULL) { list(features=x) }) {
    if(is.null(classifier)) {
        stop("No classifier given!")
    }
    if(is.null(data) || length(data$train) == 0 || length(data$test) == 0) {
        stop("Need data with train/test split!")
    }
    if(is.object(classifier)) { classifier = list(classifier) }
    combinator = "majority"
    if(!is.null(classifier$.combine)) {
        combinator = classifier$.combine
        classifier = classifier[-which(names(classifier) == ".combine")]
    }

    totalBests = data.frame(target=factor(breakBestTies(data), levels=data$performance))

    predictions = parallelMap(function(i) {
        trf = pre(subset(data$train[[i]], T, data$features))
        tsf = pre(subset(data$test[[i]], T, data$features), trf$meta)
        trp = subset(data$train[[i]], T, data$performance)
        trw = abs(apply(trp, 1, max) - apply(trp, 1, min))

        trainpredictions = data.frame(row.names=1:nrow(trf$features))
        ensemblepredictions = data.frame(row.names=1:nrow(tsf$features))

        trainBests = data.frame(target=factor(breakBestTies(data, i), levels=data$performance))
        for(j in 1:length(classifier)) {
            if(hasProperties(classifier[[j]], "weights")) {
                task = makeClassifTask(id="classify", target="target", weights = trw, data=cbind(trainBests, trf$features))
            } else {
                task = makeClassifTask(id="classify", target="target", data=cbind(trainBests, trf$features))
            }
            model = train(classifier[[j]], task = task)
            if(inherits(combinator, "Learner")) { # only do this if we need it
                trainpredictions[,j] = predict(model, newdata=trf$features)$data$response
            }
            ensemblepredictions[,j] = predict(model, newdata=tsf$features)$data$response
        }
        if(inherits(combinator, "Learner")) {
            if(hasProperties(combinator, "weights")) {
                task = makeClassifTask(id="classify", target="target", weights = trw, data=cbind(trainBests, trf$features, lapply(trainpredictions, factor, levels=data$performance)))
            } else {
                task = makeClassifTask(id="classify", target="target", data=cbind(trainBests, trf$features, lapply(trainpredictions, factor, levels=data$performance)))
            }
            combinedmodel = train(combinator, task = task)
            preds = predict(combinedmodel, newdata=cbind(tsf$features, lapply(ensemblepredictions, factor, levels=data$performance)))$data$response
            combinedpredictions = lapply(preds, function(l) { setNames(data.frame(as.table(sort(table(l), decreasing=T))), predNames) })
        } else {
            combinedpredictions = apply(ensemblepredictions, 1, function(l) { setNames(data.frame(as.table(sort(table(l), decreasing=T))), predNames) })
        }
        return(list(combinedpredictions))
    }, 1:length(data$train), simplify=T)

    fs = pre(subset(data$data, T, data$features))
    fp = subset(data$data, T, data$performance)
    fw = abs(apply(fp, 1, max) - apply(fp, 1, min))
    models = lapply(1:length(classifier), function(i) {
        if(hasProperties(classifier[[i]], "weights")) {
            task = task=makeClassifTask(id="classify", target="target", weights = fw, data=cbind(totalBests, fs$features))
        } else {
            task = task=makeClassifTask(id="classify", target="target", data=cbind(totalBests, fs$features))
        }
        return(train(classifier[[i]], task = task))
    })
    if(inherits(combinator, "Learner")) {
        trainpredictions = data.frame(row.names=1:nrow(fs$features))
        for(i in 1:length(classifier)) {
            trainpredictions[,i] = predict(models[[i]], newdata=fs$features)$data$response
        }
        if(hasProperties(combinator, "weights")) {
            task = makeClassifTask(id="classify", target="target", weights = fw, data=cbind(totalBests, fs$features, lapply(trainpredictions, factor, levels=data$performance)))
        } else {
            task = makeClassifTask(id="classify", target="target", data=cbind(totalBests, fs$features, lapply(trainpredictions, factor, levels=data$performance)))
        }
        combinedmodel = train(combinator, task = task)
    }

    return(list(predictions=predictions, models=models, predictor=function(x) {
        tsf = pre(subset(x, T, data$features), fs$meta)
        ensemblepredictions = data.frame(row.names=1:nrow(tsf$features))
        for(i in 1:length(classifier)) {
            ensemblepredictions[,i] = predict(models[[i]], newdata=tsf$features)$data$response
        }
        if(inherits(combinator, "Learner")) {
            preds = predict(combinedmodel, newdata=cbind(tsf$features, lapply(ensemblepredictions, factor, levels=data$performance)))$data$response
            combinedpredictions = lapply(preds, function(l) { setNames(data.frame(as.table(sort(table(l), decreasing=T))), predNames) })
        } else {
            combinedpredictions = apply(ensemblepredictions, 1, function(l) { setNames(data.frame(as.table(sort(table(l), decreasing=T))), predNames) })
        }
        return(combinedpredictions)
    }))
}
