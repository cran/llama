classify <-
function(classifier=NULL, data=NULL, pre=function(x, y=NULL) { list(features=x) }) {
    if(is.null(classifier)) {
        stop("No classifier given!")
    }
    if(is.null(data) || length(data$train) == 0 || length(data$test) == 0) {
        stop("Need data with train/test split!")
    }
    if(!is.list(classifier)) { classifier = list(classifier) }
    combinator = "majority"
    if(!is.null(classifier$.combine)) {
        combinator = classifier$.combine
        classifier = classifier[-which(names(classifier) == ".combine")]
    }

    predictions = foreach(i = 1:length(data$train), .combine=append) %dopar% {
        trf = pre(subset(data$train[[i]], T, data$features))
        tsf = pre(subset(data$test[[i]], T, data$features), trf$meta)

        trainpredictions = matrix(nrow=nrow(trf$features), ncol=length(classifier))
        ensemblepredictions = matrix(nrow=nrow(tsf$features), ncol=length(classifier))
        for(j in 1:length(classifier)) {
            model = classifier[[j]](data$train[[i]]$best~., data=trf$features)
            if(is.function(combinator)) { # only do this if we need it
                trainpredictions[,j] = as.character(predict(model, trf$features))
            }
            ensemblepredictions[,j] = as.character(predict(model, tsf$features))
        }
        if(is.function(combinator)) {
            combinedmodel = combinator(data$train[[i]]$best~., data=data.frame(trainpredictions))
            preds = as.character(predict(combinedmodel, data.frame(ensemblepredictions)))
            combinedpredictions = lapply(preds, function(l) { setNames(data.frame(table(l)), predNames) })
        } else {
            combinedpredictions = apply(ensemblepredictions, 1, function(l) { setNames(data.frame(as.table(sort(table(l), decreasing=T))), predNames) })
        }
        return(list(combinedpredictions))
    }

    fs = pre(subset(data$data, T, data$features))
    models = lapply(1:length(classifier), function(i) {
        return(classifier[[i]](data$data$best~., data=fs$features))
    })
    if(is.function(combinator)) {
        trainpredictions = matrix(nrow=nrow(fs$features), ncol=length(classifier))
        for(i in 1:length(classifier)) {
            trainpredictions[,i] = as.character(predict(models[[i]], fs$features))
        }
        combinedmodel = combinator(data$data$best~., data=data.frame(trainpredictions))
    }

    return(list(predictions=predictions, models=models, predictor=function(x) {
        tsf = pre(subset(x, T, data$features), fs$meta)
        ensemblepredictions = matrix(nrow=nrow(tsf$features), ncol=length(classifier))
        for(i in 1:length(classifier)) {
            ensemblepredictions[,i] = as.character(predict(models[[i]], tsf$features))
        }
        if(is.function(combinator)) {
            preds = as.character(predict(combinedmodel, data.frame(ensemblepredictions)))
            combinedpredictions = lapply(preds, function(l) { setNames(data.frame(table(l)), predNames) })
        } else {
            combinedpredictions = apply(ensemblepredictions, 1, function(l) { setNames(data.frame(as.table(sort(table(l), decreasing=T))), predNames) })
        }
        return(combinedpredictions)
    }))
}
