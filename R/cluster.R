cluster <-
function(clusterer=NULL, data=NULL, pre=function(x, y=NULL) { list(features=x) }) {
    if(is.null(clusterer)) {
        stop("No clusterer given!")
    }
    if(is.null(data) || length(data$train) == 0 || length(data$test) == 0) {
        stop("Need data with train/test split!")
    }
    if(!is.list(clusterer)) { clusterer = list(clusterer) }
    combinator = "majority"
    if(!is.null(clusterer$.combine)) {
        combinator = clusterer$.combine
        clusterer = clusterer[-which(names(clusterer) == ".combine")]
    }

    i = 1 # prevent warning when checking package
    predictions = foreach(i = 1:length(data$train), .combine=append) %dopar% {
        trf = pre(subset(data$train[[i]], T, data$features))
        tsf = pre(subset(data$test[[i]], T, data$features), trf$meta)

        trainpredictions = matrix(nrow=nrow(trf$features), ncol=length(clusterer))
        ensemblepredictions = list()
        for(j in 1:length(clusterer)) {
            model = clusterer[[j]](trf$features)
            trainclusters = predict(model)
            best = by(data$train[[i]], trainclusters, function(x) { setNames(data.frame(as.table(sort(table(x$best), decreasing=T))), predNames) })
            if(is.function(combinator)) { # only do this if we need it
                trainpredictions[,j] = sapply(predict(model, trf$features),
                function(x) {
                    as.character(if(is.na(x)) {
                        names(sort(table(data$train[[i]]$best), decreasing=T))[1]
                    } else {
                        best[[which(names(best)==x)]][1,1]
                    })
                })
            }
            preds = predict(model, tsf$features)
            ensemblepredictions[[j]] = list()
            for(k in 1:length(preds)) {
                x = preds[k]
                if(is.na(x)) {
                    ensemblepredictions[[j]][[k]] = setNames(data.frame(as.table(sort(table(data$train[[k]]$best), decreasing=T))), predNames)
                } else {
                    ensemblepredictions[[j]][[k]] = best[[which(names(best)==x)]]
                }
            }
        }
        if(is.function(combinator)) {
            combinedmodel = combinator(data$train[[i]]$best~., data=data.frame(trainpredictions))
            featureData = matrix(nrow=nrow(tsf$features), ncol=length(clusterer))
            for(k in 1:nrow(featureData)) {
                for(j in 1:ncol(featureData)) {
                    featureData[k,j] = as.character(ensemblepredictions[[j]][[k]][1,1])
                }
            }
            preds = as.character(predict(combinedmodel, data.frame(featureData)))
            combinedpredictions = lapply(preds, function(l) { setNames(data.frame(table(l)), predNames) })
        } else {
            combinedpredictions = Reduce(function(x, y) {
                lapply(1:length(x), function(k) {
                    aggregate(data=rbind(x[[k]], y[[k]]), as.formula(paste(predNames[2], predNames[1], sep="~")), sum)
                })
            }, ensemblepredictions)
        }
        return(list(combinedpredictions))
    }

    fs = pre(subset(data$data, T, data$features))
    models = lapply(1:length(clusterer), function(i) {
        model = clusterer[[i]](fs$features)
        clusters = predict(model)
        best = by(data$data, clusters, function(x) { setNames(data.frame(as.table(sort(table(x$best), decreasing=T))), predNames) })
        return(function(newdata) {
            preds = predict(model, newdata)
            lapply(1:length(preds), function(k) {
                x = preds[k]
                if(is.na(x)) {
                    setNames(data.frame(as.table(sort(table(data$train[[k]]$best), decreasing=T))), predNames)
                } else {
                    best[[which(names(best)==x)]]
                }
            })
        })
    })
    if(is.function(combinator)) {
        trainpredictions = matrix(nrow=nrow(fs$features), ncol=length(clusterer))
        for(j in 1:length(clusterer)) {
            trainpredictions[,j] = sapply(models[[j]](fs$features), function(x) { as.character(x[1,1]) })
        }
        combinedmodel = combinator(data$data$best~., data=data.frame(trainpredictions))
    }

    return(list(predictions=predictions, models=models, predictor=function(x) {
        tsf = pre(subset(x, T, data$features), fs$meta)
        ensemblepredictions = list()
        for(j in 1:length(clusterer)) {
            ensemblepredictions[[j]] = models[[j]](tsf$features)
        }
        if(is.function(combinator)) {
            featureData = matrix(nrow=nrow(tsf$features), ncol=length(clusterer))
            for(k in 1:nrow(featureData)) {
                for(j in 1:ncol(featureData)) {
                    featureData[k,j] = as.character(ensemblepredictions[[j]][[k]][1,1])
                }
            }
            preds = as.character(predict(combinedmodel, data.frame(featureData)))
            combinedpredictions = lapply(preds, function(l) { setNames(data.frame(table(l)), predNames) })
        } else {
            combinedpredictions = Reduce(function(x, y) {
                lapply(1:length(x), function(k) {
                    aggregate(data=rbind(x[[k]], y[[k]]), as.formula(paste(predNames[2], predNames[1], sep="~")), sum)
                })
            }, ensemblepredictions)
        }
        return(combinedpredictions)
    }))
}
