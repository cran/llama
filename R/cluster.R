cluster <-
function(clusterer=NULL, data=NULL, bestBy="performance", pre=function(x, y=NULL) { list(features=x) }) {
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

    if(bestBy == "performance") {
        innerbest = function(ss) { sort(sapply(data$performance, function(x) { sum(ss[x]) }), decreasing=!data$minimize) }
    } else if(bestBy == "count") {
        innerbest = function(ss) { sort(table(unlist(ss$best)), decreasing=!data$minimize) }
    } else if(bestBy == "successes") {
        if(is.null(data$success)) {
            stop("Need successes to determine best by successes!")
        }
        innerbest = function(ss) { setNames(sort(sapply(data$success, function(x) { colSums(ss[x])[1] }), decreasing=T), data$performance) }
    } else {
        stop(paste("Unknown bestBy: ", bestBy, sep=""))
    }

    bestfun = function(ss) { setNames(data.frame(as.table(innerbest(ss))), predNames) }

    i = 1 # prevent warning when checking package
    predictions = parallelMap(function(i) {
        trf = pre(subset(data$train[[i]], T, data$features))
        tsf = pre(subset(data$test[[i]], T, data$features), trf$meta)

        trainpredictions = matrix(nrow=nrow(trf$features), ncol=length(clusterer))
        ensemblepredictions = list()
        for(j in 1:length(clusterer)) {
            model = clusterer[[j]](trf$features)
            trainclusters = predict(model)
            best = by(data$train[[i]], trainclusters, bestfun)
            if(is.function(combinator)) { # only do this if we need it
                trainpredictions[,j] = sapply(predict(model, trf$features),
                function(x) {
                    as.character(if(is.na(x)) {
                        innerbest(data$train[[i]]);
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
                    ensemblepredictions[[j]][[k]] = bestfun(data$train[[k]])
                } else {
                    ensemblepredictions[[j]][[k]] = best[[which(names(best)==x)]]
                }
            }
        }
        if(is.function(combinator)) {
            trainBests = breakBestTies(data, i)
            combinedmodel = combinator(trainBests~., data=data.frame(trainpredictions))
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
    }, 1:length(data$train), simplify=T)

    fs = pre(subset(data$data, T, data$features))
    models = lapply(1:length(clusterer), function(i) {
        model = clusterer[[i]](fs$features)
        clusters = predict(model)
        best = by(data$data, clusters, bestfun)
        return(function(newdata) {
            preds = predict(model, newdata)
            lapply(1:length(preds), function(k) {
                x = preds[k]
                if(is.na(x)) {
                    bestfun(data$train[[k]])
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
        totalBests = breakBestTies(data)
        combinedmodel = combinator(totalBests~., data=data.frame(trainpredictions))
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
