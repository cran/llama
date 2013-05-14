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
        ensemblepredictions = matrix(nrow=nrow(tsf$features), ncol=length(clusterer))
        for(j in 1:length(clusterer)) {
            model = clusterer[[j]](trf$features)
            trainclusters = predict(model)
            best = by(data$train[[i]], trainclusters, function(x) { names(sort(table(x$best), decreasing=T))[1] })
            if(is.function(combinator)) { # only do this if we need it
                trainpredictions[,j] = sapply(predict(model, trf$features),
                function(x) {
                    as.character(if(is.na(x)) {
                        names(sort(table(data$train[[i]]$best), decreasing=T))[1]
                    } else {
                        best[[which(names(best)==x)]]
                    })
                })
            }
            ensemblepredictions[,j] = sapply(predict(model, tsf$features),
                function(x) {
                    as.character(if(is.na(x)) {
                        names(sort(table(data$train[[i]]$best), decreasing=T))[1]
                    } else {
                        best[[which(names(best)==x)]]
                    })
                })
        }
        if(is.function(combinator)) {
            combinedmodel = combinator(data$train[[i]]$best~., data=data.frame(trainpredictions))
            combinedpredictions = as.character(predict(combinedmodel, data.frame(ensemblepredictions)))
        } else {
            combinedpredictions = apply(ensemblepredictions, 1, function(l) { names(sort(table(l), decreasing=T)[1]) })
        }
        return(list(combinedpredictions))
    }

    fs = pre(subset(data$data, T, data$features))
    models = lapply(1:length(clusterer), function(i) {
        model = clusterer[[i]](fs$features)
        clusters = predict(model)
        best = by(data$data, clusters, function(x) { names(sort(table(x$best), decreasing=T))[1] })
        return(function(newdata) { sapply(predict(model, newdata),
            function(x) {
                as.character(if(is.na(x)) {
                    names(sort(table(data$data$best), decreasing=T))[1]
                } else {
                    best[[which(names(best)==x)]]
                })
            })})
    })
    if(is.function(combinator)) {
        trainpredictions = matrix(nrow=nrow(fs$features), ncol=length(clusterer))
        for(j in 1:length(clusterer)) {
            trainpredictions[,j] = models[[j]](fs$features)
        }
        combinedmodel = combinator(data$data$best~., data=data.frame(trainpredictions))
    }

    return(list(predictions=predictions, models=models, predictor=function(x) {
        tsf = pre(subset(x, T, data$features), fs$meta)

        ensemblepredictions = matrix(nrow=nrow(tsf$features), ncol=length(clusterer))
        for(j in 1:length(clusterer)) {
            ensemblepredictions[,j] = models[[j]](tsf$features)
        }
        if(is.function(combinator)) {
            combinedpredictions = as.character(predict(combinedmodel, data.frame(ensemblepredictions)))
        } else {
            combinedpredictions = apply(ensemblepredictions, 1, function(l) { names(sort(table(l), decreasing=T)[1]) })
        }
        return(combinedpredictions)
    }))
}
