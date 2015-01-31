classify <-
function(classifier=NULL, data=NULL, pre=function(x, y=NULL) { list(features=x) }, save.models=NA, use.weights = TRUE) {
    if(!testClass(classifier, "Learner") && !testList(classifier, types="Learner")) {
        stop("Need classifier or list of classifiers!")
    }
    assertClass(data, "llama.data")
    hs = attr(data, "hasSplits")
    if(is.null(hs) || hs != TRUE) {
        stop("Need data with train/test split!")
    }
    if(testClass(classifier, "Learner")) { classifier = list(classifier) }
    combinator = "majority"
    if(!is.null(classifier$.combine)) {
        combinator = classifier$.combine
        classifier = classifier[-which(names(classifier) == ".combine")]
    }

    totalBests = data.frame(target=factor(breakBestTies(data), levels=data$performance))

    predictions = do.call(rbind, parallelMap(function(i) {
        trf = pre(data$data[data$train[[i]],][data$features])
        tsf = pre(data$data[data$test[[i]],][data$features], trf$meta)
        ids = data$data[data$test[[i]],][data$ids]
        trp = data$data[data$train[[i]],][data$performance]
        trw = abs(apply(trp, 1, max) - apply(trp, 1, min))

        trainpredictions = data.frame(row.names=1:nrow(trf$features))
        ensemblepredictions = data.frame(row.names=1:nrow(tsf$features))

        trainBests = data.frame(target=factor(breakBestTies(data, i), levels=data$performance))
        for(j in 1:length(classifier)) {
            if(hasProperties(classifier[[j]], "weights") && use.weights) {
                task = makeClassifTask(id="classify", target="target", weights = trw, data=cbind(trainBests, trf$features))
            } else {
                task = makeClassifTask(id="classify", target="target", data=cbind(trainBests, trf$features))
            }
            model = train(classifier[[j]], task = task)
            if(!is.na(save.models)) {
                saveRDS(list(model=model, train.data=task, test.data=tsf$features), file = paste(save.models, classifier[[j]]$id, i, "rds", sep="."))
            }
            if(inherits(combinator, "Learner")) { # only do this if we need it
                trainpredictions[,j] = predict(model, newdata=trf$features)$data$response
            }
            ensemblepredictions[,j] = predict(model, newdata=tsf$features)$data$response
        }
        if(inherits(combinator, "Learner")) {
            if(hasProperties(combinator, "weights") && use.weights) {
                task = makeClassifTask(id="classify", target="target", weights = trw, data=cbind(trainBests, trf$features, lapply(trainpredictions, factor, levels=data$performance)))
            } else {
                task = makeClassifTask(id="classify", target="target", data=cbind(trainBests, trf$features, lapply(trainpredictions, factor, levels=data$performance)))
            }
            combinedmodel = train(combinator, task = task)
            if(!is.na(save.models)) {
                saveRDS(list(model=combinedmodel, train.data=task, test.data=cbind(tsf$features, lapply(ensemblepredictions, factor, levels=data$performance))), file = paste(save.models, combinator$id, "combined", i, "rds", sep="."))
            }
            preds = predict(combinedmodel, newdata=cbind(tsf$features, lapply(ensemblepredictions, factor, levels=data$performance)))$data$response
            combinedpredictions = do.call(rbind, lapply(1:length(preds), function(j) {
                if(all(is.na(preds[j,drop=F]))) {
                    data.frame(ids[j,,drop=F], algorithm=NA, score=-Inf, iteration=i, row.names = NULL)
                } else {
                    tab = as.table(sort(table(preds[j,drop=F]), decreasing=T))
                    data.frame(ids[j,,drop=F], algorithm=names(tab), score=as.vector(tab), iteration=i, row.names = NULL)
                }
            }))
        } else {
            combinedpredictions = do.call(rbind, lapply(1:nrow(ensemblepredictions), function(j) {
                if(all(is.na(ensemblepredictions[j,]))) {
                    data.frame(ids[j,,drop=F], algorithm=NA, score=-Inf, iteration=i, row.names = NULL)
                } else {
                    tab = as.table(sort(table(unlist(ensemblepredictions[j,])), decreasing=T))
                    data.frame(ids[j,,drop=F], algorithm=names(tab), score=as.vector(tab), iteration=i, row.names = NULL)
                }
            }))
        }
        return(combinedpredictions)
    }, 1:length(data$train), level = "llama.llama-fold"))

    fs = pre(data$data[data$features])
    fp = data$data[data$performance]
    fw = abs(apply(fp, 1, max) - apply(fp, 1, min))
    models = lapply(1:length(classifier), function(i) {
        if(hasProperties(classifier[[i]], "weights") && use.weights) {
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
        if(hasProperties(combinator, "weights") && use.weights) {
            task = makeClassifTask(id="classify", target="target", weights = fw, data=cbind(totalBests, fs$features, lapply(trainpredictions, factor, levels=data$performance)))
        } else {
            task = makeClassifTask(id="classify", target="target", data=cbind(totalBests, fs$features, lapply(trainpredictions, factor, levels=data$performance)))
        }
        combinedmodel = train(combinator, task = task)
    }

    predictor = function(x) {
        tsf = pre(x[data$features], fs$meta)
        if(length(intersect(colnames(x), data$ids)) > 0) {
            ids = x[data$ids]
        } else {
            ids = data.frame(id = 1:nrow(x)) # don't have IDs, generate them
        }
        ensemblepredictions = data.frame(row.names=1:nrow(tsf$features))
        for(i in 1:length(classifier)) {
            ensemblepredictions[,i] = predict(models[[i]], newdata=tsf$features)$data$response
        }
        if(inherits(combinator, "Learner")) {
            preds = predict(combinedmodel, newdata=cbind(tsf$features, lapply(ensemblepredictions, factor, levels=data$performance)))$data$response
            combinedpredictions = do.call(rbind, lapply(1:length(preds), function(j) {
                if(all(is.na(preds[j,drop=F]))) {
                    data.frame(ids[j,,drop=F], algorithm=NA, score=-Inf, iteration=1, row.names = NULL)
                } else {
                    tab = as.table(sort(table(preds[j,drop=F]), decreasing=T))
                    data.frame(ids[j,,drop=F], algorithm=names(tab), score=as.vector(tab), iteration=1, row.names = NULL)
                }
            }))
        } else {
            combinedpredictions = do.call(rbind, lapply(1:nrow(ensemblepredictions), function(j) {
                if(all(is.na(ensemblepredictions[j,]))) {
                    data.frame(ids[j,,drop=F], algorithm=NA, score=-Inf, iteration=1, row.names = NULL)
                } else {
                    tab = as.table(sort(table(unlist(ensemblepredictions[j,])), decreasing=T))
                    data.frame(ids[j,,drop=F], algorithm=names(tab), score=as.vector(tab), iteration=1, row.names = NULL)
                }
            }))
        }
        return(combinedpredictions)
    }
    class(predictor) = "llama.model"
    attr(predictor, "type") = "classify"
    attr(predictor, "hasPredictions") = FALSE
    attr(predictor, "addCosts") = TRUE

    retval = list(predictions=predictions, models=models, predictor=predictor)
    class(retval) = "llama.model"
    attr(retval, "type") = "classify"
    attr(retval, "hasPredictions") = TRUE
    attr(retval, "addCosts") = TRUE

    return(retval)
}
