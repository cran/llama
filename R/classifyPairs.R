classifyPairs <-
function(classifier=NULL, data=NULL, pre=function(x, y=NULL) { list(features=x) }, combine=NULL, save.models=NA, use.weights = TRUE) {
    if(!testClass(classifier, "Learner")) {
        stop("Need classifier!")
    }
    assertClass(data, "llama.data")
    hs = attr(data, "hasSplits")
    if(is.null(hs) || hs != TRUE) {
        stop("Need data with train/test split!")
    }

    totalBests = data.frame(target=factor(breakBestTies(data), levels=data$performance))
    combns = combn(data$performance, 2)
    predictions = do.call(rbind, parallelMap(function(i) {
        trf = pre(data$data[data$train[[i]],][data$features])
        tsf = pre(data$data[data$test[[i]],][data$features], trf$meta)
        ids = data$data[data$test[[i]],][data$ids]
        trp = data$data[data$train[[i]],][data$performance]

        trainpredictions = data.frame(row.names=1:nrow(trf$features))
        pairpredictions = data.frame(row.names=1:nrow(tsf$features))
        for (j in 1:ncol(combns)) {
            if(data$minimize) {
                cmp = function(x, y) {
                    sapply(data$data[data$train[[i]],][x] < data$data[data$train[[i]],][y], function(z) { if(z) { x } else { y } })
                }
            } else {
                cmp = function(x, y) {
                    sapply(data$data[data$train[[i]],][x] > data$data[data$train[[i]],][y], function(z) { if(z) { x } else { y } })
                }
            }
            labels = data.frame(target=factor(cmp(combns[1,j], combns[2,j]), levels=data$performance))
            if(hasProperties(classifier, "weights") && use.weights) {
                task = makeClassifTask(id="classifyPairs", target="target", weights=abs(data$data[data$train[[i]],][combns[1,j]] - data$data[data$train[[i]],][combns[2,j]]), data=cbind(labels, trf$features), fixup.data="quiet")
            } else {
                task = makeClassifTask(id="classifyPairs", target="target", data=cbind(labels, trf$features), fixup.data="quiet")
            }
            model = train(classifier, task = task)
            if(!is.na(save.models)) {
                saveRDS(list(model=model, train.data=task, test.data=tsf$features), file = paste(save.models, classifier$id, combns[1,j], combns[2,j], i, "rds", sep="."))
            }
            if(!is.null(combine)) { # only do this if we need it
                trainpredictions[,j] = predict(model, newdata=trf$features)$data$response
            }
            pairpredictions[,j] = predict(model, newdata=tsf$features)$data$response
        }

        if(!is.null(combine)) {
            trainBests = data.frame(target=factor(breakBestTies(data, i), levels=data$performance))
            if(hasProperties(combine, "weights") && use.weights) {
                trw = abs(apply(trp, 1, max) - apply(trp, 1, min))
                task = makeClassifTask(id="classifyPairs", target="target", weights=trw, data=cbind(trainBests, trf$features, lapply(data.frame(trainpredictions), factor, levels=data$performance)), fixup.data="quiet")
            } else {
                task = makeClassifTask(id="classifyPairs", target="target", data=cbind(trainBests, trf$features, lapply(data.frame(trainpredictions), factor, levels=data$performance)), fixup.data="quiet")
            }
            combinedmodel = train(combine, task = task)
            if(!is.na(save.models)) {
                saveRDS(list(model=combinedmodel, train.data=task, test.data=cbind(tsf$features, lapply(data.frame(pairpredictions), factor, levels=data$performance))), file = paste(save.models, combine$id, "combined", i, "rds", sep="."))
            }
            preds = predict(combinedmodel, newdata=cbind(tsf$features, lapply(data.frame(pairpredictions), factor, levels=data$performance)))$data$response
            combinedpredictions = do.call(rbind, lapply(1:length(preds), function(j) {
                if(all(is.na(preds[j,drop=F]))) {
                    data.frame(ids[j,,drop=F], algorithm=NA, score=-Inf, iteration=i, row.names = NULL)
                } else {
                    tab = as.table(sort(table(preds[j,drop=F]), decreasing=T))
                    data.frame(ids[j,,drop=F], algorithm=names(tab), score=as.vector(tab), iteration=i, row.names = NULL)
                }
            }))
        } else {
            combinedpredictions = do.call(rbind, lapply(1:nrow(pairpredictions), function(j) {
                if(all(is.na(pairpredictions[j,]))) {
                    data.frame(ids[j,,drop=F], algorithm=NA, score=-Inf, iteration=i, row.names = NULL)
                } else {
                    tab = as.table(sort(table(unlist(pairpredictions[j,])), decreasing=T))
                    data.frame(ids[j,,drop=F], algorithm=names(tab), score=as.vector(tab), iteration=i, row.names = NULL)
                }
            }))
        }
        return(combinedpredictions)
    }, 1:length(data$train), level = "llama.llama-fold"))

    fs = pre(data$data[data$features])
    fp = data$data[data$performance]
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
        if(hasProperties(classifier, "weights") && use.weights) {
            task = makeClassifTask(id="classifyPairs", target="target", weights=abs(data$data[[combns[1,i]]] - data$data[[combns[2,i]]]), data=cbind(labels, fs$features), fixup.data="quiet")
        } else {
            task = makeClassifTask(id="classifyPairs", target="target", data=cbind(labels, fs$features), fixup.data="quiet")
        }
        return(train(classifier, task = task))
    })
    if(!is.null(combine)) {
        trainpredictions = data.frame(row.names=1:nrow(fs$features))
        for(i in 1:ncol(combns)) {
            trainpredictions[,i] = predict(models[[i]], newdata=fs$features)$data$response
        }
        if(hasProperties(combine, "weights") && use.weights) {
            task = makeClassifTask(id="classifyPairs", target="target", weights=fw, data=cbind(totalBests, fs$features, lapply(data.frame(trainpredictions), factor, levels=data$performance)), fixup.data="quiet")
        } else {
            task = makeClassifTask(id="classifyPairs", target="target", data=cbind(totalBests, fs$features, lapply(data.frame(trainpredictions), factor, levels=data$performance)), fixup.data="quiet")
        }
        combinedmodel = train(combine, task = task)
    }

    predictor = function(x) {
        tsf = pre(x[data$features], fs$meta)
        if(length(intersect(colnames(x), data$ids)) > 0) {
            ids = x[data$ids]
        } else {
            ids = data.frame(id = 1:nrow(x)) # don't have IDs, generate them
        }
        pairpredictions = data.frame(row.names=1:nrow(tsf$features))
        for(i in 1:ncol(combns)) {
            pairpredictions[,i] = predict(models[[i]], newdata=tsf$features)$data$response
        }
        if(!is.null(combine)) {
            preds = predict(combinedmodel, newdata=cbind(tsf$features, lapply(data.frame(pairpredictions), factor, levels=data$performance)))$data$response
            combinedpredictions = do.call(rbind, lapply(1:length(preds), function(j) {
                if(all(is.na(preds[j,drop=F]))) {
                    data.frame(ids[j,,drop=F], algorithm=NA, score=-Inf, iteration=1, row.names = NULL)
                } else {
                    tab = as.table(sort(table(preds[j,drop=F]), decreasing=T))
                    data.frame(ids[j,,drop=F], algorithm=names(tab), score=as.vector(tab), iteration=1, row.names = NULL)
                }
            }))
        } else {
            combinedpredictions = do.call(rbind, lapply(1:nrow(pairpredictions), function(j) {
                if(all(is.na(pairpredictions[j,]))) {
                    data.frame(ids[j,,drop=F], algorithm=NA, score=-Inf, iteration=1, row.names = NULL)
                } else {
                    tab = as.table(sort(table(unlist(pairpredictions[j,])), decreasing=T))
                    data.frame(ids[j,,drop=F], algorithm=names(tab), score=as.vector(tab), iteration=1, row.names = NULL)
                }
            }))
        }
        return(combinedpredictions)
    }
    class(predictor) = "llama.model"
    attr(predictor, "type") = "classifyPairs"
    attr(predictor, "hasPredictions") = FALSE
    attr(predictor, "addCosts") = TRUE

    retval = list(predictions=predictions, models=models, predictor=predictor)
    class(retval) = "llama.model"
    attr(retval, "type") = "classifyPairs"
    attr(retval, "hasPredictions") = TRUE
    attr(retval, "addCosts") = TRUE

    return(retval)
}
