predNames = c("algorithm", "score")
vbs <-
function(data=NULL) {
    if(is.null(data)) {
        stop("Need data to determine virtual best!")
    }
    lens = sapply(data$best, length)
    idxs = unlist(lapply(1:length(lens), function(i) { rep.int(i, lens[i]) }))
    if(is.null(data$algorithmFeatures)) {
        ids = data$data[idxs,data$ids,drop=F]
    } else {
        ids = unique(data$data[[data$ids]])[idxs]
        ids = data.frame(ids)
        colnames(ids) = data$ids
    }
    
    bests = unlist(data$best)
    scores = rep.int(1, length(bests))
    scores[is.na(bests)] = 0
    return(data.frame(ids, algorithm = factor(bests), score = scores, iteration = 1))
}
class(vbs) = "llama.model"
attr(vbs, "type") = "virtual best"
attr(vbs, "hasPredictions") = FALSE
attr(vbs, "addCosts") = FALSE

singleBestByCount <-
function(data=NULL) {
    if(is.null(data)) {
        stop("Need data to determine single best!")
    }
    best = breakBestTies(data)
    if(is.null(data$algorithmFeatures)) {
        ids = data$data[data$ids]
    } else { 
        best = best[seq(1, length(best), length(data$algorithmNames))]
        ids = unique(data$data[data$ids]) 
    }
    data.frame(ids, algorithm = factor(names(sort(table(best), decreasing=T)[1])), score = 1, iteration = 1)
}
class(singleBestByCount) = "llama.model"
attr(singleBestByCount, "type") = "single best"
attr(singleBestByCount, "hasPredictions") = FALSE
attr(singleBestByCount, "addCosts") = FALSE

singleBestByPar <-
function(data=NULL, factor=10) {
    if(is.null(data)) {
        stop("Need data to determine single best!")
    }
    if(is.null(data$success)) {
        stop("Need successes to compute PAR scores.")
    }
    if(is.null(data$algorithmFeatures)) {
        algos = data$performance
    } else {
        algos = data$algorithmNames 
    }
    
    best = names(sort(sapply(algos, function(x) {
        prs = data.frame(data$data[,data$ids,drop=F], algorithm = factor(x), score = 1, iteration = 1)
        if(!is.null(data$algorithmFeatures)) {
            prs = unique(prs)
        }
        model = list(predictions=prs, ids=c("id"))
        class(model) = "llama.model"
        attr(model, "hasPredictions") = TRUE
        attr(model, "addCosts") = FALSE
        mean(parscores(data, model, factor=factor))
    })))[1]
    if(is.null(data$algorithmFeatures)) {
        ids = data$data[data$ids]
    } else {
        ids = unique(data$data[data$ids])
    }
    data.frame(ids, algorithm = factor(best), score = 1, iteration = 1)
}
class(singleBestByPar) = "llama.model"
attr(singleBestByPar, "type") = "single best"
attr(singleBestByPar, "hasPredictions") = FALSE
attr(singleBestByPar, "addCosts") = FALSE

singleBestBySuccesses <-
function(data=NULL) {
    if(is.null(data)) {
        stop("Need data to determine single best!")
    }
    if(is.null(data$success)) {
        stop("Need successes to compute successes.")
    }
    if(is.null(data$algorithmFeatures)) {
        algos = data$performance
    } else {
        algos = data$algorithmNames 
    }
    
    best = names(sort(sapply(algos, function(x) {
        prs = data.frame(data$data[,data$ids,drop=F], algorithm = factor(x), score = 1, iteration = 1)
        if(!is.null(data$algorithmFeatures)) {
            prs = unique(prs)
        }
        model = list(predictions=prs, ids=c("id"))
        class(model) = "llama.model"
        attr(model, "hasPredictions") = TRUE
        attr(model, "addCosts") = FALSE
        mean(successes(data, model))
    }), decreasing=T))[1]
    if(is.null(data$algorithmFeatures)) {
        ids = data$data[data$ids]
    } else {
        ids = unique(data$data[data$ids])
    }
    data.frame(ids, algorithm = factor(best), score = 1, iteration = 1)
}
class(singleBestBySuccesses) = "llama.model"
attr(singleBestBySuccesses, "type") = "single best"
attr(singleBestBySuccesses, "hasPredictions") = FALSE
attr(singleBestBySuccesses, "addCosts") = FALSE

singleBest <-
function(data=NULL) {
    if(is.null(data)) {
        stop("Need data to determine single best!")
    }
    
    if(is.null(data$algorithmFeatures)) {
        best = names(sort(sapply(data$performance, function(x) { mean(data$data[,x]) }), decreasing=!data$minimize))[1]
        ids = data$data[data$ids]
    } else {
        best = names(sort(sapply(data$algorithmNames, function(x) { mean(data$data[data$data[[data$algos]] == x, data$performance]) }), decreasing=!data$minimize))[1]
        ids = unique(data$data[data$ids])
    }
    data.frame(ids, algorithm = factor(best), score = 1, iteration = 1)
    
}
class(singleBest) = "llama.model"
attr(singleBest, "type") = "single best"
attr(singleBest, "hasPredictions") = FALSE
attr(singleBest, "addCosts") = FALSE

breakBestTies <-
function(data=NULL, fold=NULL, pairs=FALSE) {
    if(is.null(data)) {
        stop("Need data to break ties!")
    }
    
    optfun = if(data$minimize) { which.min } else { which.max }
    if(!is.null(data$algorithmFeatures)) {
        if(is.null(fold)) {
            perfs = data$data[c(data$performance, data$algos, data$ids)]
        } else {
            perfs = data$data[data$train[[fold]],][c(data$performance, data$algos, data$ids)]
        }
        if(pairs) {
            if(is.null(fold)) {
                perfs = data$data[c(data$performance, data$algos, data$ids)]
            } else {
                perfs = data$data[data$train[[fold]],][c(data$performance, data$algos, data$ids)]
            }
            combns = combn(data$algorithmNames, 2)
            values = lapply(1:ncol(combns), function(j) {
                p = perfs[perfs[[data$algos]] == combns[1,j], ]
                return(p = p)
            })
            perfs = as.data.frame(rbindlist(lapply(values, function(x) { x })))
        }
        best = factor(sapply(perfs[[data$ids]], function(x) { order=perfs[perfs[[data$ids]]==x, data$algos]; order[optfun(unlist(perfs[perfs[[data$ids]]==x, data$performance]))] }))
    } else {
        if(is.null(fold)) {
            perfs = data$data[data$performance]
        } else {
            perfs = data$data[data$train[[fold]],][data$performance]
        }
        order = names(sort(sapply(perfs, mean), decreasing=!data$minimize))
        best = factor(apply(perfs[order], 1, function(x) { order[optfun(unlist(x))] }))
    }
    
    names(best) = NULL
    return(best)
}

convertLongToWide <- 
function(data=NULL, timevar=NULL, idvar=NULL, prefix=NULL, remove.id=TRUE) {
    data = reshape(data, direction = "wide", timevar = timevar, idvar = idvar)
    colnames(data) = gsub(prefix, "", colnames(data))
    if(remove.id) {
        data[[idvar]] = NULL
    }
    return(data)
}

predTable <-
function(predictions=NULL, bestOnly=TRUE) {
    if(is.null(predictions)) {
        stop("Need predictions to tabulate!")
    }
    if(bestOnly) {
        ids = setdiff(names(predictions), c(predNames))
        lvls = levels(predictions$algorithm)
        algorithms = factor(lvls[as.vector(by(predictions$algorithm, predictions[ids], head, 1))])
    } else {
        algorithms = predictions$algorithm
    }
    sort(table(algorithms), decreasing = TRUE)
}

