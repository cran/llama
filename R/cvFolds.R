cvFolds <-
function(data, nfolds = 10L, stratify = FALSE) {
    assertClass(data, "llama.data")
    assertInteger(nfolds)
    
    if(nfolds == -1L) {
        stratify = FALSE
        if(is.null(data$algorithmFeatures)) {
            nfolds = nrow(data$data) 
        } else {
            nfolds = nrow(data$data) / length(data$algorithmNames)
        }
    }
    if(is.null(data$algorithmFeatures)) {
        d = data$data
    } else { 
        d = data$data[c(data$ids, data$algos, data$performance)]
        d = reshape(d, direction = "wide", timevar = data$algos, idvar = data$ids)
    }
    if(stratify) {
        if(is.null(data$algorithmFeatures)) {
            best = data$best
        } else {
            best = data$best[seq(1, length(data$best), length(data$algorithmNames))]
        }
        stratifier = sapply(best, paste, collapse="-")
    } else {
        stratifier = rep.int(TRUE, nrow(d))
    }
    
    tmp = do.call(c, by(1:nrow(d), stratifier, function(x) {
        n = length(x)
        rep(1:nfolds, length.out = n)[sample(n, n)]
    }))
    if(length(unique(tmp)) != nfolds) {
        stop(paste("Requested ", nfolds, " folds, but cannot produce this many.", sep=""))
    } 
    
    if(!is.null(data$algorithmFeatures)) {
        tmp = tmp[match(data$data[[data$ids]], d[[data$ids]])]
    }
    
    parts = split(1:nrow(data$data), tmp)
    
    newdata = data
    newdata$train = lapply(1:nfolds, function(x) { return(unsplit(parts[-x], tmp[tmp!=x])) })
    newdata$test = lapply(1:nfolds, function(x) { return(parts[[x]]) })
    attr(newdata, "hasSplits") = TRUE
    return(newdata)
}

