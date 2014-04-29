cvFolds <-
function(data, nfolds=10, stratify=T) {
    if(nfolds == -1) {
        stratify = F
        nfolds = nrow(data$data)
    }
    if(stratify) {
        data$data$stratifier = sapply(data$data$best, paste, collapse="-")
    }
    tmp = ddply(data$data, if(stratify) "stratifier", function(x) {
        n = nrow(x)
        x$fold = rep(1:nfolds, length.out = n)[sample(n, n)]
        x
    })
    data$data$stratifier = NULL
    if(length(unique(tmp$fold)) != nfolds) {
        stop(paste("Requested ", nfolds, " folds, but cannot produce this many.", sep=""))
    }
    parts = split(data$data, tmp$fold)
    return(c(data,
            list(train = lapply(1:nfolds, function(x) { return(unsplit(parts[-x], tmp$fold[tmp$fold!=x])) }),
                 test = lapply(1:nfolds, function(x) { return(parts[[x]]) }))))
}
