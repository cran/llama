cvFolds <-
function(data, nfolds=10, stratify=T) {
    if(nfolds == -1) {
        stratify = F
        nfolds = nrow(data$data)
    }
    tmp = ddply(data$data, if(stratify) { "best" }, function(x) {
        n = nrow(x)
        x$fold = rep(1:nfolds, length.out = n)[sample(n, n)]
        x
    })
    parts = split(data$data, tmp$fold)
    return(c(data,
            list(train = lapply(1:nfolds, function(x) { return(unsplit(parts[-x], tmp$fold[tmp$fold!=x])) }),
                 test = lapply(1:nfolds, function(x) { return(parts[[x]]) }))))
}
