cvFolds <-
function(data, nfolds = 10, stratify = FALSE) {
    if(nfolds == -1) {
        stratify = F
        nfolds = nrow(data$data)
    }
    if(stratify) {
        stratifier = sapply(data$best, paste, collapse="-")
    } else {
        stratifier = rep.int(T, nrow(data$data))
    }

    tmp = do.call(c, by(1:nrow(data$data), stratifier, function(x) {
        n = length(x)
        rep(1:nfolds, length.out = n)[sample(n, n)]
    }))
    if(length(unique(tmp)) != nfolds) {
        stop(paste("Requested ", nfolds, " folds, but cannot produce this many.", sep=""))
    }
    parts = split(1:nrow(data$data), tmp)

    newdata = c(data,
            list(train = lapply(1:nfolds, function(x) { return(unsplit(parts[-x], tmp[tmp!=x])) }),
                 test = lapply(1:nfolds, function(x) { return(parts[[x]]) })))
    class(newdata) = "llama.data"
    attr(newdata, "hasSplits") = TRUE
    return(newdata)
}
