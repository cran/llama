bsFolds <-
function(data, nfolds = 10, stratify = FALSE) {
    if(stratify) {
        stratifier = sapply(data$best, paste, collapse="-")
    } else {
        stratifier = rep.int(T, nrow(data$data))
    }

    trainIdxs = lapply(1:nfolds, function(i) {
        tmp = by(1:nrow(data$data), stratifier, function(x) {
            unique(sample(x, length(x), replace = TRUE))
        })
        as.integer(unlist(tmp))
    })

    newdata = c(data,
            list(train = trainIdxs,
                 test = lapply(trainIdxs, function(x) { setdiff(1:nrow(data$data), x) })))
    class(newdata) = "llama.data"
    attr(newdata, "hasSplits") = TRUE
    return(newdata)
}
