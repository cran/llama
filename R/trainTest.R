trainTest <-
function(data, trainpart = 0.6, stratify = FALSE) {
    if(stratify) {
        stratifier = sapply(data$best, paste, collapse="-")
    } else {
        stratifier = rep.int(T, nrow(data$data))
    }

    tmp = do.call(c, by(1:nrow(data$data), stratifier, function(x) {
        n = length(x)
        c(rep.int(1, round(n*trainpart)), rep.int(2, n-round(n*trainpart)))[sample(n, n)]
    }))
    parts = split(1:nrow(data$data), tmp)

    newdata = c(data, list(train = list(parts[[1]]), test = list(parts[[2]])))
    class(newdata) = "llama.data"
    attr(newdata, "hasSplits") = TRUE
    return(newdata)
}
