trainTest <-
function(data, trainpart=0.6, stratify=T) {
    if(stratify) {
        data$data$stratifier = sapply(data$data$best, paste, collapse="-")
    }
    tmp = ddply(data$data, if(stratify) "stratifier", function(x) {
        n = nrow(x)
        x$train = c(rep.int(1, round(n*trainpart)), rep.int(2, n-round(n*trainpart)))[sample(n, n)]
        x
    })
    data$data$stratifier = NULL
    parts = split(data$data, tmp$train)
    return(c(data, list(train = list(parts[[1]]), test = list(parts[[2]]))))
}
