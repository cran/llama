trainTest <-
function(data, trainpart = 0.6, stratify = FALSE) {
    assertClass(data, "llama.data")
    assertNumeric(trainpart)

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
        c(rep.int(1, round(n*trainpart)), rep.int(2, n-round(n*trainpart)))[sample(n, n)]
    }))
    if(!is.null(data$algorithmFeatures)) {
        tmp = tmp[match(data$data[[data$ids]], d[[data$ids]])]
    }
    parts = split(1:nrow(data$data), tmp)

    newdata = data
    newdata$train = list(parts[[1]])
    newdata$test = list(parts[[2]])
    attr(newdata, "hasSplits") = TRUE
    return(newdata)
}

