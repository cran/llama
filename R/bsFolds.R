bsFolds <-
function(data, nfolds = 10L, stratify = FALSE) {
    assertClass(data, "llama.data")
    assertInteger(nfolds)
    
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
            best = data$best[seq(1, length(data$best), length(unique(data$data[[data$algos]])))]
        }
        stratifier = sapply(best, paste, collapse="-")
    } else {
        stratifier = rep.int(TRUE, nrow(d))
    }
    
    trainIdxs = lapply(1:nfolds, function(i) {
        tmp = by(1:nrow(d), stratifier, function(x) {
            unique(sample(x, length(x), replace = TRUE))
        })
        if(!is.null(data$algorithmFeatures)) {
            t = lapply(1:length(tmp[[1]]), function(j) {
                which(data$data[[data$ids]] == d[[data$ids]][tmp[[1]][j]])
            })
            as.integer(unlist(t))
        } else {
            as.integer(unlist(tmp))
        }
    })
    
    newdata = data
    newdata$train = trainIdxs
    newdata$test = lapply(trainIdxs, function(x) { setdiff(1:nrow(data$data), x) })
    attr(newdata, "hasSplits") = TRUE
    return(newdata)
}

