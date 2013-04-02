input <-
function(features, performances, successes=NULL, minimize=T) {
    if(nrow(features) != nrow(performances) || (!is.null(successes) && (nrow(features) != nrow(successes) || nrow(performances) != nrow(successes)))) {
        warning("Different number of rows in data frames, taking only common rows.")
    }
    common = intersect(names(features), names(performances))
    pnames = setdiff(names(performances), common)
    combined = merge(features, performances)
    optfun = if(minimize) { min } else { max }
    combined$best = factor(apply(subset(combined, T, pnames), 1, function(x) { pnames[head(which(unlist(x) == optfun(unlist(x))), 1)] }))

    snames = NULL
    if(!is.null(successes)) {
        names(successes) = c(intersect(names(successes), common), sapply(setdiff(names(successes), common), function(x) { paste(x, "success", sep="_") }))
        combined = merge(combined, successes)
        snames = setdiff(names(successes), common)
    }

    return(list(data=combined, features=setdiff(names(features), common), performance=pnames, success=snames))
}
