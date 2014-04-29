# FIXME: when excess (i.e. with no corresponding performance or feature) successes or costs are specified, no warning/error is given
input <-
function(features, performances, successes=NULL, costs=NULL, minimize=T) {
    if(nrow(features) != nrow(performances) || (!is.null(successes) && (nrow(features) != nrow(successes) || nrow(performances) != nrow(successes)))) {
        warning("Different number of rows in data frames, taking only common rows.")
    }
    common = intersect(names(features), names(performances))
    if(length(common) == 0) {
        stop("Performance can't be linked to features -- no common columns!")
    }
    pnames = setdiff(names(performances), common)

    ids = factor(do.call(paste0, subset(features, T, common)))
    if(length(ids) != length(levels(ids))) {
        stop("Common columns do not provide unique IDs!")
    }

    combined = merge(features, performances, by=common)
    optfun = if(minimize) { min } else { max }
    combined$best = apply(subset(combined, select=pnames), 1,
        function(x) {
            factor(pnames[which(unlist(x) == optfun(unlist(x)))])
        })
    # simplify...
    names(combined$best) = NULL

    snames = NULL
    if(!is.null(successes)) {
        commonSuccess = intersect(names(successes), common)
        successNames = setdiff(names(successes), common)
        if(length(commonSuccess) == 0) {
            stop("Successes can't be linked to rest of data -- no common columns!")
        }
        if(length(intersect(successNames, pnames)) != length(successNames)) {
            stop(paste("Successes (", paste(successNames, collapse=", "), ") and performances (", paste(pnames, collapse=", "), ") for different algorithms given!", sep=""))
        }
        names(successes) = c(commonSuccess, sapply(successNames[order(match(successNames, pnames))], function(x) { paste(x, "success", sep="_") }))
        combined = merge(combined, successes, by=common)
        snames = setdiff(names(successes), common)
    }

    retval = list(data=combined, features=setdiff(names(features), common), performance=pnames, success=snames, minimize=minimize)


    if(!is.null(costs)) {
        if(is.numeric(costs)) {
            # same cost for everything
            combined$cost = rep.int(costs, times=length(combined$best))
            cnames = c("cost")
        } else if(is.data.frame(costs)) {
            commonCosts = intersect(names(costs), common)
            if(length(commonCosts) == 0) {
                stop("Costs can't be linked to rest of data -- no common columns!")
            }
            cnames = setdiff(names(costs), common)
            if(length(intersect(cnames, retval$features)) != length(retval$features)) {
                stop(paste("Costs (", paste(cnames, collapse=", "), ") and features (", paste(retval$features, collapse=", "), ") are disjoint!", sep=""))
            }
            cnames = as.vector(sapply(cnames, function(x) { paste(x, "cost", sep="_") }))
            names(costs) = c(commonCosts, cnames)
            combined = merge(combined, costs, by=common)
        } else if(is.list(costs)) {
            # cost groups
            retval$costGroups = costs$groups
            commonCosts = intersect(names(costs$values), common)
            cnames = setdiff(names(costs$values), common)
            if(length(commonCosts) == 0) {
                stop("Costs can't be linked to rest of data -- no common columns!")
            }
            if(length(intersect(do.call(c, costs$groups), retval$features)) != length(retval$features)) {
                stop(paste("Cost groups (", paste(do.call(c, costs$groups), collapse=", "), ") and features (", paste(retval$features, collapse=", "), ") are disjoint!", sep=""))
            }
            if(length(intersect(names(costs$groups), cnames)) != length(names(costs$groups))) {
                stop(paste("Cost groups (", paste(do.call(c, costs$groups), collapse=", "), ") and costs (", paste(cnames, collapse=", "), ") are disjoint!", sep=""))
            }
            combined = merge(combined, costs$values, by=common)
        } else {
            stop("Invalid format for costs!")
        }
        retval$cost = cnames
        retval$data = combined
    }

    return(retval)
}
