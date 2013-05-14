imputeCensored <-
function(data=NULL, estimator=lm, epsilon=0.1, maxit=1000) {
    if(is.null(data)) {
        stop("No data given!")
    }
    if(is.null(data$success)) {
        stop("Need successes to impute censored values!")
    }
    if(epsilon <= 0) {
        stop("Epsilon must be > 0!")
    }

    data$original_data = data$data

    i = 0
    foreach(i = 1:length(data$success)) %dopar% {
        s = data$success[i]
        p = data$performance[i]
        if(!any(data$data[[s]])) {
            stop(paste("Cannot impute for ", p, ", no non-censored values!"), sep="")
        }
        if(!all(data$data[[s]])) {
            haveind = (1:nrow(data$data))[data$data[[s]]]
            wantind = (1:nrow(data$data))[!data$data[[s]]]
            model = estimator(data$data[haveind,][[p]]~., data=subset(data$data[haveind,], T, data$features))
            data$data[wantind,][[p]] = predict(model, subset(data$data[wantind,], T, data$features))

            diff = Inf
            it = 1
            while(diff > epsilon) {
                model = estimator(data$data[[p]]~., data=subset(data$data, T, data$features))
                preds = predict(model, subset(data$data[wantind,], T, data$features))
                diff = max(abs(preds - data$data[wantind,][[p]]))
                data$data[wantind,][[p]] = preds
                it = it + 1
                if(it > maxit) {
                    warning(paste("Did not reach convergence within ", maxit, " iterations for ", p, ".", sep=""))
                    break
                }
            }
            data$data[[s]] = rep.int(T, nrow(data$data))
        }
    }

    return(data)
}
