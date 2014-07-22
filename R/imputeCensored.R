imputeCensored <-
function(data=NULL, estimator=makeLearner("regr.lm"), epsilon=0.1, maxit=1000) {
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
    for(i in 1:length(data$success)) {
        s = data$success[i]
        p = data$performance[i]
        if(!any(data$data[[s]])) {
            stop(paste("Cannot impute for ", p, ", no non-censored values!"), sep="")
        }
        if(!all(data$data[[s]])) {
            haveind = (1:nrow(data$data))[data$data[[s]]]
            wantind = (1:nrow(data$data))[!data$data[[s]]]
            task = makeRegrTask(id="imputation", target="target", data=cbind(data.frame(target=data$data[haveind,][[p]]), subset(data$data[haveind,], T, data$features)))
            model = train(estimator, task=task)
            data$data[wantind,][[p]] = predict(model, newdata=subset(data$data[wantind,], T, data$features))$data$response

            diff = Inf
            it = 1
            while(diff > epsilon) {
                task = makeRegrTask(id="imputation", target="target", data=cbind(data.frame(target=data$data[[p]]), subset(data$data, T, data$features)))
                model = train(estimator, task=task)
                preds = predict(model, newdata=subset(data$data[wantind,], T, data$features))$data$response
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
