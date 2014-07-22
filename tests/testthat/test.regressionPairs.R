test_that("regressionPairs predicts", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("foo", 10), foo=rep.int(0, 10), bar=rep.int(1, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"), minimize=T)
    res = regressionPairs(regressor=testregressor, d)
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm=c("foo", "bar"), score=c(1, -1)) }) })))
})

test_that("regressionPairs returns predictor", {
   fold = data.frame(a=rep.int(0, 10), best=rep.int("foo", 10), foo=rep.int(0, 10), bar=rep.int(1, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"), minimize=T)
    res = regressionPairs(regressor=testregressor, d)
    expect_true(all(sapply(res$predictor(fold), function(x) { x == data.frame(algorithm=c("foo", "bar"), score=c(1, -1)) })))
})

test_that("regressionPairs raises error without regressor", {
    expect_error(regressionPairs(), "No regressor given!")
})

test_that("regressionPairs raises error without train/test split", {
    expect_error(regressionPairs(function() { return(NULL) }), "Need data with train/test split!")
})

test_that("regressionPairs respects minimize", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("bar", 10), foo=rep.int(0, 10), bar=rep.int(1, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"), minimize=F)
    res = regressionPairs(regressor=testregressor, d)
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm=c("bar", "foo"), score=c(1, -1)) }) })))
    expect_true(all(sapply(res$predictor(fold), function(x) { x == data.frame(algorithm=c("bar", "foo"), score=c(1, -1)) })))
})

test_that("regressionPairs allows combine classifier", {
    fold = data.frame(foo=rep.int(0, 10), best=c(rep.int("a", 5), rep.int("bar", 5)), a=c(rep.int(0, 5), rep.int(1, 5)), bar=c(rep.int(1, 5), rep.int(0, 5)))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("foo"), performance=c("a", "bar"), minimize=T)
    res = regressionPairs(regressor=testregressor, d, combine=othertestclassifier)
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm=c("a", "bar"), score=c(1, 0)) }) })))
    expect_true(all(sapply(res$predictor(fold), function(x) { x == data.frame(algorithm=c("a", "bar"), score=c(1, 0)) })))
})
