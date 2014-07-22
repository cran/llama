test_that("regression predicts", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10), foo=rep.int(1, 10), bar=rep.int(2, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"), minimize=T)
    res = regression(testregressor, d)
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm=c("foo", "bar"), score=c(1, 2)) }) })))
})

test_that("regression returns predictor", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10), foo=rep.int(1, 10), bar=rep.int(2, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"), minimize=T)
    res = regression(testregressor, d)
    expect_true(all(sapply(res$predictor(fold), function(x) { x == data.frame(algorithm=c("foo", "bar"), score=c(1, 2)) })))
})

test_that("regression raises error without regressor", {
    expect_error(regression(), "No regressor given!")
})

test_that("regression raises error without train/test split", {
    expect_error(regression(function() { return(NULL) }), "Need data with train/test split!")
})

test_that("regression allows to combine by max", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10), foo=rep.int(2, 10), bar=rep.int(1, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"), minimize=F)
    res = regression(testregressor, d)
    expectedDf = data.frame(algorithm=c("foo", "bar"), score=c(2, 1))
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == expectedDf }) })))
    expect_true(all(sapply(res$predictor(fold), function(x) { x == expectedDf })))
})

test_that("regression allows stacking", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("foo", 10), foo=c(rep.int(1, 5), rep.int(2, 5)), bar=c(rep.int(2, 5), rep.int(1, 5)))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"), minimize=T)
    res = regression(testregressor, d, combine=bartestclassifier)
    expectedDf = data.frame(algorithm=c("bar", "foo"), score=c(1, 0))
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == expectedDf }) })))
    expect_true(all(sapply(res$predictor(fold), function(x) { x == expectedDf })))
})
