testregressor = setClass("testregressor", representation(data="list"), contains="formula", where=globalenv())
predict.testregressor = function(object, y) {
    suppressWarnings({
    terms = terms(as.formula(object), data=get("data", envir=environment(object)))
    predictions = eval(attr(terms, "variables")[[2]], envir=environment(object))
    })
    return(rep.int(predictions[1], nrow(y)))
    } 
setMethod("predict", signature(object="testregressor"), predict.testregressor, where=globalenv())

test_that("regression predicts", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10), foo=rep.int(1, 10), bar=rep.int(2, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"))
    res = regression(testregressor, d)
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm=c("foo", "bar"), score=c(1, 2)) }) })))
})

test_that("regression returns predictor", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10), foo=rep.int(1, 10), bar=rep.int(2, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"))
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
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"))
    res = regression(testregressor, d, combine=max)
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm=c("foo", "bar"), score=c(2, 1)) }) })))
    expect_true(all(sapply(res$predictor(fold), function(x) { x == data.frame(algorithm=c("foo", "bar"), score=c(2, 1)) })))
})

testclassifier = setClass("testclassifier", representation(data="list"), contains="formula", where=globalenv())
predict.testclassifier = function(object, y) { return(rep.int("b", nrow(y))) } 
setMethod("predict", signature(object="testclassifier"), predict.testclassifier, where=globalenv())

test_that("regression allows stacking", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10), foo=rep.int(1, 10), bar=rep.int(2, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"))
    res = regression(testregressor, d, combine=testclassifier, stack=T)
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm="b", score=1) }) })))
    expect_true(all(sapply(res$predictor(fold), function(x) { x == data.frame(algorithm="b", score=1) })))
})
