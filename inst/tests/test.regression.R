testregressor = setClass("testregressor", representation(data="list"), contains="formula", where=globalenv())
predict.testregressor = function(object, y) { return(rep.int(1, nrow(y))) } 
setMethod("predict", signature(object="testregressor"), predict.testregressor, where=globalenv())

test_that("regression predicts", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10), foo=rep.int(0, 10), bar=rep.int(0, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"))
    res = regression(regressor=testregressor, d)
    expect_true(all(unlist(res$predictions) == "foo"))
})

test_that("regression returns predictor", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10), foo=rep.int(0, 10), bar=rep.int(0, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"))
    res = regression(regressor=testregressor, d)
    expect_true(all(res$predictor(fold) == "foo"))
})

test_that("regression raises error without regressor", {
    expect_error(regression(), "No regressor given!")
})

test_that("regression raises error without train/test split", {
    expect_error(regression(function() { return(NULL) }), "Need data with train/test split!")
})

testclassifier = setClass("testclassifier", representation(data="list"), contains="formula", where=globalenv())
predict.testclassifier = function(object, y) { return(rep.int("b", nrow(y))) } 
setMethod("predict", signature(object="testclassifier"), predict.testclassifier, where=globalenv())

test_that("regression allows stacking", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10), foo=rep.int(0, 10), bar=rep.int(0, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"))
    res = regression(regressor=testregressor, data=d, combine=testclassifier, stack=T)
    expect_true(all(unlist(res$predictions) == "b"))
})
