testclassifier = setClass("testclassifier", representation(data="list"), contains="formula", where=globalenv())
predict.testclassifier = function(object, y) { return(rep.int("b", nrow(y))) } 
setMethod("predict", signature(object="testclassifier"), predict.testclassifier, where=globalenv())

othertestclassifier = setClass("othertestclassifier", representation(data="list"), contains="formula", where=globalenv())
predict.othertestclassifier = function(object, y) { return(rep.int("a", nrow(y))) } 
setMethod("predict", signature(object="othertestclassifier"), predict.othertestclassifier, where=globalenv())

test_that("classify classifies", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"))
    res = classify(classifier=testclassifier, d)
    expect_true(all(unlist(res$predictions) == "b"))
})

test_that("classify returns predictor", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"))
    res = classify(classifier=testclassifier, d)
    expect_true(all(res$predictor(fold) == "b"))
})

test_that("classify raises error without classifier", {
    expect_error(classify(), "No classifier given!")
})

test_that("classify raises error without train/test split", {
    expect_error(classify(function() { return(NULL) }), "Need data with train/test split!")
})

test_that("classify takes list of predictors", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"))
    res = classify(classifier=list(testclassifier, testclassifier, testclassifier), d)
    expect_true(all(res$predictor(fold) == "b"))
})

test_that("classify takes list of predictors and combination function", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"))
    res = classify(classifier=list(testclassifier, testclassifier, testclassifier, .combine=othertestclassifier), d)
    expect_true(all(res$predictor(fold) == "a"))
})

test_that("classify ensemble does majority voting", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"))
    res = classify(classifier=list(testclassifier, othertestclassifier, othertestclassifier), d)
    expect_true(all(unlist(res$predictions) == "a"))
    expect_true(all(res$predictor(fold) == "a"))
})
