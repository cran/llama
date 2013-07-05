testclassifier = setClass("testclassifier", representation(data="list"), contains="formula", where=globalenv())
predict.testclassifier = function(object, y) { return(rep.int(get("labels", environment(formula(object)))[1], nrow(y))) } 
setMethod("predict", signature(object="testclassifier"), predict.testclassifier, where=globalenv())

othertestclassifier = setClass("othertestclassifier", representation(data="list"), contains="formula", where=globalenv())
predict.othertestclassifier = function(object, y) { return(rep.int("a", nrow(y))) } 
setMethod("predict", signature(object="othertestclassifier"), predict.othertestclassifier, where=globalenv())

test_that("classifyPairs classifies", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("foo", 10), foo=rep.int(0, 10), bar=rep.int(1, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"))
    res = classifyPairs(classifier=testclassifier, d)
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm="foo", score=1) }) })))
})

test_that("classifyPairs returns predictor", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("foo", 10), foo=rep.int(0, 10), bar=rep.int(1, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"))
    res = classifyPairs(classifier=testclassifier, d)
    expect_true(all(sapply(res$predictor(fold), function(x) { x == data.frame(algorithm="foo", score=1) })))
})

test_that("classifyPairs raises error without classifier", {
    expect_error(classifyPairs(), "No classifier given!")
})

test_that("classifyPairs raises error without train/test split", {
    expect_error(classifyPairs(function() { return(NULL) }), "Need data with train/test split!")
})

test_that("classifyPairs respects minimize", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("bar", 10), foo=rep.int(0, 10), bar=rep.int(1, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"))
    res = classifyPairs(classifier=testclassifier, d, minimize=F)
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm="bar", score=1) }) })))
    expect_true(all(sapply(res$predictor(fold), function(x) { x == data.frame(algorithm="bar", score=1) })))
})

test_that("classifyPairs allows combination function", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("foo", 10), foo=rep.int(0, 10), bar=rep.int(1, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("foo", "bar"))
    res = classifyPairs(classifier=testclassifier, d, combinator=othertestclassifier)
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm="a", score=1) }) })))
    expect_true(all(sapply(res$predictor(fold), function(x) { x == data.frame(algorithm="a", score=1) })))
})
