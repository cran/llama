testclusterer = setClass("testclusterer", representation(data="data.frame"), contains="data.frame", where=globalenv())
predict.testclusterer = function(object, y=NULL) {
    if(is.null(y)) {
        length = nrow(object)
    } else {
        length = nrow(y)
    }
    return(rep.int("b", length))
} 
setMethod("predict", signature(object="testclusterer"), predict.testclusterer, where=globalenv())

test_that("cluster predicts", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"))
    res = cluster(testclusterer, d)
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm="b", score=10) }) })))
})

test_that("cluster returns predictor", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"))
    res = cluster(testclusterer, d)
    expect_true(all(sapply(res$predictor(fold), function(x) { x == data.frame(algorithm="b", score=20) })))
})

test_that("cluster raises error without clusterer", {
    expect_error(cluster(), "No clusterer given!")
})

test_that("cluster raises error without train/test split", {
    expect_error(cluster(function() { return(NULL) }), "Need data with train/test split!")
})

test_that("cluster takes list of clusterers", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"))
    res = cluster(list(testclusterer, testclusterer, testclusterer), d)
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm="b", score=30) }) })))
    expect_true(all(sapply(res$predictor(fold), function(x) { x == data.frame(algorithm="b", score=60) })))
})

testclassifier = setClass("testclassifier", representation(data="list"), contains="formula", where=globalenv())
predict.testclassifier = function(object, y) { return(rep.int("b", nrow(y))) } 
setMethod("predict", signature(object="testclassifier"), predict.testclassifier, where=globalenv())
test_that("cluster takes list of clusterers and combinator", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"))
    res = cluster(list(testclusterer, testclusterer, testclusterer, .combine=testclassifier), d)
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm="b", score=1) }) })))
    expect_true(all(sapply(res$predictor(fold), function(x) { x == data.frame(algorithm="b", score=1) })))
})

