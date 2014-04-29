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

test_that("cluster finds best for cluster", {
    fold = data.frame(a=rep.int(0, 10), b=rep.int(1, 10), c=rep.int(0, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("b", "c"), minimize=T)
    res = cluster(testclusterer, d)
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm=c("c", "b"), score=c(0, 10)) }) })))
})

test_that("cluster finds best by count for cluster", {
    fold = data.frame(a=rep.int(0, 10), b=rep.int(1, 10), c=rep.int(0, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("b", "c"), minimize=T)
    res = cluster(testclusterer, d, bestBy="count")
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm="b", score=10) }) })))
})

test_that("cluster finds best by successes for cluster", {
    fold = data.frame(a=rep.int(0, 10), b=rep.int(1, 10), c=rep.int(0, 10), best=rep.int("b", 10), d=rep.int(T, 10), e=rep.int(F, 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("b", "c"), success=c("d", "e"), minimize=T)
    res = cluster(testclusterer, d, bestBy="successes")
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm=c("b", "c"), score=c(10, 0)) }) })))
})

test_that("cluster returns predictor", {
    fold = data.frame(a=rep.int(0, 10), b=rep.int(1, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("b"), minimize=T)
    res = cluster(testclusterer, d)
    expect_true(all(sapply(res$predictor(fold), function(x) { x == data.frame(algorithm="b", score=20) })))
})

test_that("cluster raises error without clusterer", {
    expect_error(cluster(), "No clusterer given!")
})

test_that("cluster raises error without train/test split", {
    expect_error(cluster(function() { return(NULL) }), "Need data with train/test split!")
})

test_that("cluster raises error with unknown bestBy", {
    fold = data.frame(a=rep.int(0, 10), b=rep.int(1, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("b"), minimize=T)
    expect_error(cluster(testclusterer, d, bestBy="foo"), "Unknown bestBy: foo")
})

test_that("cluster takes list of clusterers", {
    fold = data.frame(a=rep.int(0, 10), b=rep.int(1, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("b"), minimize=T)
    res = cluster(list(testclusterer, testclusterer, testclusterer), d)
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm="b", score=30) }) })))
    expect_true(all(sapply(res$predictor(fold), function(x) { x == data.frame(algorithm="b", score=60) })))
})

testclassifier = setClass("testclassifier", representation(data="list"), contains="formula", where=globalenv())
predict.testclassifier = function(object, y) { return(rep.int("b", nrow(y))) } 
setMethod("predict", signature(object="testclassifier"), predict.testclassifier, where=globalenv())
test_that("cluster takes list of clusterers and combinator", {
    fold = data.frame(a=rep.int(0, 10), b=rep.int(1, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"), performance=c("b"), minimize=T)
    res = cluster(list(testclusterer, testclusterer, testclusterer, .combine=testclassifier), d)
    expect_true(all(sapply(1:length(res$predictions), function(i) { sapply(res$predictions[[i]], function(x) { x == data.frame(algorithm="b", score=1) }) })))
    expect_true(all(sapply(res$predictor(fold), function(x) { x == data.frame(algorithm="b", score=1) })))
})
