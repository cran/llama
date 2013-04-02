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
    res = cluster(clusterer=testclusterer, d)
    expect_true(all(unlist(res$predictions) == "b"))
})

test_that("cluster returns predictor", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"))
    res = cluster(clusterer=testclusterer, d)
    expect_true(all(res$predictor(fold) == "b"))
})

test_that("cluster raises error without clusterer", {
    expect_error(cluster(), "No clusterer given!")
})

test_that("cluster raises error without train/test split", {
    expect_error(cluster(function() { return(NULL) }), "Need data with train/test split!")
})

test_that("cluster takes list of predictors", {
    fold = data.frame(a=rep.int(0, 10), best=rep.int("b", 10))
    d = list(data=rbind(fold, fold), train=list(fold), test=list(fold), features=c("a"))
    res = cluster(clusterer=list(testclusterer, testclusterer, testclusterer), d)
    expect_true(all(res$predictor(fold) == "b"))
})
