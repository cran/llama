test_that("featureFilter filters", {
    origfeat = c("a")
    filterfeat = c("b")
    d = list(data=data.frame(a=rep.int(1, 10), best=rep.int("a", 10)),
            performance=c("a"), minimize=T,
            features=origfeat)
    f = function(formula, data) { return(filterfeat) }

    df = featureFilter(f, d)
    expect_equal(df$features, filterfeat)
    expect_equal(df$original_features, origfeat)
})

test_that("featureFilter raises error if no filter function given", {
    expect_error(featureFilter(), "No filter function given!")
})

test_that("featureFilter raises error if no data given", {
    expect_error(featureFilter(function() { return(NULL) }), "No data given!")
})
