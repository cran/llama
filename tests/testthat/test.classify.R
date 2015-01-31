test_that("classify classifies", {
    res = classify(classifier=testclassifier, d)
    expect_equal(unique(res$predictions$id), 11:20)
    by(res$predictions, res$predictions$id, function(ss) {
        expect_equal(ss$algorithm, factor(c("b", "c")))
        expect_equal(ss$score, c(1, 0))
    })
})

test_that("classify returns predictor", {
    res = classify(classifier=testclassifier, d)
    fold$id = 1:10
    preds = res$predictor(fold)
    expect_equal(unique(preds$id), 1:10)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor(c("b", "c")))
        expect_equal(ss$score, c(1, 0))
    })
})

test_that("classify returns predictor that works without IDs", {
    res = classify(classifier=testclassifier, d)
    fold$id = 1:10
    preds = res$predictor(fold[d$features])
    expect_equal(unique(preds$id), 1:10)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor(c("b", "c")))
        expect_equal(ss$score, c(1, 0))
    })
})

test_that("classify raises error without classifier", {
    expect_error(classify())
})

test_that("classify raises error without data", {
    expect_error(classify(testclassifier))
})

test_that("classify raises error without train/test split", {
    expect_error(classify(testclassifier, dnosplit), "Need data with train/test split!")
})

test_that("classify takes list of classifiers", {
    res = classify(classifier=list(testclassifier, testclassifier, testclassifier), d)
    expect_equal(unique(res$predictions$id), 11:20)
    by(res$predictions, res$predictions$id, function(ss) {
        expect_equal(ss$algorithm, factor(c("b", "c")))
        expect_equal(ss$score, c(3, 0))
    })
    
    fold$id = 1:10
    preds = res$predictor(fold)
    expect_equal(unique(preds$id), 1:10)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor(c("b", "c")))
        expect_equal(ss$score, c(3, 0))
    })
})

test_that("classify takes list of classifiers and combination function", {
    res = classify(classifier=list(foo, foo, foo, .combine=othertestclassifier), e)
    expect_equal(unique(res$predictions$id), 11:20)
    by(res$predictions, res$predictions$id, function(ss) {
        expect_equal(ss$algorithm, factor(c("a", "b")))
        expect_equal(ss$score, c(1, 0))
    })
    
    folde$id = 1:10
    preds = res$predictor(folde)
    expect_equal(unique(preds$id), 1:10)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor(c("a", "b")))
        expect_equal(ss$score, c(1, 0))
    })
})

test_that("classify ensemble does majority voting by default", {
    res = classify(classifier=list(testclassifier, othertestclassifier, othertestclassifier), e)
    expect_equal(unique(res$predictions$id), 11:20)
    by(res$predictions, res$predictions$id, function(ss) {
        expect_equal(ss$algorithm, factor(c("a", "b")))
        expect_equal(ss$score, c(2, 1))
    })

    folde$id = 1:10
    preds = res$predictor(folde)
    expect_equal(unique(preds$id), 1:10)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor(c("a", "b")))
        expect_equal(ss$score, c(2, 1))
    })
})

test_that("classify works with NA predictions", {
    res = classify(classifier=natestclassifier, d)
    expect_equal(unique(res$predictions$id), 11:20)
    by(res$predictions, res$predictions$id, function(ss) {
        expect_equal(ss$algorithm, NA)
        expect_equal(ss$score, -Inf)
    })
    fold$id = 1:10
    preds = res$predictor(fold)
    expect_equal(unique(preds$id), 1:10)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, NA)
        expect_equal(ss$score, -Inf)
    })

    res = classify(classifier=list(natestclassifier, natestclassifier, natestclassifier), d)
    expect_equal(unique(res$predictions$id), 11:20)
    by(res$predictions, res$predictions$id, function(ss) {
        expect_equal(ss$algorithm, NA)
        expect_equal(ss$score, -Inf)
    })
    fold$id = 1:10
    preds = res$predictor(fold)
    expect_equal(unique(preds$id), 1:10)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, NA)
        expect_equal(ss$score, -Inf)
    })

    res = classify(classifier=list(natestclassifier, natestclassifier, natestclassifier, .combine=natestclassifier), d)
    expect_equal(unique(res$predictions$id), 11:20)
    by(res$predictions, res$predictions$id, function(ss) {
        expect_equal(ss$algorithm, NA)
        expect_equal(ss$score, -Inf)
    })
    fold$id = 1:10
    preds = res$predictor(fold)
    expect_equal(unique(preds$id), 1:10)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, NA)
        expect_equal(ss$score, -Inf)
    })
})
