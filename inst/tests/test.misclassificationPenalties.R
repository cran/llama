test_that("misclassificationPenalties returns penalties", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5))
    d = list(test=list(fold, fold), performance=c("a", "b"), minimize=T)
    as = rep.int(list(data.frame(algorithm="a", score=1)), 5)
    bs = rep.int(list(data.frame(algorithm="b", score=1)), 5)
    modela = list(predictions=list(as, as))
    modelb = list(predictions=list(bs, bs))

    expect_equal(sum(misclassificationPenalties(d, modela)), 10)
    expect_equal(sum(misclassificationPenalties(d, modelb)), 0)
})

test_that("misclassificationPenalties works without test split", {
    fold = data.frame(a=rep.int(1, 10), b=rep.int(0, 10))
    d = list(data=fold, performance=c("a", "b"), minimize=T)
    modela = list(predictions=rep.int(list(data.frame(algorithm="a", score=1)), 10))
    modelb = list(predictions=rep.int(list(data.frame(algorithm="b", score=1)), 10))

    expect_equal(sum(misclassificationPenalties(d, modela)), 10)
    expect_equal(sum(misclassificationPenalties(d, modelb)), 0)
})

test_that("misclassificationPenalties allows to maximise", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5))
    d = list(test=list(fold, fold), performance=c("a", "b"), minimize=F)
    as = rep.int(list(data.frame(algorithm="a", score=1)), 5)
    bs = rep.int(list(data.frame(algorithm="b", score=1)), 5)
    modela = list(predictions=list(as, as))
    modelb = list(predictions=list(bs, bs))

    expect_equal(sum(misclassificationPenalties(d, modela)), 0)
    expect_equal(sum(misclassificationPenalties(d, modelb)), 10)
})
