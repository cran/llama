test_that("misclassificationPenalties returns penalties", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5))
    d = list(test=list(fold, fold), performance=c("a", "b"))
    as = rep.int("a", 5)
    bs = rep.int("b", 5)
    preda = list(as, as)
    predb = list(bs, bs)

    expect_true(all(sapply(misclassificationPenalties(d, preda), sum) == 5))
    expect_true(all(sapply(misclassificationPenalties(d, predb), sum) == 0))
})

test_that("misclassificationPenalties works without test split", {
    fold = data.frame(a=rep.int(1, 10), b=rep.int(0, 10))
    d = list(data=fold, performance=c("a", "b"))
    preda = rep.int("a", 10)
    predb = rep.int("b", 10)

    expect_equal(sum(misclassificationPenalties(d, preda)), 10)
    expect_equal(sum(misclassificationPenalties(d, predb)), 0)
})

test_that("misclassificationPenalties allows to maximise", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5))
    d = list(test=list(fold, fold), performance=c("a", "b"))
    as = rep.int("a", 5)
    bs = rep.int("b", 5)
    preda = list(as, as)
    predb = list(bs, bs)

    expect_true(all(sapply(misclassificationPenalties(d, preda, F), sum) == 0))
    expect_true(all(sapply(misclassificationPenalties(d, predb, F), sum) == 5))
})
