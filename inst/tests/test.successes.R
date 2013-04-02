test_that("successes returns successes", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5),
        d=rep.int(F, 5), e=rep.int(T, 5))
    d = list(test=list(fold, fold), performance=c("a", "b"), success=c("d", "e"))
    as = rep.int("a", 5)
    bs = rep.int("b", 5)
    preda = list(as, as)
    predb = list(bs, bs)

    expect_true(all(sapply(successes(d, preda), sum) == 0))
    expect_true(all(sapply(successes(d, predb), sum) == 5))
})

test_that("successes raises error if no successes are given", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5))
    d = list(test=list(fold, fold), performance=c("a", "b"))
    as = rep.int("a", 5)
    preda = list(as, as)
    expect_error(successes(d, preda), "Need successes to compute successes.")
})

test_that("successes works without test split", {
    fold = data.frame(a=rep.int(1, 10), b=rep.int(0, 10),
        d=rep.int(F, 10), e=rep.int(T, 10))
    d = list(data=fold, performance=c("a", "b"), success=c("d", "e"))
    preda = rep.int("a", 10)
    predb = rep.int("b", 10)

    expect_equal(sum(successes(d, preda)), 0)
    expect_equal(sum(successes(d, predb)), 10)
})
