test_that("parscores returns parscores", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5),
        d=rep.int(F, 5), e=rep.int(T, 5))
    d = list(test=list(fold, fold), performance=c("a", "b"), success=c("d", "e"))
    as = rep.int("a", 5)
    bs = rep.int("b", 5)
    preda = list(as, as)
    predb = list(bs, bs)

    expect_true(all(sapply(parscores(d, preda), sum) == 50))
    expect_true(all(sapply(parscores(d, predb), sum) == 0))
})

test_that("parscores raises error if no successes are given", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5))
    d = list(test=list(fold, fold), performance=c("a", "b"))
    as = rep.int("a", 5)
    preda = list(as, as)
    expect_error(parscores(d, preda), "Need successes to compute PAR scores.")
})

test_that("parscores works without test split", {
    fold = data.frame(a=rep.int(1, 10), b=rep.int(0, 10),
        d=rep.int(F, 10), e=rep.int(T, 10))
    d = list(data=fold, performance=c("a", "b"), success=c("d", "e"))
    preda = rep.int("a", 10)
    predb = rep.int("b", 10)

    expect_equal(sum(parscores(d, preda)), 100)
    expect_equal(sum(parscores(d, predb)), 0)
})

test_that("parscores allows to specify factor", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5),
        d=rep.int(F, 5), e=rep.int(T, 5))
    d = list(test=list(fold, fold), performance=c("a", "b"), success=c("d", "e"))
    as = rep.int("a", 5)
    bs = rep.int("b", 5)
    preda = list(as, as)
    predb = list(bs, bs)

    expect_true(all(sapply(parscores(d, preda, 100), sum) == 500))
    expect_true(all(sapply(parscores(d, predb, 100), sum) == 0))
})

test_that("parscores allows to specify timeout", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5),
        d=rep.int(F, 5), e=rep.int(T, 5))
    d = list(test=list(fold, fold), performance=c("a", "b"), success=c("d", "e"))
    as = rep.int("a", 5)
    bs = rep.int("b", 5)
    preda = list(as, as)
    predb = list(bs, bs)

    expect_true(all(sapply(parscores(d, preda, timeout=5), sum) == 250))
    expect_true(all(sapply(parscores(d, predb, timeout=5), sum) == 0))
})
