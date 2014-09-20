test_that("successes returns successes", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5),
        d=rep.int(F, 5), e=rep.int(T, 5))
    d = list(data=rbind(fold, fold), test=list(fold, fold), performance=c("a", "b"), success=c("d", "e"))
    as = rep.int(list(data.frame(algorithm="a", score=1)), 5)
    bs = rep.int(list(data.frame(algorithm="b", score=1)), 5)
    modela = list(predictions=list(as, as))
    modelb = list(predictions=list(bs, bs))

    expect_equal(sum(successes(d, modela)), 0)
    expect_equal(sum(successes(d, modelb)), 10)
})

test_that("successes raises error if no successes are given", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5))
    d = list(test=list(fold, fold), performance=c("a", "b"))
    as = rep.int(list(data.frame(algorithm="a", score=1)), 5)
    modela = list(predictions=list(as, as))
    expect_error(successes(d, modela), "Need successes to calculate successes!")
})

test_that("successes works without test split", {
    fold = data.frame(a=rep.int(1, 10), b=rep.int(0, 10),
        d=rep.int(F, 10), e=rep.int(T, 10))
    d = list(data=fold, performance=c("a", "b"), success=c("d", "e"))
    modela = list(predictions=rep.int(list(data.frame(algorithm="a", score=1)), 10))
    modelb = list(predictions=rep.int(list(data.frame(algorithm="b", score=1)), 10))

    expect_equal(sum(successes(d, modela)), 0)
    expect_equal(sum(successes(d, modelb)), 10)
})

test_that("sucesses takes feature costs into account", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5),
        d=rep.int(T, 5), e=rep.int(T, 5), c=rep.int(1, 5), c_cost=rep.int(2, 5))
    d = list(data=rbind(fold, fold), test=list(fold, fold), performance=c("a", "b"), success=c("d", "e"), cost=c("c_cost"), features=c("c"))
    as = rep.int(list(data.frame(algorithm="a", score=1)), 5)
    bs = rep.int(list(data.frame(algorithm="b", score=1)), 5)
    modela = list(predictions=list(as, as))
    modelb = list(predictions=list(bs, bs))

    expect_equal(sum(successes(d, modela, timeout=1.5)), 0)
    expect_equal(sum(successes(d, modela, timeout=5)), 10)
    expect_equal(sum(successes(d, modelb, timeout=2.5)), 10)

    expect_equal(sum(successes(d, modela, timeout=1.5, addCosts=F)), 10)
    expect_equal(sum(successes(d, modela, timeout=5, addCosts=F)), 10)
    expect_equal(sum(successes(d, modelb, timeout=2.5, addCosts=F)), 10)
})

test_that("sucesses does not cost unused features", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5),
        d=rep.int(T, 5), e=rep.int(T, 5), c=rep.int(1, 5), c_cost=rep.int(2, 5), f_cost=rep.int(1, 5))
    d = list(data=rbind(fold, fold), test=list(fold, fold), performance=c("a", "b"), success=c("d", "e"), cost=c("c_cost", "f_cost"), features=c("c"))
    as = rep.int(list(data.frame(algorithm="a", score=1)), 5)
    modela = list(predictions=list(as, as))

    expect_equal(sum(successes(d, modela, timeout=1.5)), 0)
    expect_equal(sum(successes(d, modela, timeout=3.5)), 10)
})

test_that("successes takes feature cost groups into account", {
    fold = data.frame(a=rep.int(1, 5), e=rep.int(0, 5), f=rep.int(0, 5), g=rep.int(0, 5),
        d=rep.int(T, 5), g1=rep.int(1, 5), g2=rep.int(1, 5))
    groups = list(g1=c("e", "f"), g2=c("g"))
    d = list(data=rbind(fold, fold), test=list(fold, fold), performance=c("a"), success=c("d"), costGroups=groups, cost=c("g1", "g2"), features=c("f", "g"))
    e = d
    e$features = c("f")

    as = rep.int(list(data.frame(algorithm="a", score=1)), 5)
    modela = list(predictions=list(as, as))

    expect_equal(sum(successes(d, modela, timeout=1.5)), 0)
    expect_equal(sum(successes(d, modela, timeout=5)), 10)

    expect_equal(sum(successes(e, modela, timeout=1.5)), 0)
    expect_equal(sum(successes(e, modela, timeout=2.5)), 10)
})

test_that("successes works for test splits", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5),
        d=rep.int(F, 5), e=rep.int(T, 5))
    d = list(data=rbind(fold, fold), test=list(fold), performance=c("a", "b"), success=c("d", "e"))
    model = function(data) {
        lapply(1:nrow(data$data), function(i) { data.frame(algorithm="b", score=1) })
    }

    expect_equal(sum(successes(d, model)), 5)
})

test_that("successes works with NA predictions", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5),
        d=rep.int(F, 5), e=rep.int(T, 5))
    d = list(data=rbind(fold, fold), test=list(fold, fold), performance=c("a", "b"), success=c("d", "e"))
    nas = rep.int(list(data.frame(algorithm=NA, score=0)), 5)
    as = rep.int(list(data.frame(algorithm="b", score=1)), 5)
    model = list(predictions=list(nas, as))

    expect_equal(sum(successes(d, model), na.rm=T), 5)
})
