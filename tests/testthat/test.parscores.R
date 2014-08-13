test_that("parscores returns parscores", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5),
        d=rep.int(F, 5), e=rep.int(T, 5))
    d = list(data=rbind(fold, fold), test=list(fold, fold), performance=c("a", "b"), success=c("d", "e"))
    as = rep.int(list(data.frame(algorithm="a", score=1)), 5)
    bs = rep.int(list(data.frame(algorithm="b", score=1)), 5)
    modela = list(predictions=list(as, as))
    modelb = list(predictions=list(bs, bs))

    expect_equal(sum(parscores(d, modela)), 100)
    expect_equal(sum(parscores(d, modelb)), 0)
})

test_that("parscores raises error if no successes are given", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5))
    d = list(data=rbind(fold, fold), test=list(fold, fold), performance=c("a", "b"))
    as = rep.int(list(data.frame(algorithm="a", score=1)), 5)
    modela = list(predictions=list(as, as))
    expect_error(parscores(d, modela), "Need successes to calculate PAR scores!")
})

test_that("parscores works without test split", {
    fold = data.frame(a=rep.int(1, 10), b=rep.int(0, 10),
        d=rep.int(F, 10), e=rep.int(T, 10))
    d = list(data=fold, performance=c("a", "b"), success=c("d", "e"))
    modela = list(predictions=rep.int(list(data.frame(algorithm="a", score=1)), 10))
    modelb = list(predictions=rep.int(list(data.frame(algorithm="b", score=1)), 10))

    expect_equal(sum(parscores(d, modela)), 100)
    expect_equal(sum(parscores(d, modelb)), 0)
})

test_that("parscores allows to specify factor", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5),
        d=rep.int(F, 5), e=rep.int(T, 5))
    d = list(data=rbind(fold, fold), test=list(fold, fold), performance=c("a", "b"), success=c("d", "e"))
    as = rep.int(list(data.frame(algorithm="a", score=1)), 5)
    bs = rep.int(list(data.frame(algorithm="b", score=1)), 5)
    modela = list(predictions=list(as, as))
    modelb = list(predictions=list(bs, bs))

    expect_equal(sum(parscores(d, modela, 100)), 1000)
    expect_equal(sum(parscores(d, modelb, 100)), 0)
})

test_that("parscores allows to specify timeout", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5),
        d=rep.int(F, 5), e=rep.int(T, 5))
    d = list(data=rbind(fold, fold), test=list(fold, fold), performance=c("a", "b"), success=c("d", "e"))
    as = rep.int(list(data.frame(algorithm="a", score=1)), 5)
    bs = rep.int(list(data.frame(algorithm="b", score=1)), 5)
    modela = list(predictions=list(as, as))
    modelb = list(predictions=list(bs, bs))

    expect_equal(sum(parscores(d, modela, timeout=5)), 500)
    expect_equal(sum(parscores(d, modelb, timeout=5)), 0)
})

test_that("parscores takes feature costs into account", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5),
        d=rep.int(T, 5), e=rep.int(T, 5), c=rep.int(1, 5))
    d = list(data=rbind(fold, fold), test=list(fold, fold), performance=c("a", "b"), success=c("d", "e"), cost=c("c"), features=c("c"))
    as = rep.int(list(data.frame(algorithm="a", score=1)), 5)
    bs = rep.int(list(data.frame(algorithm="b", score=1)), 5)
    modela = list(predictions=list(as, as))
    modelb = list(predictions=list(bs, bs))

    expect_equal(sum(parscores(d, modela, timeout=1.5)), 150)
    expect_equal(sum(parscores(d, modela, timeout=5)), 20)
    expect_equal(sum(parscores(d, modelb, timeout=1.5)), 10)
})

test_that("parscores does not cost unused features", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5),
        d=rep.int(T, 5), e=rep.int(T, 5), c=rep.int(1, 5), f=rep.int(1, 5))
    d = list(data=rbind(fold, fold), test=list(fold, fold), performance=c("a", "b"), success=c("d", "e"), cost=c("c", "f"), features=c("c"))
    as = rep.int(list(data.frame(algorithm="a", score=1)), 5)
    modela = list(predictions=list(as, as))

    expect_equal(sum(parscores(d, modela, timeout=1.5)), 150)
    expect_equal(sum(parscores(d, modela, timeout=2.5)), 20)
})

test_that("parscores takes feature cost groups into account", {
    fold = data.frame(a=rep.int(1, 5), e=rep.int(0, 5), f=rep.int(0, 5), g=rep.int(0, 5),
        d=rep.int(T, 5), g1=rep.int(1, 5), g2=rep.int(1, 5))
    groups = list(g1=c("e", "f"), g2=c("g"))
    d = list(data=rbind(fold, fold), test=list(fold, fold), performance=c("a"), success=c("d"), costGroups=groups, cost=c("g1", "g2"), features=c("f", "g"))
    e = d
    e$features = c("f")

    as = rep.int(list(data.frame(algorithm="a", score=1)), 5)
    modela = list(predictions=list(as, as))

    expect_equal(sum(parscores(d, modela, timeout=2.5)), 250)
    expect_equal(sum(parscores(d, modela, timeout=1.5)), 150)
    expect_equal(sum(parscores(d, modela, timeout=5)), 30)

    expect_equal(sum(parscores(e, modela, timeout=1.5)), 150)
    expect_equal(sum(parscores(e, modela, timeout=2.5)), 20)
})

test_that("parscores works for test splits", {
    fold = data.frame(a=rep.int(1, 5), b=rep.int(0, 5),
        d=rep.int(F, 5), e=rep.int(T, 5))
    d = list(data=rbind(fold, fold), test=list(fold), performance=c("a", "b"), success=c("d", "e"))
    model = function(data) {
        lapply(1:nrow(data$data), function(i) { data.frame(algorithm="a", score=1) })
    }

    expect_equal(sum(parscores(d, model)), 50)
})
