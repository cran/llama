bests = c("a", "a", "a", "b", "b")
bestlist = list("a", "b", c("a", "b"))
bestlistlong = list("a", "a", "b", c("a", "b"), "a", "a", c("a", "b"), "a", "a", "a")

test_that("virtual best returns the vbs", {
    d = list(data=list(best=bests))

    preds = vbs(d)
    expect_true(all(sapply(1:length(preds), function(i) {
        preds[[i]] == data.frame(algorithm=bests[i], score=1)
    })))
})

test_that("virtual best works with best list", {
    d = list(data=list(best=bestlist))

    preds = vbs(d)
    expect_true(all(sapply(1:length(preds), function(i) {
        algs = bestlist[[i]]
        df = data.frame(algorithm=algs, score=rep.int(1, length(algs)))
        all(preds[[i]] == df)
    })))
})

test_that("virtual best raises error without data", {
    expect_error(vbs(), "Need data to determine virtual best!")
})

test_that("single best raises error without data", {
    expect_error(singleBest(), "Need data to determine single best!")
})

test_that("virtual best works with NAs", {
    d = list(data=list(best=c(NA)))

    preds = vbs(d)
    expect_equal(length(preds), 1)
    expect_equal(preds[[1]], data.frame(algorithm=NA, score=0))
})

test_that("single best returns the single best", {
    fold = data.frame(a=rep.int(1, 10), b=rep.int(0, 10), best=rep.int("a", 10))
    d = list(data=fold, performance=c("a", "b"), minimize=T)
    e = list(data=fold, performance=c("a", "b"), minimize=F)

    preds = singleBest(d)
    expect_true(all(sapply(1:length(preds), function(i) {
        preds[[i]] == data.frame(algorithm="b", score=1)
    })))
    preds = singleBest(e)
    expect_true(all(sapply(1:length(preds), function(i) {
        preds[[i]] == data.frame(algorithm="a", score=1)
    })))
})

test_that("single best works with best list", {
    fold = data.frame(a=rep.int(1, 10), b=rep.int(0, 10))
    fold$best = bestlistlong
    d = list(data=fold, performance=c("a", "b"), minimize=T)
    e = list(data=fold, performance=c("a", "b"), minimize=F)

    preds = singleBest(d)
    expect_true(all(sapply(1:length(preds), function(i) {
        preds[[i]] == data.frame(algorithm="b", score=1)
    })))
    preds = singleBest(e)
    expect_true(all(sapply(1:length(preds), function(i) {
        preds[[i]] == data.frame(algorithm="a", score=1)
    })))
})

test_that("single best by count returns the single best", {
    d = list(data=data.frame(a=c(1,2,3), b=c(2,2,3)),
             performance=c("a", "b"), minimize=T)
    d$data$best = rep.int("a", 3)

    preds = singleBestByCount(d)
    expect_true(all(sapply(1:length(preds), function(i) {
        preds[[i]] == data.frame(algorithm="a", score=1)
    })))
})

test_that("single best by count works with best list", {
    d = list(data=data.frame(a=c(1,2,3), b=c(2,2,3)),
             performance=c("a", "b"), minimize=T)
    d$data$best = bestlist

    preds = singleBestByCount(d)
    expect_true(all(sapply(1:length(preds), function(i) {
        preds[[i]] == data.frame(algorithm="a", score=1)
    })))
})

test_that("single best by par returns the single best", {
    fold = data.frame(a=rep.int(1, 10), b=rep.int(0, 10),
        d=rep.int(F, 10), e=rep.int(T, 10), best=rep.int("a", 10))
    d = list(data=fold, performance=c("a", "b"), success=c("d", "e"))

    preds = singleBestByPar(d)
    expect_true(all(sapply(1:length(preds), function(i) {
        preds[[i]] == data.frame(algorithm="b", score=1)
    })))
})

test_that("single best by par works with best list", {
    fold = data.frame(a=rep.int(1, 10), b=rep.int(0, 10),
        d=rep.int(F, 10), e=rep.int(T, 10))
    fold$best = bestlistlong
    d = list(data=fold, performance=c("a", "b"), success=c("d", "e"))

    preds = singleBestByPar(d)
    expect_true(all(sapply(1:length(preds), function(i) {
        preds[[i]] == data.frame(algorithm="b", score=1)
    })))
})

test_that("single best by par raises error without data", {
    expect_error(singleBestByPar(), "Need data to determine single best!")
})

test_that("single best by successes returns the single best", {
    fold = data.frame(a=rep.int(1, 10), b=rep.int(0, 10),
        d=rep.int(F, 10), e=rep.int(T, 10), best=rep.int("a", 10))
    d = list(data=fold, performance=c("a", "b"), success=c("d", "e"))

    preds = singleBestBySuccesses(d)
    expect_true(all(sapply(1:length(preds), function(i) {
        preds[[i]] == data.frame(algorithm="b", score=1)
    })))
})

test_that("single best by successes works with best list", {
    fold = data.frame(a=rep.int(1, 10), b=rep.int(0, 10),
        d=rep.int(F, 10), e=rep.int(T, 10))
    fold$best = bestlistlong
    d = list(data=fold, performance=c("a", "b"), success=c("d", "e"))

    preds = singleBestBySuccesses(d)
    expect_true(all(sapply(1:length(preds), function(i) {
        preds[[i]] == data.frame(algorithm="b", score=1)
    })))
})

test_that("single best by successes raises error without data", {
    expect_error(singleBestBySuccesses(), "Need data to determine single best!")
})

test_that("break best ties recomputes bests without ties", {
    d = list(data=data.frame(a=c(1,2,3), b=c(2,2,2.5)),
             performance=c("a", "b"), minimize=T)

    expect_equal(breakBestTies(d), factor(c("a", "a", "b")))
})

test_that("break best ties accepts fold argument", {
    fold = data=data.frame(a=c(1,2,3), b=c(2,2,2.5))
    d = list(data=rbind(fold, fold), performance=c("a", "b"), minimize=T, train=list(fold))

    expect_equal(breakBestTies(d), factor(c("a", "a", "b", "a", "a", "b")))
    expect_equal(breakBestTies(d, 1), factor(c("a", "a", "b")))
})
