bests = c("a", "a", "a", "b", "b")

test_that("virtual best returns the vbs", {
    d = list(data=list(best=bests))

    preds = vbs(d)
    expect_true(all(sapply(1:length(preds), function(i) {
        preds[[i]] == data.frame(algorithm=bests[i], score=1)
    })))
})

test_that("virtual best raises error without data", {
    expect_error(vbs(), "Need data to determine virtual best!")
})

test_that("single best returns the single best", {
    d = list(data=list(best=bests))

    preds = singleBest(d)
    expect_true(all(sapply(1:length(preds), function(i) {
        preds[[i]] == data.frame(algorithm="a", score=1)
    })))
})

test_that("single best raises error without data", {
    expect_error(singleBest(), "Need data to determine single best!")
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

test_that("single best by successes raises error without data", {
    expect_error(singleBestBySuccesses(), "Need data to determine single best!")
})
