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
