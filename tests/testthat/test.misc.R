test_that("virtual best returns the vbs", {
    d = list(data=data.frame(id=1:5), best=bests, ids=c("id"))
    class(d) = "llama.data"

    preds = vbs(d)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor(bests[ss$id], levels=unique(bests)))
        expect_equal(ss$score, 1)
    })
    
    # same test with algorithm features
    d.algo = list(data=data.frame(id=rep.int(1:5, rep.int(2, 5)), a=1:10), best=bests, ids=c("id"), algorithmFeatures=c("a"))
    class(d.algo) = "llama.data"
    
    preds.algo = vbs(d.algo)
    by(preds.algo, preds.algo$id, function(ss) {
        expect_equal(ss$algorithm, factor(bests[ss$id], levels=unique(bests)))
        expect_equal(ss$score, 1)
    })
})

test_that("virtual best works with best list", {
    d = list(data=data.frame(id=1:3), best=bestlist, ids=c("id"))
    class(d) = "llama.data"

    preds = vbs(d)
    by(preds, preds$id, function(ss) {
        algs = bestlist[[ss$id[1]]]
        expect_equal(ss$algorithm, factor(algs, levels = c("a", "b")))
        expect_equal(ss$score, rep.int(1, length(algs)))
    })
    
    # same test with algorithm features
    d.algo = list(data=data.frame(id=rep.int(1:3, rep.int(2, 3)), a=1:6), best=bestlist, ids=c("id"), algorithmFeatures=c("a"))
    class(d.algo) = "llama.data"
    
    preds.algo = vbs(d.algo)
    by(preds.algo, preds.algo$id, function(ss) {
        algs = bestlist[[ss$id[1]]]
        expect_equal(ss$algorithm, factor(algs, levels = c("a", "b")))
        expect_equal(ss$score, rep.int(1, length(algs)))
    })
})

test_that("virtual best raises error without data", {
    expect_error(vbs(), "Need data to determine virtual best!")
})

test_that("single best raises error without data", {
    expect_error(singleBest(), "Need data to determine single best!")
})

test_that("virtual best works with NAs", {
    d = list(data=data.frame(id=1), best=c(NA), ids=c("id"))
    class(d) = "llama.data"

    preds = vbs(d)
    expect_equal(nrow(preds), 1)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor(NA))
        expect_equal(ss$score, 0)
    })
    
    # same test with algorithm features
    d.algo = list(data=data.frame(id=rep.int(1, 2), a=1:2), best=c(NA), ids=c("id"), algorithmFeatures=c("a"))
    class(d.algo) = "llama.data"
    
    preds.algo = vbs(d.algo)
    expect_equal(nrow(preds.algo), 1)
    by(preds.algo, preds.algo$id, function(ss) {
        expect_equal(ss$algorithm, factor(NA))
        expect_equal(ss$score, 0)
    })
})

test_that("single best returns the single best", {
    fold = data.frame(id=1:10, a=rep.int(1, 10), b=rep.int(0, 10))
    d = list(data=fold, performance=c("a", "b"), minimize=T, best=rep.int("a", 10), ids=c("id"))
    class(d) = "llama.data"
    e = list(data=fold, performance=c("a", "b"), minimize=F, best=rep.int("a", 10), ids=c("id"))
    class(e) = "llama.data"

    preds = singleBest(d)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor("b"))
        expect_equal(ss$score, 1)
    })
    preds = singleBest(e)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor("a"))
        expect_equal(ss$score, 1)
    })
    
    # same test with algorithm features
    fold.algo = data.frame(id=rep.int(c(1:10), rep.int(2, 10)), p=rep.int(c(1, 0), 10), f=1:20, a=rep.int(c("a", "b"), 10))
    d.algo = list(data=fold.algo, performance=c("p"), minimize=T, best=rep.int("a", 10), ids=c("id"), 
             algorithmFeatures=c("f"), algos=c("a"), algorithmNames=c("a", "b"))
    class(d.algo) = "llama.data"
    e.algo = list(data=fold.algo, performance=c("p"), minimize=F, best=rep.int("a", 10), ids=c("id"),
             algorithmFeatures=c("f"), algos=c("a"), algorithmNames=c("a", "b"))
    class(e.algo) = "llama.data"
    
    preds.algo = singleBest(d.algo)
    by(preds.algo, preds.algo$id, function(ss) {
        expect_equal(ss$algorithm, factor("b"))
        expect_equal(ss$score, 1)
    })
    preds.algo = singleBest(e.algo)
    by(preds.algo, preds.algo$id, function(ss) {
        expect_equal(ss$algorithm, factor("a"))
        expect_equal(ss$score, 1)
    })
})

test_that("single best works with best list", {
    fold = data.frame(id=1:10, a=rep.int(1, 10), b=rep.int(0, 10))
    d = list(data=fold, performance=c("a", "b"), minimize=T, ids=c("id"))
    d$best = bestlistlong
    class(d) = "llama.data"
    e = list(data=fold, performance=c("a", "b"), minimize=F, ids=c("id"))
    e$best = bestlistlong
    class(e) = "llama.data"

    preds = singleBest(d)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor("b"))
        expect_equal(ss$score, 1)
    })
    preds = singleBest(e)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor("a"))
        expect_equal(ss$score, 1)
    })
    
    # same test with algorithm features
    fold.algo = data.frame(id=rep.int(c(1:10), rep.int(2, 10)), p=rep.int(c(1, 0), 10), f=1:20, a=rep.int(c("a", "b"), 10))
    d.algo = list(data=fold.algo, performance=c("p"), minimize=T, ids=c("id"), algorithmFeatures=c("f"), algos=c("a"), algorithmNames=c("a", "b"))
    d.algo$best = bestlistlong
    class(d.algo) = "llama.data"
    e.algo = list(data=fold.algo, performance=c("p"), minimize=F, ids=c("id"), algorithmFeatures=c("f"), algos=c("a"), algorithmNames=c("a", "b"))
    e.algo$best = bestlistlong
    class(e.algo) = "llama.data"
    
    preds.algo = singleBest(d.algo)
    by(preds.algo, preds.algo$id, function(ss) {
        expect_equal(ss$algorithm, factor("b"))
        expect_equal(ss$score, 1)
    })
    preds.algo = singleBest(e.algo)
    by(preds.algo, preds.algo$id, function(ss) {
        expect_equal(ss$algorithm, factor("a"))
        expect_equal(ss$score, 1)
    })
})

test_that("single best by count returns the single best", {
    d = list(data=data.frame(id=1:3, a=c(1,2,3), b=c(2,2,3)),
             performance=c("a", "b"), minimize=T, ids=c("id"))
    d$best = rep.int("a", 3)
    class(d) = "llama.data"

    preds = singleBestByCount(d)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor("a"))
        expect_equal(ss$score, 1)
    })
    
    # same test with algorithm features
    d.algo = list(data=data.frame(id=rep.int(c(1:3), rep.int(2, 3)), p=c(1,2,2,2,3,3), a=rep.int(c("a", "b"), 3)),
             performance=c("p"), minimize=T, ids=c("id"), algorithmFeatures=c("f"), 
             algos=c("a"), algorithmNames=c("a", "b"))
    d.algo$best = rep.int("a", 3)
    class(d.algo) = "llama.data"
    
    preds.algo = singleBestByCount(d.algo)
    by(preds.algo, preds.algo$id, function(ss) {
        expect_equal(ss$algorithm, factor("a"))
        expect_equal(ss$score, 1)
    })
})

test_that("single best by count works with best list", {
    d = list(data=data.frame(id=1:3, a=c(1,2,3), b=c(2,2,3)),
             performance=c("a", "b"), minimize=T, ids=c("id"))
    d$best = bestlist
    class(d) = "llama.data"

    preds = singleBestByCount(d)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor("a"))
        expect_equal(ss$score, 1)
    })
    
    # same test with algorithm features
    d.algo = list(data=data.frame(id=rep.int(c(1:3), rep.int(2, 3)), p=c(1,2,2,2,3,3), a=rep.int(c("a", "b"), 3)),
                  performance=c("p"), minimize=T, ids=c("id"), algorithmFeatures=c("f"), 
                  algos=c("a"), algorithmNames=c("a", "b"))
    d.algo$best = bestlist
    class(d.algo) = "llama.data"
    
    preds.algo = singleBestByCount(d.algo)
    by(preds.algo, preds.algo$id, function(ss) {
        expect_equal(ss$algorithm, factor("a"))
        expect_equal(ss$score, 1)
    })
})

test_that("single best by par returns the single best", {
    fold = data.frame(id=1:10, a=rep.int(1, 10), b=rep.int(0, 10),
        d=rep.int(F, 10), e=rep.int(T, 10))
    d = list(data=fold, performance=c("a", "b"), success=c("d", "e"), best=rep.int("a", 10), ids=c("id"))
    class(d) = "llama.data"

    preds = singleBestByPar(d)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor("b"))
        expect_equal(ss$score, 1)
    })
    
    # same test with algorithm features
    fold.algo = data.frame(id=rep.int(c(1:10), rep.int(2, 10)), p=rep.int(c(1, 0), 10), f=1:20, a=rep.int(c("a", "b"), 10),
                           s=rep.int(c(F, T), 10))
    d.algo = list(data=fold.algo, performance=c("p"), success=c("s"), best=rep.int("a", 10), 
                  ids=c("id"), algorithmFeatures=c("f"), algos=c("a"), algorithmNames=c("a", "b"))
    class(d.algo) = "llama.data"
    
    preds.algo = singleBestByPar(d.algo)
    by(preds.algo, preds.algo$id, function(ss) {
        expect_equal(ss$algorithm, factor("b"))
        expect_equal(ss$score, 1)
    })
})

test_that("single best by par works with best list", {
    fold = data.frame(id=1:10, a=rep.int(1, 10), b=rep.int(0, 10),
        d=rep.int(F, 10), e=rep.int(T, 10))
    d = list(data=fold, performance=c("a", "b"), success=c("d", "e"), ids=c("id"))
    d$best = bestlistlong
    class(d) = "llama.data"

    preds = singleBestByPar(d)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor("b"))
        expect_equal(ss$score, 1)
    })
    
    # same test with algorithm features
    fold.algo = data.frame(id=rep.int(c(1:10), rep.int(2, 10)), p=rep.int(c(1, 0), 10), f=1:20, a=rep.int(c("a", "b"), 10),
                           s=rep.int(c(F, T), 10))
    d.algo = list(data=fold.algo, performance=c("p"), success=c("s"), 
                  ids=c("id"), algorithmFeatures=c("f"), algos=c("a"), algorithmNames=c("a", "b"))
    d.algo$best = bestlistlong
    class(d.algo) = "llama.data"
    
    preds.algo = singleBestByPar(d.algo)
    by(preds.algo, preds.algo$id, function(ss) {
        expect_equal(ss$algorithm, factor("b"))
        expect_equal(ss$score, 1)
    })
})

test_that("single best by par works with train/test split", {
    fold = data.frame(id=1:10, a=rep.int(1, 10), b=rep.int(0, 10),
        d=rep.int(F, 10), e=rep.int(T, 10))
    d = list(data=fold, test=list(1:5), train=list(6:10),
        performance=c("a", "b"), success=c("d", "e"), ids=c("id"))
    d$best = bestlistlong
    class(d) = "llama.data"

    preds = singleBestByPar(d)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor("b"))
        expect_equal(ss$score, 1)
    })
    
    # same test with algorithm features
    fold.algo = data.frame(id=rep.int(c(1:10), rep.int(2, 10)), p=rep.int(c(1, 0), 10), f=1:20, a=rep.int(c("a", "b"), 10),
                           s=rep.int(c(F, T), 10))
    d.algo = list(data=fold.algo, test=list(1:10), train=list(11:20), 
                  performance=c("p"), success=c("s"), ids=c("id"), algorithmFeatures=c("f"), algos=c("a"), algorithmNames=c("a", "b"))
    d.algo$best = bestlistlong
    class(d.algo) = "llama.data"
    
    preds.algo = singleBestByPar(d.algo)
    by(preds.algo, preds.algo$id, function(ss) {
        expect_equal(ss$algorithm, factor("b"))
        expect_equal(ss$score, 1)
    })
})

test_that("single best by par raises error without data", {
    expect_error(singleBestByPar(), "Need data to determine single best!")
})

test_that("single best by successes returns the single best", {
    fold = data.frame(id=1:10, a=rep.int(1, 10), b=rep.int(0, 10),
        d=rep.int(F, 10), e=rep.int(T, 10))
    d = list(data=fold, performance=c("a", "b"), success=c("d", "e"), best=rep.int("a", 10), ids=c("id"))
    class(d) = "llama.data"

    preds = singleBestBySuccesses(d)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor("b"))
        expect_equal(ss$score, 1)
    })
    
    # same test with algorithm features
    fold.algo = data.frame(id=rep.int(c(1:10), rep.int(2, 10)), p=rep.int(c(1, 0), 10), f=1:20, a=rep.int(c("a", "b"), 10),
                           s=rep.int(c(F, T), 10))
    d.algo = list(data=fold.algo, performance=c("p"), success=c("s"), best=rep.int("a", 10), 
                  ids=c("id"), algorithmFeatures=c("f"), algos=c("a"), algorithmNames=c("a", "b"))
    class(d.algo) = "llama.data"
    
    preds.algo = singleBestBySuccesses(d.algo)
    by(preds.algo, preds.algo$id, function(ss) {
        expect_equal(ss$algorithm, factor("b"))
        expect_equal(ss$score, 1)
    })
})

test_that("single best by successes works with best list", {
    fold = data.frame(id=1:10, a=rep.int(1, 10), b=rep.int(0, 10),
        d=rep.int(F, 10), e=rep.int(T, 10))
    d = list(data=fold, performance=c("a", "b"), success=c("d", "e"), ids=c("id"))
    d$best = bestlistlong
    class(d) = "llama.data"

    preds = singleBestBySuccesses(d)
    by(preds, preds$id, function(ss) {
        expect_equal(ss$algorithm, factor("b"))
        expect_equal(ss$score, 1)
    })
    
    # same test with algorithm features
    fold.algo = data.frame(id=rep.int(c(1:10), rep.int(2, 10)), p=rep.int(c(1, 0), 10), f=1:20, a=rep.int(c("a", "b"), 10),
                           s=rep.int(c(F, T), 10))
    d.algo = list(data=fold.algo, performance=c("p"), success=c("s"), 
                  ids=c("id"), algorithmFeatures=c("f"), algos=c("a"), algorithmNames=c("a", "b"))
    d.algo$best = bestlistlong
    class(d.algo) = "llama.data"
    
    preds.algo = singleBestBySuccesses(d.algo)
    by(preds.algo, preds.algo$id, function(ss) {
        expect_equal(ss$algorithm, factor("b"))
        expect_equal(ss$score, 1)
    })
})

test_that("single best by successes raises error without data", {
    expect_error(singleBestBySuccesses(), "Need data to determine single best!")
})

test_that("break best ties recomputes bests without ties", {
    d = list(data=data.frame(a=c(1,2,3), b=c(2,2,2.5)),
             performance=c("a", "b"), minimize=T)
    class(d) = "llama.data"

    expect_equal(breakBestTies(d), factor(c("a", "a", "b")))
    
    # same test with algorithm features
    d.algo = list(data=data.frame(p=c(1,2,2,2,3,2.5), a=rep.int(c("a", "b"), 3), f=1:6,
                                  id=rep.int(c(1:3), rep.int(2, 3))),
             performance=c("p"), algos=c("a"), algorithmFeatures=c("f"), ids=c("id"), algorithmNames=c("a", "b"), minimize=T)
    class(d.algo) = "llama.data"
    
    expect_equal(breakBestTies(d.algo), factor(c("a", "a", "a", "a", "b", "b")))
})

test_that("break best ties accepts fold argument", {
    fold = data=data.frame(a=c(1,2,3), b=c(2,2,2.5))
    d = list(data=rbind(fold, fold), performance=c("a", "b"), minimize=T,
        train=list(1:nrow(fold)))
    class(d) = "llama.data"

    expect_equal(breakBestTies(d), factor(c("a", "a", "b", "a", "a", "b")))
    expect_equal(breakBestTies(d, 1), factor(c("a", "a", "b")))
    
    # same test with algorithm features
    fold.algo = data.frame(p=c(1,2,2,2,3,2.5), a=rep.int(c("a", "b"), 3), f=1:6,
               id=rep.int(c(1:3), rep.int(2, 3)))
    d.algo = list(data=rbind(fold.algo, fold.algo), performance=c("p"), algos=c("a"), 
                  algorithmFeatures=c("f"), ids=c("id"), minimize=T, algorithmNames=c("a", "b"),
                  train=list(1:nrow(fold.algo)))
    class(d.algo) = "llama.data"
    
    expect_equal(breakBestTies(d.algo), factor(c("a", "a", "a", "a", "b", "b", "a", "a", "a", "a", "b", "b")))
    expect_equal(breakBestTies(d.algo, 1), factor(c("a", "a", "a", "a", "b", "b")))
})

test_that("predTable tabulates", {
    d = list(data=data.frame(id=1:3), best=bestlist, ids=c("id"))
    class(d) = "llama.data"
    preds = vbs(d)

    tab1 = predTable(preds)
    expect_equal(as.vector(tab1), c(2, 1))
    expect_equal(names(tab1), c("a", "b"))

    tab2 = predTable(preds, FALSE)
    expect_equal(as.vector(tab2), c(2, 2))
    expect_equal(names(tab2), c("a", "b"))

    preds = data.frame(id=1:3, algorithm=factor(c("a", "a", "b")), score = 1, iteration=1)
    tab3 = predTable(preds)
    expect_equal(as.vector(tab3), c(2, 1))
    expect_equal(names(tab3), c("a", "b"))

    preds = data.frame(id=c(1, 2, 3, 3), algorithm=factor(c("a", "a", "a", "b")), score = 1, iteration=1)
    tab4 = predTable(preds)
    expect_equal(as.vector(tab4), 3)
    expect_equal(names(tab4), "a")
    tab5 = predTable(preds, FALSE)
    expect_equal(as.vector(tab5), c(3, 1))
    expect_equal(names(tab5), c("a", "b"))

    i = 0
    model = function(data) {
        i <<- i + 1
        data.frame(id=data$data$id, algorithm=factor("a"), score=1, iteration=i)
    }
    class(model) = "llama.model"
    attr(model, "hasPredictions") = FALSE
    attr(model, "addCosts") = TRUE

    d = dmeas
    d$test = list(c(1,2), c(1,2), c(1,2))
    preds = do.call(rbind, lapply(d$test, function(x) {
        d$data = d$data[x,]
        d$best = d$best[x]
        model(d)
    }))
    tab6 = predTable(preds)
    expect_equal(as.vector(tab6), 6)
    expect_equal(names(tab6), "a")
    
    # same test with algorithm features
    d.algo = list(data=data.frame(id=rep.int(1:3, rep.int(2, 3)), a=1:6), best=bestlist, ids=c("id"), algorithmFeatures=c("a"))
    class(d.algo) = "llama.data"
    preds.algo = vbs(d.algo)
    
    tab1.algo = predTable(preds.algo)
    expect_equal(as.vector(tab1.algo), c(2, 1))
    expect_equal(names(tab1.algo), c("a", "b"))
    
    tab2.algo = predTable(preds.algo, FALSE)
    expect_equal(as.vector(tab2.algo), c(2, 2))
    expect_equal(names(tab2.algo), c("a", "b"))
    
    d.algo = dmeas.algo
    d.algo$test = list(c(1,2,3,4), c(1,2,3,4), c(1,2,3,4))
    preds.algo = do.call(rbind, lapply(d.algo$test, function(x) {
        d.algo$data = d.algo$data[x,]
        d.algo$best = d.algo$best[x]
        model(d.algo)
    }))
    tab6.algo = predTable(preds.algo)
    expect_equal(as.vector(tab6.algo), 6)
    expect_equal(names(tab6.algo), "a")
})

