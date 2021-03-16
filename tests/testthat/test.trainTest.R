test_that("trainTest splits", {
    d = list(data=data.frame(a=rep.int(0, 10)), best=rep.int(0, 10))
    class(d) = "llama.data"

    dtt = trainTest(d)
    expect_equal(length(dtt$train), 1)
    expect_equal(length(dtt$test), 1)
    expect_equal(length(dtt$train[[1]]), 6)
    expect_equal(length(dtt$test[[1]]), 4)
    
    # same test with algorithm features
    d.algo = list(data=cbind(data.frame(p=rep.int(0, 20), f=rep.int(1, 20), algo=rep(c("a1", "a2"), 10)), 
                             id=rep.int(1:10, rep.int(2, 10))),
                  algorithmFeatures=c("f"),
                  ids=c("id"),
                  algos=c("algo"),
                  algorithmNames=c("a1", "a2"), 
                  best=rep.int("a1", 20))
    class(d.algo) = "llama.data"
    
    dtt.algo = trainTest(d.algo)
    expect_equal(length(dtt.algo$train), 1)
    expect_equal(length(dtt.algo$test), 1)
    expect_equal(length(dtt.algo$train[[1]]), 12)
    expect_equal(length(dtt.algo$test[[1]]), 8)
})

test_that("trainTest splits with best list", {
    d = list(data=data.frame(a=rep.int(0, 10)))
    d$best = list(0, 0, 0, c(0, 1), 0, 0, 0, 0, 0, 0)
    class(d) = "llama.data"

    dtt = trainTest(d)
    expect_equal(length(dtt$train), 1)
    expect_equal(length(dtt$test), 1)
    expect_equal(length(dtt$train[[1]]), 6)
    expect_equal(length(dtt$test[[1]]), 4)
    
    # same test with algorithm features
    d.algo = list(data=cbind(data.frame(p=rep.int(0, 20), f=rep.int(1, 20), algo=rep(c("a1", "a2"), 10)), 
                             id=rep.int(1:10, rep.int(2, 10))),
                  algorithmFeatures=c("f"),
                  algorithmNames=c("a1", "a2"),
                  ids=c("id"),
                  algos=c("algo"),
                  best=rep.int("a1", 20))
    class(d.algo) = "llama.data"
    
    dtt.algo = trainTest(d.algo)
    expect_equal(length(dtt.algo$train), 1)
    expect_equal(length(dtt.algo$test), 1)
    expect_equal(length(dtt.algo$train[[1]]), 12)
    expect_equal(length(dtt.algo$test[[1]]), 8)
})

test_that("trainTest allows to specify split ratio", {
    d = list(data=data.frame(a=rep.int(0, 10)), best=rep.int(0, 10))
    class(d) = "llama.data"

    dtt = trainTest(d, trainpart=0.1)
    expect_equal(length(dtt$train), 1)
    expect_equal(length(dtt$test), 1)
    expect_equal(length(dtt$train[[1]]), 1)
    expect_equal(length(dtt$test[[1]]), 9)
    
    # same test with algorithm features
    d.algo = list(data=cbind(data.frame(p=rep.int(0, 20), f=rep.int(1, 20), algo=rep(c("a1", "a2"), 10)), 
                             id=rep.int(1:10, rep.int(2, 10))),
                  algorithmFeatures=c("f"),
                  algorithmNames=c("a1", "a2"),
                  ids=c("id"),
                  algos=c("algo"),
                  best=rep.int("a1", 20))
    class(d.algo) = "llama.data"
    
    dtt.algo = trainTest(d.algo, trainpart=0.1)
    expect_equal(length(dtt.algo$train), 1)
    expect_equal(length(dtt.algo$test), 1)
    expect_equal(length(dtt.algo$train[[1]]), 2)
    expect_equal(length(dtt.algo$test[[1]]), 18)
})

test_that("trainTest stratifies", {
    d = list(data=data.frame(a=rep.int(0, 10)), best=c(rep.int(0, 5), rep.int(1, 5)))
    class(d) = "llama.data"

    dtt = trainTest(d, stratify = T)
    expect_equal(length(dtt$train), 1)
    expect_equal(length(dtt$test), 1)
    expect_equal(length(dtt$train[[1]]), 6)
    expect_equal(length(dtt$test[[1]]), 4)

    expect_equal(sum(d$best[dtt$train[[1]]]==0), 3)
    expect_equal(sum(d$best[dtt$train[[1]]]==1), 3)
    expect_equal(sum(d$best[dtt$test[[1]]]==0), 2)
    expect_equal(sum(d$best[dtt$test[[1]]]==1), 2)    
    
    # same test with algorithm features
    d.algo = list(data=cbind(data.frame(p=rep.int(0, 20), f=rep.int(1, 20), algo=rep(c("a1", "a2"), 10)), 
                             id=rep.int(1:10, rep.int(2, 10))), algorithmFeatures=c("f"), ids=c("id"),
                             algorithmNames=c("a1", "a2"), algos=c("algo"), best=c(rep.int(0, 10), rep.int(1, 10)))
    class(d.algo) = "llama.data"
    
    dtt.algo = trainTest(d.algo, stratify = T)
    expect_equal(length(dtt.algo$train), 1)
    expect_equal(length(dtt.algo$test), 1)
    expect_equal(length(dtt.algo$train[[1]]), 12)
    expect_equal(length(dtt.algo$test[[1]]), 8)
    
    expect_equal(sum(d.algo$best[dtt.algo$train[[1]]]==0), 6)
    expect_equal(sum(d.algo$best[dtt.algo$train[[1]]]==1), 6)
    expect_equal(sum(d.algo$best[dtt.algo$test[[1]]]==0), 4)
    expect_equal(sum(d.algo$best[dtt.algo$test[[1]]]==1), 4)    
})

test_that("trainTest replaces existing splits", {
    d = list(data=data.frame(a=rep.int(0, 10)), best=rep.int(0, 10), train=c(1,1), test=c(1,1))
    class(d) = "llama.data"

    dtt = trainTest(d)
    expect_equal(length(dtt$train), 1)
    expect_equal(length(dtt$test), 1)
    expect_equal(length(dtt$train[[1]]), 6)
    expect_equal(length(dtt$test[[1]]), 4)
    
    # same test with algorithm features
    d.algo = list(data=cbind(data.frame(p=rep.int(0, 20), f=rep.int(1, 20), algo=rep(c("a1", "a2"), 10)), 
                             id=rep.int(1:10, rep.int(2, 10))), algorithmFeatures=c("f"), ids=c("id"),
                             algorithmNames=c("a1", "a2"), algos=c("algo"), best=rep.int("a1", 20),
                             train=c(1,1), test=c(1,1))
    class(d.algo) = "llama.data"
    
    dtt.algo = trainTest(d.algo)
    expect_equal(length(dtt.algo$train), 1)
    expect_equal(length(dtt.algo$test), 1)
    expect_equal(length(dtt.algo$train[[1]]), 12)
    expect_equal(length(dtt.algo$test[[1]]), 8)
})

test_that("trainTest with algorithm features includes all algorithms for each instance", {
    d.algo = list(data=cbind(data.frame(p=rep.int(0, 20), f=rep.int(1, 20), algo=rep(c("a1", "a2"), 10)), 
                             id=rep.int(1:10, rep.int(2, 10))), algorithmFeatures=c("f"), ids=c("id"),
                             algorithmNames=c("a1", "a2"), algos=c("algo"))
    class(d.algo) = "llama.data"
    
    dtt.algo = trainTest(d.algo)
    by(dtt.algo$data[dtt.algo$train[[1]], ], dtt.algo$data[dtt.algo$train[[1]], d.algo$ids], function(ss) {
        expect_equal(ss$algo, c("a1", "a2"))
    })
    
    by(dtt.algo$data[dtt.algo$test[[1]], ], dtt.algo$data[dtt.algo$test[[1]], d.algo$ids], function(ss) {
        expect_equal(ss$algo, c("a1", "a2"))
    })
})

