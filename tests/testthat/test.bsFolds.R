test_that("bsFolds splits", {
    d = list(data=data.frame(a=rep.int(0, 10)), best=rep.int(0, 10))
    class(d) = "llama.data"

    dfs = bsFolds(d)
    expect_equal(length(dfs$train), 10)
    expect_equal(length(dfs$test), 10)
    expect_true(all(length(sapply(1:10,
        function(x) { intersect(dfs$train[[x]], dfs$test[[x]]) }) == 0)))
    
    # same test with algorithm features
    d.algo = list(data=cbind(data.frame(p=rep.int(0, 10), f=rep.int(1, 10), algo=rep("a", 10)), 
                             id=c(1:10)),
                  algorithmFeatures=c("f"),
                  ids=c("id"),
                  algos=c("algo"),
                  best=rep.int("a", 10))
    class(d.algo) = "llama.data"
    
    dfs.algo = bsFolds(d.algo)
    expect_equal(length(dfs.algo$train), 10)
    expect_equal(length(dfs.algo$test), 10)
    expect_true(all(length(sapply(1:10,
                                  function(x) { intersect(dfs.algo$train[[x]], dfs.algo$test[[x]]) }) == 0)))
})

test_that("bsFolds splits with best list", {
    d = list(data=data.frame(a=rep.int(0, 10)))
    d$best = list(0, 0, 0, c(0,1), 0, 0, c(0,1), 0, 0, 0)
    class(d) = "llama.data"

    dfs = bsFolds(d, nfolds=2L)
    expect_equal(length(dfs$train), 2)
    expect_equal(length(dfs$test), 2)
    expect_true(all(length(sapply(1:2,
        function(x) { intersect(dfs$train[[x]], dfs$test[[x]]) }) == 0)))
    
    # same test with algorithm features
    d.algo = list(data=data.frame(p=rep.int(0, 10), f=rep.int(1, 10), algo=rep("a", 10), 
                             id=c(1:10)),
                  algorithmFeatures=c("f"),
                  ids=c("id"),
                  algos=c("algo"))
    d.algo$best = list(rep.int("a", 10))
    class(d.algo) = "llama.data"
    
    dfs.algo = bsFolds(d.algo, nfolds=2L)
    expect_equal(length(dfs.algo$train), 2)
    expect_equal(length(dfs.algo$test), 2)
    expect_true(all(length(sapply(1:2,
                                  function(x) { intersect(dfs.algo$train[[x]], dfs.algo$test[[x]]) }) == 0)))
})

test_that("bsFolds allows to specify number of folds", {
    d = list(data=data.frame(a=rep.int(0, 10)), best=rep.int(0, 10))
    class(d) = "llama.data"

    dfs = bsFolds(d, nfolds=2L)
    expect_equal(length(dfs$train), 2)
    expect_equal(length(dfs$test), 2)
    expect_true(all(length(sapply(1:2,
        function(x) { intersect(dfs$train[[x]], dfs$test[[x]]) }) == 0)))
    
    # same test with algorithm features
    d.algo = list(data=data.frame(p=rep.int(0, 10), f=rep.int(1, 10), algo=rep("a", 10), 
                                  id=c(1:10)), algorithmFeatures=c("f"),
                                  ids=c("id"), algos=c("algo"), best=rep.int("a", 10))
    class(d.algo) = "llama.data"
    
    dfs.algo = bsFolds(d.algo, nfolds=2L)
    expect_equal(length(dfs.algo$train), 2)
    expect_equal(length(dfs.algo$test), 2)
    expect_true(all(length(sapply(1:2,
                                  function(x) { intersect(dfs.algo$train[[x]], dfs.algo$test[[x]]) }) == 0)))
})

test_that("bsFolds stratifies", {
    d = list(data=data.frame(a=rep.int(0, 10)), best=c(rep.int(0, 5), rep.int(1, 5)))
    class(d) = "llama.data"

    dfs = bsFolds(d, nfolds=5L, T)
    expect_equal(length(dfs$train), 5)
    expect_equal(length(dfs$test), 5)
    expect_true(all(length(sapply(1:5,
        function(x) { intersect(dfs$train[[x]], dfs$test[[x]]) }) == 0)))

    # random, you know...
    #expect_true(all(sapply(dfs$train,
    #    function(x) { abs(sum(d$best[x]==0) - sum(d$best[x]==1)) < 1 })))
    #expect_true(all(sapply(dfs$test,
    #    function(x) { abs(sum(d$best[x]==0) - sum(d$best[x]==1)) < 1 })))
    
    # same test with algorithm features
    d.algo = list(data=cbind(data.frame(p=rep.int(0, 20), f=rep.int(1, 20), algo=rep(c("a1", "a2"), 10)), 
                             id=rep.int(1:10, rep.int(2, 10))),
                  algorithmFeatures=c("f"),
                  ids=c("id"),
                  algos=c("algo"),
                  best=c(rep.int("a1", 10), rep.int("a2", 10)))
    class(d.algo) = "llama.data"
    
    dfs.algo = bsFolds(d.algo, nfolds=5L, T)
    expect_equal(length(dfs.algo$train), 5)
    expect_equal(length(dfs.algo$test), 5)
    expect_true(all(length(sapply(1:5,
                                  function(x) { intersect(dfs.algo$train[[x]], dfs.algo$test[[x]]) }) == 0)))
})

test_that("bsFolds replaces existing splits", {
    d = list(data=data.frame(a=rep.int(0, 10)), best=rep.int(0, 10), train=1, test=2)
    class(d) = "llama.data"

    dfs = bsFolds(d)
    expect_equal(length(dfs$train), 10)
    expect_equal(length(dfs$test), 10)
    expect_true(all(length(sapply(1:10,
        function(x) { intersect(dfs$train[[x]], dfs$test[[x]]) }) == 0)))
    
    # same test with algorithm features
    d.algo = list(data=cbind(data.frame(p=rep.int(0, 20), f=rep.int(1, 20), algo=rep(c("a1", "a2"), 10)), 
                             id=rep.int(1:10, rep.int(2, 10))),
                            algorithmFeatures=c("f"), ids=c("id"), algos=c("algo"),
                            best=rep.int("a1", 20), train=1, test=2)
    class(d.algo) = "llama.data"
    
    dfs.algo = bsFolds(d.algo)
    expect_equal(length(dfs.algo$train), 10)
    expect_equal(length(dfs.algo$test), 10)
    expect_true(all(length(sapply(1:10,
                                  function(x) { intersect(dfs.algo$train[[x]], dfs.algo$test[[x]]) }) == 0)))
})

