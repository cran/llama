test_that("imputeCensored raises error without data", {
    expect_error(imputeCensored())
})

test_that("imputeCensored leaves successes alone", {
    data = data.frame(a=rep.int(0, 10), foo=rep.int(0, 10), bar=rep.int(T, 10))
    d = list(data=data, features=c("a"), performance=c("foo"), success=c("bar"), best=rep.int("b", 10))
    class(d) = "llama.data"
    res = imputeCensored(d, testregressor)
    expect_identical(res$data, res$original_data)
    
    # same test with algorithm features
    data.algo = data.frame(a=rep.int(0, 10), p=rep.int(0, 10), s=rep.int(T, 10), algo=rep.int("foo", 10), 
                           f=rep.int(1, 10))
    d.algo = list(data=data.algo, features=c("a"), performance=c("p"), success=c("s"), best=rep.int("b", 10), 
             algos=c("algo"), algorithmFeatures=c("f"))
    class(d.algo) = "llama.data"
    res.algo = imputeCensored(d.algo, algotestregressor)
    expect_identical(res.algo$data, res.algo$original_data)
})

test_that("imputeCensored fails with no non-censored values", {
    data = data.frame(a=rep.int(0, 10), foo=rep.int(0, 10), bar=rep.int(F, 10))
    d = list(data=data, features=c("a"), performance=c("foo"), success=c("bar"), best=rep.int("b", 10))
    class(d) = "llama.data"
    expect_error(imputeCensored(d, testregressor), "Cannot impute for  foo , no non-censored values!")
    
    # same test with algorithm features
    data.algo = data.frame(a=rep.int(0, 10), p=rep.int(0, 10), s=rep.int(F, 10), algo=rep.int("foo", 10), 
                           f=rep.int(1, 10))
    d.algo = list(data=data.algo, features=c("a"), performance=c("p"), success=c("s"), best=rep.int("b", 10), 
                  algos=c("algo"), algorithmFeatures=c("f"))
    class(d.algo) = "llama.data"
    expect_error(imputeCensored(d.algo, algotestregressor), "Cannot impute for  p , no non-censored values!")
})

test_that("imputeCensored makes everything successes", {
    data = data.frame(a=rep.int(0, 10), foo=rep.int(0, 10), bar=c(rep.int(T, 5), rep.int(F, 5)))
    d = list(data=data, features=c("a"), performance=c("foo"), success=c("bar"), best=rep.int("b", 10))
    class(d) = "llama.data"
    res = imputeCensored(d, testregressor)
    expect_false(all(res$original_data$bar))
    expect_true(all(res$data$bar))
    
    # same test with algorithm features
    data.algo = data.frame(a=rep.int(0, 10), p=rep.int(0, 10), s=c(rep.int(T, 5), rep.int(F, 5)), algo=rep.int("foo", 10), 
                           f=rep.int(1, 10))
    d.algo = list(data=data.algo, features=c("a"), performance=c("p"), success=c("s"), best=rep.int("b", 10), 
                  algos=c("algo"), algorithmFeatures=c("f"))
    class(d.algo) = "llama.data"
    res.algo = imputeCensored(d.algo, testregressor)
    expect_false(all(res.algo$original_data$s))
    expect_true(all(res.algo$data$s))
})

test_that("imputeCensored imputes non-successes", {
    data = data.frame(a=rep.int(0, 10), foo=rep.int(0, 10), bar=c(rep.int(T, 5), rep.int(F, 5)))
    d = list(data=data, features=c("a"), performance=c("foo"), success=c("bar"), best=rep.int("b", 10))
    class(d) = "llama.data"
    res = imputeCensored(d, footestregressor)
    expect_identical(res$data$foo[1:5], res$original_data$foo[1:5])
    expect_true(all(res$data$foo[6:10] == 1))
    
    # same test with algorithm features
    data.algo = data.frame(a=rep.int(0, 10), p=rep.int(0, 10), s=c(rep.int(T, 5), rep.int(F, 5)), algo=rep.int("foo", 10), 
                           f=rep.int(1, 10))
    d.algo = list(data=data.algo, features=c("a"), performance=c("p"), success=c("s"), best=rep.int("b", 10), 
                  algos=c("algo"), algorithmFeatures=c("f"))
    class(d.algo) = "llama.data"
    res.algo = imputeCensored(d.algo, footestregressor)
    expect_identical(res.algo$data$p[1:5], res.algo$original_data$p[1:5])
    expect_true(all(res.algo$data$p[6:10] == 1))
})

