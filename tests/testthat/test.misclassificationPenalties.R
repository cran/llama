test_that("misclassificationPenalties returns penalties", {
    expect_equal(sum(misclassificationPenalties(dmeas, modelameas)), 10)
    expect_equal(sum(misclassificationPenalties(dmeas, modelbmeas)), 0)
    
    # same test with algorithm features
    expect_equal(sum(misclassificationPenalties(dmeas.algo, modelameas.algo)), 10)
    expect_equal(sum(misclassificationPenalties(dmeas.algo, modelbmeas.algo)), 0)
})

test_that("misclassificationPenalties works without test split", {
    fold = data.frame(id=1:10, a=rep.int(1, 10), b=rep.int(0, 10))
    d = list(data=fold, performance=c("a", "b"), minimize=T, ids=c("id"))

    expect_equal(sum(misclassificationPenalties(d, modelameas)), 10)
    expect_equal(sum(misclassificationPenalties(d, modelbmeas)), 0)
    
    # same test with algorithm features    
    fold.algo = data.frame(id=rep(1:10, rep.int(2, 10)), p=rep(c(1, 0), 10),
                           f=rep(1, 20), a=rep(c("a", "b"), 10))
    d.algo = list(data=fold.algo, performance=c("p"), minimize=T, ids=c("id"), 
             algorithmFeatures=c("f"), algos=c("a"), algorithmNames=c("a", "b"))
    
    expect_equal(sum(misclassificationPenalties(d.algo, modelameas.algo)), 10)
    expect_equal(sum(misclassificationPenalties(d.algo, modelbmeas.algo)), 0)
})

test_that("misclassificationPenalties allows to maximise", {
    emeas = dmeas
    emeas$minimize = F

    expect_equal(sum(misclassificationPenalties(emeas, modelameas)), 0)
    expect_equal(sum(misclassificationPenalties(emeas, modelbmeas)), 10)
    
    # same test with algorithm features
    emeas.algo = dmeas.algo
    emeas.algo$minimize = F
    
    expect_equal(sum(misclassificationPenalties(emeas.algo, modelameas.algo)), 0)
    expect_equal(sum(misclassificationPenalties(emeas.algo, modelbmeas.algo)), 10)
})

test_that("misclassificationPenalties works for test splits", {
    model = function(data) {
        data.frame(id=data$data$id, algorithm="a", score=1, iteration=1)
    }
    class(model) = "llama.model"
    attr(model, "hasPredictions") = FALSE

    expect_equal(sum(misclassificationPenalties(dmeas, model)), 10)
    
    # same test with algorithm features
    expect_equal(sum(misclassificationPenalties(dmeas.algo, model)), 10)
})

test_that("misclassificationPenalties works for test splits that repeat", {
    i = 0
    model = function(data) {
        i <<- i + 1
        data.frame(id=data$data$id, algorithm="a", score=1, iteration=i)
    }
    class(model) = "llama.model"
    attr(model, "hasPredictions") = FALSE
    attr(model, "addCosts") = TRUE

    d = dmeas
    d$test = list(c(1,2), c(1,2), c(2,3))

    expect_equal(sum(misclassificationPenalties(d, model)), 6)
    
    # same test with algorithm features
    d.algo = dmeas.algo
    d.algo$test = list(c(1,2,3,4), c(1,2,3,4), c(3,4,5,6))
    
    expect_equal(sum(misclassificationPenalties(d.algo, model)), 6)
})

test_that("misclassificationPenalties works with NA predictions", {
    nasmeas = data.frame(algorithm=rep.int(NA, 5), score=0, iteration=1)
    asmeas = data.frame(algorithm=rep.int("a", 5), score=1, iteration=1)

    model = list(predictions=rbind(cbind(nasmeas, id=1:5), cbind(asmeas, id=6:10)))
    class(model) = "llama.model"
    attr(model, "hasPredictions") = TRUE

    meass = misclassificationPenalties(dmeas, model)
    expect_equal(length(meass), 10)
    expect_equal(length(meass[meass == 0]), 5)
    expect_equal(sum(meass), 5)
    
    # same test with algorithm features
    nasmeas.algo = data.frame(algorithm=rep.int(NA, 10), score=0, iteration=1)
    asmeas.algo = data.frame(algorithm=rep.int("a", 10), score=1, iteration=1)
    
    model.algo = list(predictions=rbind(cbind(nasmeas.algo, id=rep(1:5, rep(2, 5))), cbind(asmeas.algo, id=rep(6:10, rep(2, 5)))))
    class(model.algo) = "llama.model"
    attr(model.algo, "hasPredictions") = TRUE
    
    meass.algo = misclassificationPenalties(dmeas.algo, model.algo)
    expect_equal(length(meass.algo), 10)
    expect_equal(length(meass.algo[meass.algo == 0]), 5)
    expect_equal(sum(meass.algo), 5)
})

