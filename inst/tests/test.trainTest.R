test_that("trainTest splits", {
    d = list(data=data.frame(a=rep.int(0, 10), best=rep.int(0, 10)))

    dtt = trainTest(d)
    expect_equal(length(dtt$train), 1)
    expect_equal(length(dtt$test), 1)
    expect_equal(dim(dtt$train[[1]]), c(6, 2))
    expect_equal(dim(dtt$test[[1]]), c(4, 2))
})

test_that("trainTest allows to specify split ratio", {
    d = list(data=data.frame(a=rep.int(0, 10), best=rep.int(0, 10)))

    dtt = trainTest(d, trainpart=0.1)
    expect_equal(length(dtt$train), 1)
    expect_equal(length(dtt$test), 1)
    expect_equal(dim(dtt$train[[1]]), c(1, 2))
    expect_equal(dim(dtt$test[[1]]), c(9, 2))
})

test_that("trainTest stratifies", {
    d = list(data=data.frame(a=rep.int(0, 10), best=c(rep.int(0, 5), rep.int(1, 5))))

    dtt = trainTest(d)
    expect_equal(length(dtt$train), 1)
    expect_equal(length(dtt$test), 1)
    expect_equal(dim(dtt$train[[1]]), c(6, 2))
    expect_equal(dim(dtt$test[[1]]), c(4, 2))

    expect_equal(sum(dtt$train[[1]]$best==0), 3)
    expect_equal(sum(dtt$train[[1]]$best==1), 3)
    expect_equal(sum(dtt$test[[1]]$best==0), 2)
    expect_equal(sum(dtt$test[[1]]$best==1), 2)
})

# this one randomly fails...
#test_that("trainTest allows stratification to be turned off", {
#    d = list(data=data.frame(a=rep.int(0, 10), best=c(rep.int(0, 5), rep.int(1, 5))))
#
#    dtt = trainTest(d, stratify=F)
#    expect_equal(length(dtt$train), 1)
#    expect_equal(length(dtt$test), 1)
#    expect_equal(dim(dtt$train[[1]]), c(6, 2))
#    expect_equal(dim(dtt$test[[1]]), c(4, 2))
#
#    expect_false(sum(dtt$train[[1]]$best==0) == 3)
#    expect_false(sum(dtt$train[[1]]$best==1) == 3)
#    expect_false(sum(dtt$test[[1]]$best==0) == 2)
#    expect_false(sum(dtt$test[[1]]$best==1) == 2)
#})
