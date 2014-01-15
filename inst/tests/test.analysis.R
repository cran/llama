test_that("contributions returns contributions", {
    d = list(data=data.frame(a=c(1,2,3), b=c(4,2,1)), performance=c("a", "b"))
    cs = contributions(d)
    expect_equal(cs[[1]], 1.5)
    expect_equal(cs[[2]], 2.5)
})

test_that("contributions raises error if no data given", {
    expect_error(contributions(), "Need data to determine contributions!")
})
