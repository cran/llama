test_that("input reads features and performances", {
    a = data.frame(a=c(1:5), b=rep.int(1, 5))
    b = data.frame(a=c(1:5), c=rep.int(1, 5))

    data = input(a, b)
    expect_equal(data$features, c("b"))
    expect_equal(data$performance, c("c"))
    expect_equal(data$success, c())
    expect_equal(dim(data$data), c(5, 4))
    expect_equal(data$data$best, factor(rep.int("c", 5)))
})

test_that("input determines best", {
    a = data.frame(a=c(1:5), b=rep.int(1, 5))
    b = data.frame(a=c(1:5), c=rep.int(1, 5), d=rep.int(2, 5))

    data = input(a, b)
    expect_equal(data$features, c("b"))
    expect_equal(data$performance, c("c", "d"))
    expect_equal(data$data$best, factor(rep.int("c", 5)))
})

test_that("input determines best with max", {
    a = data.frame(a=c(1:5), b=rep.int(1, 5))
    b = data.frame(a=c(1:5), c=rep.int(1, 5), d=rep.int(2, 5))

    data = input(a, b, minimize=F)
    expect_equal(data$features, c("b"))
    expect_equal(data$performance, c("c", "d"))
    expect_equal(data$data$best, factor(rep.int("d", 5)))
})

test_that("input determines best and reports all ties", {
    a = data.frame(a=c(1:5), b=rep.int(1, 5))
    b = data.frame(a=c(1:5), c=c(2,2,3,4,5), d=c(1,2,3,4,5))

    data = input(a, b)
    expect_equal(data$features, c("b"))
    expect_equal(data$performance, c("c", "d"))
    expectedBest = list(factor("d"), factor(c("c", "d")), factor(c("c", "d")), factor(c("c", "d")), factor(c("c", "d")))
    expect_equal(data$data$best, expectedBest)
})

test_that("input reads features, performances and successes", {
    a = data.frame(a=c(1:5), b=rep.int(1, 5))
    b = data.frame(a=c(1:5), c=rep.int(1, 5))

    data = input(a, b, b)
    expect_equal(data$features, c("b"))
    expect_equal(data$performance, c("c"))
    expect_equal(data$success, c("c_success"))
    expect_equal(dim(data$data), c(5, 5))
    expect_equal(data$data$best, factor(rep.int("c", 5)))
})

test_that("input orders successes by performances", {
    a = data.frame(a=c(1:5), b=rep.int(1, 5))
    b = data.frame(a=c(1:5), c=rep.int(1, 5), d=rep.int(2, 5))
    c = data.frame(a=c(1:5), d=rep.int(F, 5), c=rep.int(T, 5))

    data = input(a, b, c)
    expect_equal(data$features, c("b"))
    expect_equal(data$performance, c("c", "d"))
    expect_equal(data$success, c("c_success", "d_success"))
    expect_equal(dim(data$data), c(5, 7))
    expect_equal(data$data$best, factor(rep.int("c", 5)))
})

test_that("input allows to specify single cost for all", {
    a = data.frame(a=c(1:5), b=rep.int(1, 5))
    b = data.frame(a=c(1:5), c=rep.int(1, 5))

    data = input(a, b, costs=3)
    expect_equal(data$features, c("b"))
    expect_equal(data$performance, c("c"))
    expect_equal(data$cost, c("cost"))
    expect_equal(data$data$cost, rep.int(3, 5))
})

test_that("input allows to specify costs", {
    a = data.frame(a=c(1:5), b=rep.int(1, 5))
    b = data.frame(a=c(1:5), c=rep.int(1, 5))

    data = input(a, b, costs=data.frame(a=c(1:5), b=c(1:5)))
    expect_equal(data$features, c("b"))
    expect_equal(data$performance, c("c"))
    expect_equal(data$cost, c("b_cost"))
    expect_equal(data$data$b_cost, c(1:5))
})

test_that("input allows to specify costs for groups", {
    a = data.frame(a=c(1:5), b=rep.int(1, 5), f=rep.int(1, 5))
    b = data.frame(a=c(1:5), c=rep.int(1, 5))

    groups = list(g1=c("b"), g2=c("f"))
    costs = list(groups=groups, values=data.frame(a=c(1:5), g1=c(1:5), g2=c(1:5)))

    data = input(a, b, costs=costs)
    expect_equal(data$features, c("b", "f"))
    expect_equal(data$performance, c("c"))
    expect_equal(data$costGroups, groups)
    expect_equal(data$cost, c("g1", "g2"))
    expect_equal(data$data$g1, c(1:5))
    expect_equal(data$data$g2, c(1:5))
})

test_that("input stops on invalid cost format", {
    a = data.frame(a=c(1:5), b=rep.int(1, 5))
    b = data.frame(a=c(1:5), c=rep.int(1, 5))

    expect_error(input(a, b, costs="foo"), "Invalid format for costs!")
})

test_that("input stops if features and costs disagree", {
    a = data.frame(a=c(1:5), b=rep.int(1, 5))
    b = data.frame(a=c(1:5), c=rep.int(1, 5))

    expect_error(input(a, b, costs=data.frame(a=c(1:5), f=c(1:5))), "Costs ")
})

test_that("input stops if features and cost groups disagree", {
    a = data.frame(a=c(1:5), b=rep.int(1, 5), g=rep.int(1, 5))
    b = data.frame(a=c(1:5), c=rep.int(1, 5))

    groups = list(g1=c("b"), g2=c("f"))
    costs = list(groups=groups, values=data.frame(a=c(1:5), g1=c(1:5), g2=c(1:5)))

    expect_error(input(a, b, costs=costs), "Cost groups ")
})

test_that("input stops if cost groups and specified costs disagree", {
    a = data.frame(a=c(1:5), b=rep.int(1, 5), f=rep.int(1, 5))
    b = data.frame(a=c(1:5), c=rep.int(1, 5))

    groups = list(g1=c("b"), g2=c("f"))
    costs = list(groups=groups, values=data.frame(a=c(1:5), g1=c(1:5)))

    expect_error(input(a, b, costs=costs), "Cost groups ")
})

test_that("input warns about differing number of rows", {
    a = data.frame(a=c(1:6), b=rep.int(1, 6))
    b = data.frame(a=c(1:5), c=rep.int(1, 5))

    expect_warning(input(a, b), "Different number of rows in data frames, taking only common rows.")
})

test_that("input errors when not being able to link", {
    a = data.frame(a=rep.int(1, 5))
    b = data.frame(b=rep.int(1, 5))

    expect_error(input(a, b), "Performance can't be linked to features -- no common columns!")
})

test_that("input errors with non-unique IDs", {
    a = data.frame(a=rep.int(1, 5), b=rep.int(1, 5))
    b = data.frame(a=rep.int(1, 5), c=rep.int(1, 5))

    expect_error(input(a, b), "Common columns do not provide unique IDs!")
})
