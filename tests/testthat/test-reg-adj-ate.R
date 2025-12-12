test_that("reg_adj_ate computes ATE and is sensible", {
    skip_on_cran()
    data <- mtcars
    res <- reg_adj_ate(mpg ~ am, xformula = ~ hp + wt, data = data)
    expect_true(is.list(res))
    expect_true(is.numeric(res$ATT))
    expect_true(is.numeric(res$se) && res$se > 0)
    expect_equal(length(res$att.inf.func), nrow(data))
})
