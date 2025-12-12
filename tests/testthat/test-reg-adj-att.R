test_that("reg_adj_att ATT matches DRDID; SE may differ", {
    skip_on_cran()
    data <- mtcars
    res_pkg <- reg_adj_att(mpg ~ am, xformula = ~ hp + wt, data = data)
    # DRDID baseline
    mf <- model.frame(mpg ~ am, data = data)
    Y <- model.response(mf)
    D <- as.numeric(mf[, 2])
    X <- model.matrix(~ hp + wt, data = data)
    res_drdid <- DRDID::reg_did_panel(y1 = Y, y0 = rep(0, length(Y)), D = D, covariates = X, boot = FALSE, inffunc = TRUE)
    expect_equal(res_pkg$ATT, res_drdid$ATT, tolerance = 1e-8)
    # SE can differ due to accounting for control OLS estimation uncertainty
    expect_true(is.finite(res_pkg$se) && res_pkg$se > 0)
})
