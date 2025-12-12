#' Regression Adjustment Estimation for Binary Treatment Effects (ATE)
#'
#' Estimates the average treatment effect (ATE) using regression adjustment
#' under unconfoundedness. Outcome regression is estimated separately for the
#' treated and control groups, and counterfactual outcomes are predicted for
#' the full sample from each model. The ATE is the average difference between
#' these predicted potential outcomes.
#'
#' @param formula A formula specifying the outcome and treatment variables in the
#'   form \code{Y ~ D}, where \code{Y} is the outcome variable and \code{D} is
#'   the binary treatment indicator.
#' @param xformula An optional formula specifying the covariates for adjustment
#'   in the form \code{~X1 + X2 + ...}. If \code{NULL} (default), only an
#'   intercept is included.
#' @param data A data frame containing the variables in the model.
#'
#' @return An object of class \code{c("reg_adj", "drdid")} aligned with DRDID outputs.
#'
#' @importFrom stats model.frame model.matrix model.response na.omit qnorm var
#' @export
reg_adj_ate <- function(formula, xformula = NULL, data) {
    if (missing(data) || missing(formula)) stop("'formula' and 'data' are required")
    call <- match.call()
    mf <- model.frame(formula, data = data, na.action = na.omit)
    Y <- model.response(mf)
    if (ncol(mf) != 2) stop("formula must be Y ~ D")
    D_raw <- mf[, 2]
    if (is.logical(D_raw)) {
        D <- as.numeric(D_raw)
    } else if (is.factor(D_raw)) {
        if (nlevels(D_raw) != 2) stop("Treatment must have 2 levels")
        D <- as.numeric(D_raw) - 1
    } else {
        D <- as.numeric(D_raw)
    }
    unique_vals <- sort(unique(D))
    if (!all(unique_vals %in% c(0, 1)) || length(unique_vals) != 2) stop("Treatment must be binary")
    if (is.null(xformula)) {
        X <- model.matrix(~1, data = mf)
        covariate_names <- "(Intercept)"
    } else {
        xmf <- model.frame(xformula, data = data, na.action = na.omit)
        X <- model.matrix(xformula, data = xmf)
        covariate_names <- colnames(X)

        if (nrow(xmf) != nrow(mf)) {
            warning("Missing values in covariates resulted in different sample sizes. Using complete cases only.")
            combined_data <- data[rownames(mf), ]
            xmf <- model.frame(xformula, data = combined_data, na.action = na.omit)
            X <- model.matrix(xformula, data = xmf)
            Y <- Y[rownames(xmf)]
            D <- D[as.numeric(rownames(xmf))]
        }
    }

    n <- length(Y)
    n_treated <- sum(D == 1)
    n_control <- sum(D == 0)
    if (n_treated < 2 || n_control < 2) stop("Need at least 2 observations in both treatment and control groups")

    # Separate outcome regressions
    Xt <- X[D == 1, , drop = FALSE]
    Yt <- Y[D == 1]
    Xc <- X[D == 0, , drop = FALSE]
    Yc <- Y[D == 0]

    beta1 <- solve(crossprod(Xt), crossprod(Xt, Yt))
    beta0 <- solve(crossprod(Xc), crossprod(Xc, Yc))

    m1_hat <- as.vector(X %*% beta1)
    m0_hat <- as.vector(X %*% beta0)

    ATE_hat <- mean(m1_hat - m0_hat)

    p <- mean(D)
    if (p <= 0 || p >= 1) stop("Treatment share must be in (0,1)")

    # Influence function for RA ATE with estimated outcome models
    psi <- m1_hat - m0_hat - ATE_hat + D * (Y - m1_hat) / p - (1 - D) * (Y - m0_hat) / (1 - p)
    se <- sqrt(var(psi) / n)
    z <- qnorm(0.975)
    uci <- ATE_hat + z * se
    lci <- ATE_hat - z * se
    out <- list(
        ATT = ATE_hat,
        se = se,
        uci = uci,
        lci = lci,
        boots = NULL,
        att.inf.func = psi,
        call.param = call,
        argu = list(type = "or", panel = FALSE)
    )
    out$formula <- formula
    out$xformula <- xformula
    out$covariate_names <- covariate_names
    out$n_obs <- n
    out$n_treated <- sum(D == 1)
    out$n_control <- sum(D == 0)
    out$original_call <- call
    out$estimand <- "ATE"
    class(out) <- c("reg_adj", "drdid")
    return(out)
}
