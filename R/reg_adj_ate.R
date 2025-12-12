#' Regression Adjustment Estimation for Binary Treatment Effects (ATE)
#'
#' Estimates the average treatment effect (ATE) using regression adjustment
#' under unconfoundedness. Outcome regression is estimated on the control group
#' and used to predict counterfactual outcomes for both treated and control
#' units, averaging appropriately.
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
    }
    n <- length(Y)
    # Outcome regression on controls
    Xc <- X[D == 0, , drop = FALSE]
    Yc <- Y[D == 0]
    XtX <- crossprod(Xc)
    XtY <- crossprod(Xc, Yc)
    beta_hat <- solve(XtX, XtY)
    # Predict for all
    yhat_all <- as.vector(X %*% beta_hat)
    # ATE = E[Y(1)-Y(0)] approximated by E[Y - yhat] over treated minus control weights
    # Here use RA form: ATE = mean( (D*(Y - yhat)) / p + ((1-D)*(yhat - Y)) / (1-p) )
    p <- mean(D)
    ATE_manual <- mean(D * (Y - yhat_all) / p + (1 - D) * (yhat_all - Y) / (1 - p))
    # Use DRDID to get SE via panel RA (with y0=0) is not exact for ATE; fallback to sample variance of IF from RA formula
    psi <- D * (Y - yhat_all) / p + (1 - D) * (yhat_all - Y) / (1 - p)
    se <- sqrt(var(psi) / n)
    z <- qnorm(0.975)
    uci <- ATE_manual + z * se
    lci <- ATE_manual - z * se
    out <- list(ATT = ATE_manual, se = se, uci = uci, lci = lci, boots = NULL, att.inf.func = psi, call.param = call, argu = list(type = "or", panel = FALSE))
    out$formula <- formula
    out$xformula <- xformula
    out$covariate_names <- covariate_names
    out$n_obs <- n
    out$n_treated <- sum(D == 1)
    out$n_control <- sum(D == 0)
    out$original_call <- call
    class(out) <- c("reg_adj", "drdid")
    return(out)
}
