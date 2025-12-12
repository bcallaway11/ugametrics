#' Regression Adjustment Estimation for Binary Treatment Effects (ATT)
#'
#' Estimates the average treatment effect on the treated (ATT) using regression
#' adjustment under the unconfoundedness assumption. This function implements
#' outcome regression by estimating a model on the control group and predicting
#' counterfactual outcomes for the treated group.
#'
#' @param formula A formula specifying the outcome and treatment variables in the
#'   form \code{Y ~ D}, where \code{Y} is the outcome variable and \code{D} is
#'   the binary treatment indicator.
#' @param xformula An optional formula specifying the covariates for adjustment
#'   in the form \code{~X1 + X2 + ...}. If \code{NULL} (default), only an
#'   intercept is included (unadjusted comparison).
#' @param data A data frame containing the variables in the model.
#'
#' @details
#' The method:
#' \enumerate{
#'   \item Runs OLS regression of the outcome on covariates using control group data
#'   \item Predicts counterfactual outcomes for the treated group
#'   \item Computes ATT as the difference between observed and predicted outcomes
#' }
#'
#' The treatment variable \code{D} can be numeric (0/1), logical (FALSE/TRUE),
#' or a two-level factor. It will be converted to a numeric 0/1 indicator where
#' 0 represents the control group and 1 represents the treated group.
#'
#' \strong{Key Assumption:} Unconfoundedness (selection on observables) - all
#' confounders that affect both treatment assignment and potential outcomes must
#' be observed and included in \code{xformula}.
#'
#' @note Standard errors are computed analytically using the influence function
#' approach. Bootstrap inference is not currently supported in this wrapper.
#'
#' @return An object of class \code{c("reg_adj", "drdid")} with the following components:
#'
#'  ATT: The estimated average treatment effect on the treated
#'
#'  se: Standard error of the ATT estimate
#'
#'  uci: Upper bound of the 95\% confidence interval
#'
#'  lci: Lower bound of the 95\% confidence interval
#'
#'  boots: Bootstrap draws (NULL when boot = FALSE)
#'
#'  att.inf.func: Influence function (NULL when inffunc = FALSE)
#'
#'  call.param: Matched call parameters
#'
#'  argu: List of arguments used
#'
#' @examples
#' # Example with covariates (illustrative only, not causal)
#' result <- reg_adj_att(mpg ~ am, xformula = ~ hp + wt, data = mtcars)
#' print(result)
#' summary(result)
#'
#' # Example without covariates
#' result_simple <- reg_adj_att(mpg ~ am, data = mtcars)
#' print(result_simple)
#'
#' @importFrom stats model.frame model.matrix model.response na.omit pnorm qnorm var
#' @export
reg_adj_att <- function(formula, xformula = NULL, data) {
    # Validate inputs
    if (missing(data)) {
        stop("'data' argument is required")
    }
    if (missing(formula)) {
        stop("'formula' argument is required")
    }

    # Store the call
    call <- match.call()

    # Parse main formula (Y ~ D)
    mf <- model.frame(formula, data = data, na.action = na.omit)

    # Extract outcome (Y) and treatment (D)
    Y <- model.response(mf)
    if (ncol(mf) != 2) {
        stop("formula must be of the form Y ~ D (one outcome and one treatment variable)")
    }
    D_raw <- mf[, 2]

    # Convert treatment to numeric 0/1
    if (is.logical(D_raw)) {
        D <- as.numeric(D_raw) # FALSE -> 0, TRUE -> 1
        if (any(D_raw == FALSE)) {
            message("Treatment converted: FALSE (logical) -> 0 (control), TRUE -> 1 (treated)")
        }
    } else if (is.factor(D_raw)) {
        if (nlevels(D_raw) != 2) {
            stop("Treatment variable must have exactly 2 levels")
        }
        D <- as.numeric(D_raw) - 1 # First level -> 0, second level -> 1
        message(sprintf(
            "Treatment converted: %s (factor level 1) -> 0 (control), %s (factor level 2) -> 1 (treated)",
            levels(D_raw)[1], levels(D_raw)[2]
        ))
    } else {
        D <- as.numeric(D_raw)
    }

    # Validate treatment is binary
    unique_vals <- sort(unique(D))
    if (!all(unique_vals %in% c(0, 1)) || length(unique_vals) != 2) {
        stop("Treatment variable must be binary (0/1, FALSE/TRUE, or a two-level factor)")
    }

    # Check for sufficient observations in each group
    n_treated <- sum(D == 1)
    n_control <- sum(D == 0)
    if (n_treated < 2 || n_control < 2) {
        stop("Need at least 2 observations in both treatment and control groups")
    }

    # Parse covariate formula
    if (is.null(xformula)) {
        # Intercept only
        X <- model.matrix(~1, data = mf)
        covariate_names <- "(Intercept)"
    } else {
        # Extract covariates
        xmf <- model.frame(xformula, data = data, na.action = na.omit)
        X <- model.matrix(xformula, data = xmf)
        covariate_names <- colnames(X)

        # Check that observations match after na.omit
        if (nrow(xmf) != nrow(mf)) {
            warning("Missing values in covariates resulted in different sample sizes. Using complete cases only.")
            # Re-extract everything with consistent NA handling
            combined_data <- data[rownames(mf), ]
            xmf <- model.frame(xformula, data = combined_data, na.action = na.omit)
            X <- model.matrix(xformula, data = xmf)
            # Update Y and D to match
            Y <- Y[rownames(xmf)]
            D <- D[as.numeric(rownames(xmf))]
        }
    }

    # Manual ATT via outcome regression on controls
    n <- length(Y)
    Xc <- X[D == 0, , drop = FALSE]
    Yc <- Y[D == 0]
    Xt <- X[D == 1, , drop = FALSE]
    Yt <- Y[D == 1]

    # OLS on controls: beta_hat = (Xc'Xc)^{-1} Xc'Yc
    XtX <- crossprod(Xc)
    XtY <- crossprod(Xc, Yc)
    beta_hat <- solve(XtX, XtY)
    yhat_t <- as.vector(Xt %*% beta_hat)
    ATT_manual <- mean(Yt - yhat_t)

    # Standard error accounting for beta_hat estimated on controls
    resid_t <- Yt - yhat_t
    var1 <- var(resid_t) / n_treated
    resid_c <- Yc - as.vector(Xc %*% beta_hat)
    sigma2_c <- sum(resid_c^2) / n_control
    XtX_inv <- solve(XtX)
    mXt <- colMeans(Xt)
    var2 <- as.numeric(t(mXt) %*% (sigma2_c * XtX_inv) %*% mXt)
    se_att <- sqrt(var1 + var2)
    z <- qnorm(0.975)
    lci <- ATT_manual - z * se_att
    uci <- ATT_manual + z * se_att
    pval <- 2 * (1 - pnorm(abs(ATT_manual / se_att)))

    # Build return object consistent with drdid
    out <- list(
        ATT = ATT_manual,
        se = se_att,
        uci = uci,
        lci = lci,
        pval = pval,
        boots = NULL,
        att.inf.func = NULL,
        call.param = list(),
        argu = list()
    )

    # Add custom fields
    out$formula <- formula
    out$xformula <- xformula
    out$covariate_names <- covariate_names
    out$n_obs <- n
    out$n_treated <- n_treated
    out$n_control <- n_control
    out$original_call <- call

    class(out) <- c("reg_adj", "drdid")
    return(out)
}
