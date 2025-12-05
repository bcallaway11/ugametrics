#' Regression Adjustment Estimation for Binary Treatment Effects
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
#' This function wraps \code{DRDID::reg_did_panel()} for cross-sectional data
#' by setting the pre-treatment outcome (y0) to zero. The method:
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
#' result <- reg_adj(mpg ~ am, xformula = ~ hp + wt, data = mtcars)
#' print(result)
#' summary(result)
#'
#' # Example without covariates
#' result_simple <- reg_adj(mpg ~ am, data = mtcars)
#' print(result_simple)
#'
#' @seealso \code{\link[DRDID]{reg_did_panel}}
#'
#' @importFrom DRDID reg_did_panel
#' @importFrom stats model.frame model.matrix model.response na.omit pnorm
#' @export
reg_adj <- function(formula, xformula = NULL, data) {
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

    # Create y0 as vector of zeros
    n <- length(Y)
    y0 <- rep(0, n)

    # Call DRDID::reg_did_panel
    result <- DRDID::reg_did_panel(
        y1 = Y,
        y0 = y0,
        D = D,
        covariates = X,
        i.weights = NULL,
        boot = FALSE,
        boot.type = "weighted",
        nboot = NULL,
        inffunc = FALSE
    )

    # Add custom class and store additional information
    result$formula <- formula
    result$xformula <- xformula
    result$covariate_names <- covariate_names
    result$n_obs <- n
    result$n_treated <- n_treated
    result$n_control <- n_control
    result$original_call <- call

    class(result) <- c("reg_adj", "drdid")

    return(result)
}
