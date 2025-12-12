#' Print Method for reg_adj Objects
#'
#' Prints a summary of regression adjustment estimation results.
#'
#' @param x An object of class \code{"reg_adj"}
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object
#'
#' @importFrom stats pnorm
#' @export
print.reg_adj <- function(x, ...) {
    cat("\n")
    cat("Call:\n")
    print(x$original_call)
    cat("------------------------------------------------------------------\n")
    cat(" Regression Adjustment Estimator:\n")
    cat("\n")
    cat(" Identification: Unconfoundedness (Selection on Observables)\n")
    cat("\n")

    # Compute t-value and p-value
    t_val <- x$ATT / x$se
    p_val <- 2 * (1 - pnorm(abs(t_val)))

    # Create formatted output
    cat("     ATT       Std. Error  t value     Pr(>|t|)  [95% Conf. Interval]\n")
    cat(sprintf(
        "  %9.4f %12.4f %9.3f %12.4f  %10.4f %10.4f\n",
        x$ATT, x$se, t_val, p_val, x$lci, x$uci
    ))
    cat("------------------------------------------------------------------\n")

    # Sample information
    cat(sprintf(" Sample size: N = %d\n", x$n_obs))
    cat(sprintf(" Treated units: %d\n", x$n_treated))
    cat(sprintf(" Control units: %d\n", x$n_control))
    cat("------------------------------------------------------------------\n")

    # Covariate information
    if (length(x$covariate_names) == 1 && x$covariate_names[1] == "(Intercept)") {
        cat(" Covariates: Intercept only (unadjusted estimator)\n")
    } else {
        cat(" Covariates: ")
        if (length(x$covariate_names) <= 5) {
            cat(paste(x$covariate_names, collapse = ", "))
        } else {
            cat(paste(c(x$covariate_names[1:5], "..."), collapse = ", "))
        }
        cat("\n")
    }

    cat(" Estimation method: OLS\n")
    cat(" Standard errors: Analytic (influence function)\n")
    cat("------------------------------------------------------------------\n")
    cat("\n")

    invisible(x)
}


#' Summary Method for reg_adj Objects
#'
#' Produces a summary of regression adjustment estimation results.
#'
#' @param object An object of class \code{"reg_adj"}
#' @param ... Additional arguments (currently unused)
#'
#' @return An object of class \code{"summary.reg_adj"} containing estimated ATT,
#'   standard error, t-value, p-value, confidence interval, sample sizes
#'   (total, treated, control), and specification information
#'
#' @importFrom stats pnorm
#' @export
summary.reg_adj <- function(object, ...) {
    # Compute statistics
    t_val <- object$ATT / object$se
    p_val <- 2 * (1 - pnorm(abs(t_val)))

    # Create summary object
    out <- list(
        call = object$original_call,
        ATT = object$ATT,
        se = object$se,
        t_value = t_val,
        p_value = p_val,
        conf_int = c(lower = object$lci, upper = object$uci),
        n_obs = object$n_obs,
        n_treated = object$n_treated,
        n_control = object$n_control,
        covariates = object$covariate_names,
        formula = object$formula,
        xformula = object$xformula
    )

    class(out) <- "summary.reg_adj"
    return(out)
}


#' Print Method for summary.reg_adj Objects
#'
#' Prints a summary of regression adjustment estimation results.
#'
#' @param x An object of class \code{"summary.reg_adj"}
#' @param ... Additional arguments (currently unused)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.summary.reg_adj <- function(x, ...) {
    cat("\n")
    cat("Call:\n")
    print(x$call)
    cat("\n")
    cat("==================================================================\n")
    cat(" Regression Adjustment Estimator for the ATT\n")
    cat("==================================================================\n")
    cat("\n")
    cat("Identification Assumption:\n")
    cat("  Unconfoundedness (Selection on Observables)\n")
    cat("  All confounders must be observed and included in covariates\n")
    cat("\n")
    cat("------------------------------------------------------------------\n")
    cat("Estimate:\n")
    cat("------------------------------------------------------------------\n")
    cat(sprintf("  ATT estimate:        %10.4f\n", x$ATT))
    cat(sprintf("  Standard error:      %10.4f\n", x$se))
    cat(sprintf("  t-statistic:         %10.3f\n", x$t_value))
    cat(sprintf("  p-value:             %10.4f\n", x$p_value))
    cat(sprintf("  95%% CI:              [%9.4f, %9.4f]\n", x$conf_int[1], x$conf_int[2]))
    cat("\n")
    cat("------------------------------------------------------------------\n")
    cat("Sample:\n")
    cat("------------------------------------------------------------------\n")
    cat(sprintf("  Total observations:  %10d\n", x$n_obs))
    cat(sprintf(
        "  Treated units:       %10d (%.1f%%)\n",
        x$n_treated, 100 * x$n_treated / x$n_obs
    ))
    cat(sprintf(
        "  Control units:       %10d (%.1f%%)\n",
        x$n_control, 100 * x$n_control / x$n_obs
    ))
    cat("\n")
    cat("------------------------------------------------------------------\n")
    cat("Specification:\n")
    cat("------------------------------------------------------------------\n")
    cat("  Formula:            ")
    print(x$formula)
    if (!is.null(x$xformula)) {
        cat("  Covariates:         ")
        print(x$xformula)
    } else {
        cat("  Covariates:          None (intercept only)\n")
    }
    cat("\n")

    # List covariates
    if (length(x$covariates) == 1 && x$covariates[1] == "(Intercept)") {
        cat("  Adjustment:          Unadjusted (intercept only)\n")
    } else {
        cat("  Included covariates: ")
        cat(paste(x$covariates, collapse = ", "))
        cat("\n")
        cat(sprintf("  Number of covariates: %d\n", length(x$covariates)))
    }
    cat("\n")
    cat("------------------------------------------------------------------\n")
    cat("Method:\n")
    cat("------------------------------------------------------------------\n")
    cat("  Estimation:          Outcome regression (OLS)\n")
    cat("  Standard errors:     Analytic (influence function)\n")
    cat("  Inference:           Asymptotic (non-bootstrap)\n")
    cat("\n")
    cat("==================================================================\n")
    cat("\n")

    invisible(x)
}
