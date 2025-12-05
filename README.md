# ugametrics

Regression Adjustment Estimation for Binary Treatment Effects

## Overview

`ugametrics` provides a formula interface for regression adjustment estimation to estimate the average treatment effect on the treated (ATT) under the unconfoundedness assumption (selection on observables).

The package implements outcome regression by:
1. Estimating a regression model using the control group
2. Predicting counterfactual outcomes for the treated group
3. Computing the ATT as the difference between observed and predicted outcomes

This approach requires the assumption that all confounders are observed and included in the covariate adjustment.

## Installation

You can install the development version of ugametrics from a local source:

```r
# Install from local directory
install.packages("/path/to/ugametrics", repos = NULL, type = "source")

# Or using devtools
devtools::install_local("/path/to/ugametrics")
```

## Usage

### Basic Example with Covariates

```r
library(ugametrics)

# Estimate ATT with covariate adjustment
# Outcome: mpg, Treatment: am (transmission type)
# Covariates: hp (horsepower) and wt (weight)
result <- reg_adj(mpg ~ am, xformula = ~hp + wt, data = mtcars)
print(result)
summary(result)
```

### Example without Covariates

```r
# Estimate ATT without covariate adjustment (intercept only)
result_simple <- reg_adj(mpg ~ am, data = mtcars)
print(result_simple)
```

## Important Note

The `mtcars` examples above are for illustration purposes only and should not be interpreted as causal estimates. The unconfoundedness assumption requires that all confounders affecting both treatment assignment and the outcome are observed and included in the model.

## Methodology

The package wraps `DRDID::reg_did_panel()` for cross-sectional data by:
- Setting the pre-treatment outcome to zero (y0 = 0)
- Using the post-treatment outcome as y1
- Estimating an outcome regression model on the control group
- Computing the ATT via regression adjustment

**Key Assumption**: Unconfoundedness (selection on observables) - all variables that jointly affect treatment assignment and potential outcomes must be included in the covariate adjustment.

## License

GPL (>= 3)
