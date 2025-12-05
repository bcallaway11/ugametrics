# ugametrics


Tools for ECON 4750 at the University of Georgia.

## Installation

You can install ugametrics from GitHub using devtools:

``` r
devtools::install_github("bcallaway11/ugametrics")
```

## Usage

The main function in this package is `reg_adj()`, which implements
regression adjustment estimation for binary treatment effects.

### Basic Example

``` r
library(ugametrics)

# Estimate treatment effect with covariate adjustment
result <- reg_adj(mpg ~ am, xformula = ~hp + wt, data = mtcars)
print(result)
```


    Call:
    reg_adj(formula = mpg ~ am, xformula = ~hp + wt, data = mtcars)
    ------------------------------------------------------------------
     Regression Adjustment Estimator for the ATT:

     Identification: Unconfoundedness (Selection on Observables)

       ATT     Std. Error  t value    Pr(>|t|)  [95% Conf. Interval]
         3.3566       1.2444     2.697        0.007      0.9175     5.7957
    ------------------------------------------------------------------
     Sample size: N = 32
     Treated units: 13
     Control units: 19
    ------------------------------------------------------------------
     Covariates: (Intercept), hp, wt
     Estimation method: OLS
     Standard errors: Analytic (influence function)
    ------------------------------------------------------------------

``` r
summary(result)
```


    Call:
    reg_adj(formula = mpg ~ am, xformula = ~hp + wt, data = mtcars)

    ==================================================================
     Regression Adjustment Estimator for the ATT
    ==================================================================

    Identification Assumption:
      Unconfoundedness (Selection on Observables)
      All confounders must be observed and included in covariates

    ------------------------------------------------------------------
    Estimate:
    ------------------------------------------------------------------
      ATT estimate:            3.3566
      Standard error:          1.2444
      t-statistic:              2.697
      p-value:                 0.0070
      95% CI:              [   0.9175,    5.7957]

    ------------------------------------------------------------------
    Sample:
    ------------------------------------------------------------------
      Total observations:          32
      Treated units:               13 (40.6%)
      Control units:               19 (59.4%)

    ------------------------------------------------------------------
    Specification:
    ------------------------------------------------------------------
      Formula:            mpg ~ am
      Covariates:         ~hp + wt

      Included covariates: (Intercept), hp, wt
      Number of covariates: 3

    ------------------------------------------------------------------
    Method:
    ------------------------------------------------------------------
      Estimation:          Outcome regression (OLS)
      Standard errors:     Analytic (influence function)
      Inference:           Asymptotic (non-bootstrap)

    ==================================================================

``` r
# Unadjusted comparison
result_simple <- reg_adj(mpg ~ am, data = mtcars)
print(result_simple)
```


    Call:
    reg_adj(formula = mpg ~ am, data = mtcars)
    ------------------------------------------------------------------
     Regression Adjustment Estimator for the ATT:

     Identification: Unconfoundedness (Selection on Observables)

       ATT     Std. Error  t value    Pr(>|t|)  [95% Conf. Interval]
         7.2449       1.8825     3.849        0.000      3.5553    10.9346
    ------------------------------------------------------------------
     Sample size: N = 32
     Treated units: 13
     Control units: 19
    ------------------------------------------------------------------
     Covariates: Intercept only (unadjusted estimator)
     Estimation method: OLS
     Standard errors: Analytic (influence function)
    ------------------------------------------------------------------

## License

GPL (\>= 3)
