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

         ATT       Std. Error  t value     Pr(>|t|)  [95% Conf. Interval]
         3.3566       1.2248     2.740       0.0061      0.9559     5.7573
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
      Standard error:          1.2248
      t-statistic:              2.740
      p-value:                 0.0061
      95% CI:              [   0.9559,    5.7573]

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

         ATT       Std. Error  t value     Pr(>|t|)  [95% Conf. Interval]
         7.2449       1.8528     3.910       0.0001      3.6134    10.8765
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
