# ugametrics


Tools for ECON 4750 at the University of Georgia.

## Installation

You can install ugametrics from GitHub using devtools:

``` r
devtools::install_github("bcallaway11/ugametrics")
```

## Usage

The main function in this package is `reg_adj_att()`, which implements
regression adjustment estimation for binary treatment effects.

### Basic Example

``` r
library(ugametrics)

# Estimate treatment effect with covariate adjustment
result <- reg_adj_att(mpg ~ am, xformula = ~hp + wt, data = mtcars)
print(result)
```


    Call:
    reg_adj_att(formula = mpg ~ am, xformula = ~hp + wt, data = mtcars)
    ------------------------------------------------------------------
     Regression Adjustment Estimator for the ATT:

     Identification: Unconfoundedness (Selection on Observables)

         ATT       Std. Error  t value     Pr(>|t|)  [95% Conf. Interval]
         3.3566       1.2868     2.608       0.0091      0.8345     5.8787
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
    reg_adj_att(formula = mpg ~ am, xformula = ~hp + wt, data = mtcars)

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
      Standard error:          1.2868
      t-statistic:              2.608
      p-value:                 0.0091
      95% CI:              [   0.8345,    5.8787]

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
result_simple <- reg_adj_att(mpg ~ am, data = mtcars)
print(result_simple)
```


    Call:
    reg_adj_att(formula = mpg ~ am, data = mtcars)
    ------------------------------------------------------------------
     Regression Adjustment Estimator for the ATT:

     Identification: Unconfoundedness (Selection on Observables)

         ATT       Std. Error  t value     Pr(>|t|)  [95% Conf. Interval]
         7.2449       1.9126     3.788       0.0002      3.4963    10.9935
    ------------------------------------------------------------------
     Sample size: N = 32
     Treated units: 13
     Control units: 19
    ------------------------------------------------------------------
     Covariates: Intercept only (unadjusted estimator)
     Estimation method: OLS
     Standard errors: Analytic (influence function)
    ------------------------------------------------------------------

### ATE Example

``` r
# Regression adjustment ATE with covariate adjustment
result_ate <- reg_adj_ate(mpg ~ am, xformula = ~hp + wt, data = mtcars)
print(result_ate)
```


    Call:
    reg_adj_ate(formula = mpg ~ am, xformula = ~hp + wt, data = mtcars)
    ------------------------------------------------------------------
     Regression Adjustment Estimator for the ATE:

     Identification: Unconfoundedness (Selection on Observables)

         ATE       Std. Error  t value     Pr(>|t|)  [95% Conf. Interval]
        -0.7432       1.1387    -0.653       0.5140     -2.9750     1.4886
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
summary(result_ate)
```


    Call:
    reg_adj_ate(formula = mpg ~ am, xformula = ~hp + wt, data = mtcars)

    ==================================================================
     Regression Adjustment Estimator for the ATE
    ==================================================================

    Identification Assumption:
      Unconfoundedness (Selection on Observables)
      All confounders must be observed and included in covariates

    ------------------------------------------------------------------
    Estimate:
    ------------------------------------------------------------------
      ATE estimate:           -0.7432
      Standard error:          1.1387
      t-statistic:             -0.653
      p-value:                 0.5140
      95% CI:              [  -2.9750,    1.4886]

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

## License

GPL (\>= 3)
