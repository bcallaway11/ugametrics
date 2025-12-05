pkgname <- "ugametrics"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('ugametrics')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("reg_adj")
### * reg_adj

flush(stderr()); flush(stdout())

### Name: reg_adj
### Title: Regression Adjustment Estimation for Binary Treatment Effects
### Aliases: reg_adj

### ** Examples

# Example with covariates (illustrative only, not causal)
result <- reg_adj(mpg ~ am, xformula = ~ hp + wt, data = mtcars)
print(result)
summary(result)

# Example without covariates
result_simple <- reg_adj(mpg ~ am, data = mtcars)
print(result_simple)




cleanEx()
nameEx("ugametrics-package")
### * ugametrics-package

flush(stderr()); flush(stdout())

### Name: ugametrics-package
### Title: ugametrics: Regression Adjustment Estimation
### Aliases: ugametrics ugametrics-package
### Keywords: internal

### ** Examples

# Basic example with covariates
result <- reg_adj(mpg ~ am, xformula = ~ hp + wt, data = mtcars)
print(result)

# Example without covariates
result_simple <- reg_adj(mpg ~ am, data = mtcars)
summary(result_simple)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
