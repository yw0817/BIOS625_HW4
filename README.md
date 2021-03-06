# BIOS625_HW4

<!-- badges: start -->
  [![R build status](https://github.com/yw0817/BIOS625_HW4/workflows/R-CMD-check/badge.svg)](https://github.com/yw0817/BIOS625_HW4/actions)
<!-- badges: end -->

[![codecov](https://codecov.io/gh/yw0817/BIOS625_HW4/branch/main/graph/badge.svg?token=D42C7BUROA)](https://codecov.io/gh/yw0817/BIOS625_HW4)

[![test-coverage](https://github.com/yw0817/BIOS625_HW4/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/yw0817/BIOS625_HW4/actions/workflows/test-coverage.yaml)

## LinReg.Select

### Introduction

The R package LinReg.Select contains two functions, LinearReg and StepSelect. The function LinearReg generates linear regression model result with summary according to user defined formula and data set. The function StepSelect conducts either backward selection or forward selection based on AIC from a full model defined by user or from the null model respectively. In this function, user can also define the the penalty used in AIC, k. To show the process of the backward or forward selection, user can turn on the trace option (trace = TRUE) in function StepSelect.

### Tutorial for LinearReg

LinearReg takes two parameters, `@param theModel` and `@param data`. 

`@param theModel` is the formula user can define. 

`@param data` should be the dataset the linear regression model is based on.

For example:
```
LinearReg(mpg ~ cyl + disp + hp, data = mtcars)
```

This function can also take interaction terms, terms with higher power and model without intercept. 

For example:
```
LinearReg(mpg ~ -1 + cyl + disp + hp + cyl*disp + I(hp^2), data = mtcars).
```



### Tutorial for StepSelect

StepSelect takes four parameters, `@param full`, `@param direction`, `@param k`, and `@param trace`.

`@param full` is the full model which backward selection process starts from or the upper limit model for forward selection.

`@param direction` can be either "backward" or "forward", indicating backward selection or forward selection. 

`@param k` is the penalty used in AIC. "k" is 2 by default but user can define it. For more information, [refer to the function AIC](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/AIC).

`@param trace` equals FALSE by default, which means the process of the step selection will not be shown. To see the process of backward or forward selection, user can define "trace" to be TRUE. 

For example:
```
full = lm(mpg ~ ., data = mtcars)
StepSelect(full_lm, direction = "backward", k = log(n), trace = TRUE)
StepSelect(full_lm, direction = "forward", k = log(n), trace = TRUE)
```
