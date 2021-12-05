# BIOS625_HW4

<!-- badges: start -->
  [![R build status](https://github.com/yw0817/BIOS625_HW4/workflows/R-CMD-check/badge.svg)](https://github.com/yw0817/BIOS625_HW4/actions)
<!-- badges: end -->

[![test-coverage](https://github.com/yw0817/BIOS625_HW4/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/yw0817/BIOS625_HW4/actions/workflows/test-coverage.yaml)

## LinReg.Select

### Introduction

The R package LinReg.Select contains two functions, LinearReg and StepSelect. The function LinearReg generates linear regression model result with summary according to user defined formula and data set. The function StepSelect conducts either backward selection or forward selection based on AIC from a full model defined by user or or the null model. In this function, user can also define the the penalty used in AIC, k. To show the process of the backward or forward selection, user can turn on the trace option (trace = TRUE) in function StepSelect.

### Tutorial for LinearReg

LinearReg takes two parameters, "theModel" and "data". "theModel" is the formula user can define. 
For example, mpg ~ cyl + disp + hp, meaning that mpg is regressed on cyl, disp, and hp. 
"data" should be the dataset the linear regression model is based on. 
For example, LinearReg(mpg ~ cyl + disp + hp, data = mtcars).
This function can also take interaction terms or terms with higher power. 
For example, LinearReg(mpg ~ cyl + disp + hp + cyl*disp + I(hp^2), data = mtcars).

### Tutorial for StepSelect

StepSelect takes four parameters, "full", "direction", "k" and "trace".
"full" is the full model which backward selection process starts from or the upper limit model for forward selection.
For example, full = lm(mpg ~ ., data = mtcars).
"direction" can be either "backward" or "forward", indicating backward selection or forward selection. 
"k" is the penalty used in AIC. "k" is 2 by default but user can define it.
"trace" equals FALSE by default, which means the process of the step selection will not be shown. To see the process of backward or forward selection, user can define "trace" to be TRUE. 
