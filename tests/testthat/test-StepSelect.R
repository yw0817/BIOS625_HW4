test_that("StepSelect works", {
    install.packages("MASS", repos="http://cran.us.r-project.org")
    library(MASS)
    full_lm <- lm(mpg ~ ., data = mtcars)
    n = dim(mtcars)[1]
    # "backward selection"
    ref = stepAIC(full_lm, direction = "backward", k = log(n), trace = FALSE)
    my = StepSelect(full_lm, direction = "backward", k = log(n), trace = FALSE)
    expect_equal(my$coefficients, ref$coefficients)
    expect_equal(my$call$formula, ref$call$formula)
    # "forward selection"
    m = lm(mpg ~ 1, data = mtcars)
    ref2 = stepAIC(m, direction="forward", k = log(n), scope=list(lower=m, upper=full_lm), trace = TRUE)
    my2 = StepSelect(full_lm, direction = "forward", k = log(n), trace = TRUE)
    expect_equal(my2$coefficients, ref2$coefficients)
    expect_equal(my2$call$formula, ref2$call$formula)

    expect_warning(StepSelect(full_lm, direction = "forw", k = log(n), trace = TRUE), "Error in direction: should be 'backward' or 'forward' ")
})
