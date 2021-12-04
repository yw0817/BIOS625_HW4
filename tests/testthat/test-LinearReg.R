test_that("LinearReg works", {
    my.result = LinearReg(mpg ~ cyl + disp + hp + cyl*disp, data = mtcars)
    ref = lm(mpg ~ cyl + disp + hp + cyl*disp, data = mtcars)

    for (i in 1:length(my.result$coefficients)) {
        expect_lt(abs(my.result$coefficients[i] - ref$coefficients[i]), 0.00001)
    }
    for (i in 1:length(ref$residuals)) {
        expect_lt(abs(my.result$residuals[i] - ref$residuals[i]), 0.00001)
        expect_lt(abs(my.result$fitted.values[i] - ref$fitted.values[i]), 0.00001)
    }
    expect_equal(my.result$rank, ref$rank)
    expect_equal(my.result$df, ref$df)
    expect_equal(my.result$`Multiple R-squared`, summary(ref)$r.squared)
    expect_equal(my.result$`Adjusted R-squared`, summary(ref)$adj.r.squared)
})
