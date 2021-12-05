test_that("LinearReg works", {
    my.result = LinearReg(mpg ~ cyl + disp + hp + cyl*disp, data = mtcars)
    ref = lm(mpg ~ cyl + disp + hp + cyl*disp, data = mtcars)
    for (i in 1:length(ref$coefficients)) {
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
    # model without intercept
    my.result2 = LinearReg(mpg ~ -1 + cyl + disp + hp + cyl*disp, data = mtcars)
    ref2 = lm(mpg ~ -1 + cyl + disp + hp + cyl*disp, data = mtcars)
    for (i in 1:length(ref2$coefficients)) {
        expect_lt(abs(my.result2$coefficients[i] - ref2$coefficients[i]), 0.00001)
    }
    for (i in 1:length(ref2$residuals)) {
        expect_lt(abs(my.result2$residuals[i] - ref2$residuals[i]), 0.00001)
        expect_lt(abs(my.result2$fitted.values[i] - ref2$fitted.values[i]), 0.00001)
    }
    expect_equal(my.result2$rank, ref2$rank)
    expect_equal(my.result2$df, ref2$df)
    expect_equal(my.result2$`Multiple R-squared`, summary(ref2)$r.squared)
})
