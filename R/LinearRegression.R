#'Linear Regression
#'
#'Fit a linear regression model
#'
#'@param theModel user-defined model to fit in linear regression
#'@param data the data set this linear regression model is fitting on
#'
#'@return a summary of the linear regression model including the residual statistics, coefficients, statistics values (t and F), p-values, and degrees of freedoms
#'
#'@examples
#'LinearRegssion(mpg ~ cyl + disp + hp, data = mtcars)
#'
#'@export
#'
LinearRegssion = function(theModel, data){
    data = data[complete.cases(data[,all.vars(theModel)]),]
    Y = data[[all.vars(theModel)[1]]]
    X = model.matrix(theModel, data)
    n = dim(data)[1]

    # beta coefficients
    coeffs = solve(t(X) %*% X) %*% t(X) %*% Y
    Y.hat = X %*% coeffs
    sigma.sq = sum((Y - Y.hat)^2)/(n - length(coeffs))
    # beta standard errors
    std.errs = sqrt(diag(sigma.sq * solve(t(X) %*% X)))
    # t values
    t.vals = coeffs/std.errs
    # p-values
    p.vals = 2 * pt(abs(t.vals), df = n - length(t.vals), lower.tail = FALSE)
    p.vals = ifelse(p.vals < 2e-16, "<2e-16", round(p.vals, 3))

    SSE = sum((Y - Y.hat)^2)
    SSY = sum((Y - mean(Y))^2)
    R.sq = 1 - SSE/SSY
    adj.R.sq = 1 - (SSE/(n - length(t.vals)))/(SSY/(n-1))
    # F statistics
    F.stats = ((SSY-SSE)/(length(t.vals) - 1))/(SSE/(n - length(t.vals)))
    p.f = pf(F.stats, (length(t.vals) - 1), (n - length(t.vals)), lower.tail = FALSE)
    p.f = ifelse(p.f < 2e-16, "< 2e-16", round(p.f, 4))

    result = list()
    res.table = round(quantile((Y - Y.hat), probs = c(0, 0.25, 0.5, 0.75, 1)), 3)
    res.table = as.table(res.table)
    rownames(res.table) = c("Min", "1Q", "Median", "3Q", "Max")
    result[["Residuals:"]] = res.table

    result.df = data.frame("Variable" = colnames(X), "Estimate" = round(coeffs, 5), "Std.Error" = round(std.errs, 5), "t value" = round(t.vals, 3), "Pr(>|t|)" = p.vals)
    result[["Coefficients:"]] = result.df
    result[["Residual standard error:"]] = paste("Residual standard error: ", round((sqrt(sigma.sq)), 2), " on ", (n - length(t.vals)), " degrees of freedom")
    result[["R-squared:"]] = paste("Multiple R-squared:  ", round(R.sq, 4), ", Adjusted R-squared:  ", round(adj.R.sq, 4))
    result[["F-statistic:"]] = paste(round(F.stats, 2), " on ", (length(t.vals) - 1), " and ", (n - length(t.vals)), " DF,  p-value: ", p.f)

    return(result)
}
