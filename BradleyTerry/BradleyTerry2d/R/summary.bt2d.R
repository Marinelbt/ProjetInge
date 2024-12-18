summary.bt2d <- function (object, ...) {
    s = object$s
    Estimate = c(object$lambda1, object$lambda2)
    StdError = sqrt(diag(object$varscores))
    zvalue = Estimate/StdError
    pvalue = 2 * (1 - pt(abs(zvalue), df = object$res.df2))
    coeftab = cbind(Estimate, StdError, zvalue, pvalue)
    dimnames(coeftab) = list(c(paste(names(object$lambda1), "lambda1", 
        sep = "."), paste(names(object$lambda2), "lambda2", sep = ".")), 
        c("Estimate", "Std. Error", "z value", "Pr(>|z|)"))
    s$call = "2d Bradley-Terry model"
    s$coefficients = coeftab
    s$deviance = object$res.deviance2
    s$aic = object$res.deviance2 + 2 * (2 * length(object$lambda1) - 
        3)
    s$df.residual = object$res.df2
    s$null.deviance = object$res.deviance0
    s$iter = object$iter
    s$df = NULL
    s$deviance.resid = object$residuals[row(object$residuals) < 
        col(object$residuals)]
    s$cov.unscaled = object$varscores
    s$cov.scaled = object$varscores
    structure(s, class = "summary.glm")
}
