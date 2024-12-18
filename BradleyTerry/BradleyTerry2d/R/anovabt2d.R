anovabt2d <- function (model) 
{
    Df = c(NA, model$res.df0 - model$res.df1, model$res.df1 - 
        model$res.df2)
    Deviance = c(NA, model$res.deviance0 - model$res.deviance1, 
        model$res.deviance1 - model$res.deviance2)
    ResidDf = c(model$res.df0, model$res.df1, model$res.df2)
    ResidDev = c(model$res.deviance0, model$res.deviance1, model$res.deviance2)
    pvalue = 1 - pchisq(Deviance, Df)
    aovtab = data.frame(Df, Deviance, ResidDf, ResidDev, pvalue)
    dimnames(aovtab) = list(c("NULL", "Bradley-Terry", "2d Bradley-Terry"), 
        c("Df", "Deviance", "Resid. Df", "Resid. Dev", "P(>|Chi|)"))
    title = paste("Analysis of Deviance Table", "\n\nTerms added sequentially (first to last)\n\n", 
        sep = "")
    structure(aovtab, heading = title, class = c("anova", "data.frame"))
}
