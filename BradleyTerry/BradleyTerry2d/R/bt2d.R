bt2d <- function (data, tol = 1e-04, max.iter = 20) 
{
    initbt2d = initalgo(data)
    lambda1 = initbt2d$lambda1
    lambda2 = initbt2d$lambda2
    nbgames = initbt2d$nbgames
    tabp = initbt2d$tabp
    nbitems = nrow(tabp)
    items = dimnames(tabp)[[2]]
    signes = sign(fctlogit(tabp))
    resdev = resdev2dbt(tabp, lambda1, lambda2, nbgames)
    null.resdev = resdev$null.deviance
    resdev = resdev$res.deviance
    lag1 = 1
    lag2 = 1
    lag3 = 1
    alpha = matrix(c(lambda1, lambda2, lag1, lag2, lag3), nrow = 2 * 
        nbitems + 3, ncol = 1)
    eps = resdev
    nbiter = 1
    while ((eps > tol) & (nbiter <= max.iter)) {
        nbiter = nbiter + 1
        auxd1 = dvrais(tabp, lambda1, lambda2, lag1, lag2, lag3, 
            nbgames, signes)
        auxd2 = d2vrais(tabp, lambda1, lambda2, lag1, lag2, lag3, 
            nbgames, signes)
        iauxd2 = solve(auxd2)
        alpha = alpha - iauxd2 %*% auxd1
        lambda1 = alpha[1:nbitems]
        lambda2 = alpha[(nbitems + 1):(2 * nbitems)]
        lag1 = alpha[(2 * nbitems + 1)]
        lag2 = alpha[(2 * nbitems + 2)]
        lag3 = alpha[(2 * nbitems + 3)]
        resdev0 = resdev
        resdev = resdev2dbt(tabp, lambda1, lambda2, nbgames)$res.deviance
        eps = abs(resdev0 - resdev)
        print(resdev)
    }
    names(lambda1) = items
    names(lambda2) = items
    resdev = resdev2dbt(tabp, lambda1, lambda2, nbgames)
    residuals = resdev$residuals
    resdev = resdev$res.deviance
    matl1 = matrix(rep(lambda1, nbitems), nrow = nbitems, ncol = nbitems)
    theo = invlogit(matl1 - t(matl1))
    devp = -2 * sum(nbgames * tabp * log0(theo))
    matl1 = matrix(rep(-lambda1, nbitems), nrow = nbitems, ncol = nbitems)
    theo = invlogit(matl1 - t(matl1))
    devm = -2 * sum(nbgames * tabp * log0(theo))
    if (devm < devp) 
        lambda1 = -lambda1
    varscores = -solve(d2vrais(tabp, lambda1, lambda2, lag1, 
        lag2, lag3, nbgames, signes))[1:(2 * nbitems), 1:(2 * 
        nbitems)]
    resbt1 = bt1d(data)
    output <- list(lambda1 = lambda1, lambda2 = lambda2, lambda = resbt1$lambda, 
        res.deviance2 = resdev, res.df2 = nbitems * (nbitems - 
            1)/2 - 2 * nbitems + 3, res.deviance1 = resbt1$res.deviance, 
        res.df1 = resbt1$res.df, res.deviance0 = null.resdev, 
        res.df0 = nbitems * (nbitems - 1)/2, varscores = varscores, 
        s = resbt1$s, iter = nbiter, residuals = residuals)
	class(output) <- "bt2d"
	return(output)
}
