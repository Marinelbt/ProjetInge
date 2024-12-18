dvrais <- function (tabp, lambda1, lambda2, lag1, lag2, lag3, nbgames, 
    signes) 
{
    nbitems = length(lambda1)
    res = matrix(0, nrow = 2 * nbitems + 3, ncol = 1)
    matl1 = matrix(rep(lambda1, nbitems), nrow = nbitems, ncol = nbitems)
    matl2 = matrix(rep(lambda2, nbitems), nrow = nbitems, ncol = nbitems)
    theo = invlogitmat(signes * sqrt((matl1 - t(matl1))^2 + 
        (matl2 - t(matl2))^2))
    res[1:(2 * nbitems), 1] = d1(lambda1, lambda2, signes, tabp, 
        nbgames, theo) + matrix(c(rep(lag1, nbitems) + lag3 * 
        lambda2, rep(lag2, nbitems) + lag3 * lambda1), nrow = 2 * 
        nbitems, ncol = 1)
    res[2 * nbitems + 1, 1] <- sum(lambda1)
    res[2 * nbitems + 2, 1] <- sum(lambda2)
    res[2 * nbitems + 3, 1] <- sum(lambda1 * lambda2)
    return(res)
}
