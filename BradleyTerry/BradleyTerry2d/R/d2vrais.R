d2vrais <- function (tabp, lambda1, lambda2, lag1, lag2, lag3, nbgames, 
    signes) {
    nbitems = length(lambda1)
    res = matrix(0, nrow = 2 * nbitems + 3, ncol = 2 * nbitems + 
        3)
    matl1 = matrix(rep(lambda1, nbitems), nrow = nbitems, ncol = nbitems)
    matl2 = matrix(rep(lambda2, nbitems), nrow = nbitems, ncol = nbitems)
    theo = invlogitmat(signes * sqrt((matl1 - t(matl1))^2 + 
        (matl2 - t(matl2))^2))
    mat01 = matrix(0, nrow = 2 * nbitems, ncol = 2 * nbitems)
    mat01[(nbitems + 1):(2 * nbitems), 1:nbitems] = diag(nbitems)
    mat01[1:nbitems, (nbitems + 1):(2 * nbitems)] = diag(nbitems)
    res[1:(2 * nbitems), 1:(2 * nbitems)] = d2(lambda1, lambda2, 
        signes, tabp, nbgames, theo) + lag3 * mat01
    res[1:(2 * nbitems), (2 * nbitems + 1)] = c(rep(1, nbitems), 
        rep(0, nbitems))
    res[1:(2 * nbitems), (2 * nbitems + 2)] = c(rep(0, nbitems), 
        rep(1, nbitems))
    res[1:(2 * nbitems), (2 * nbitems + 3)] = c(lambda2, lambda1)
    res[((2 * nbitems + 1):(2 * nbitems + 3)), 1:(2 * nbitems)] = t(res[1:(2 * 
        nbitems), ((2 * nbitems + 1):(2 * nbitems + 3))])
    return(res)
}
