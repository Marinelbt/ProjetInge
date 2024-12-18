resdev2dbt <- function (tabp, lambda1, lambda2, nbgames) {
    signes = sign(fctlogit(tabp))
    nbitems = length(lambda1)
    matl1 = matrix(rep(lambda1, nbitems), nrow = nbitems, ncol = nbitems)
    matl2 = matrix(rep(lambda2, nbitems), nrow = nbitems, ncol = nbitems)
    theo = invlogit(signes * sqrt((matl1 - t(matl1))^2 + (matl2 - 
        t(matl2))^2))
    obsvstheo = nbgames * tabp * log0(theo)
    lc = lchoose(nbgames, nbgames * tabp)
    L2 = sum(obsvstheo) + 0.5 * sum(lc)
    theo0 = matrix(0.5, nrow = nbitems, ncol = nbitems)
    diag(theo0) = 0
    obsvstheo0 = nbgames * tabp * log0(theo0)
    L0 = sum(obsvstheo0) + 0.5 * sum(lc)
    r2 = -2 * lc - 2 * nbgames * tabp * log0(theo) - 2 * (nbgames * 
        (1 - tabp) * log0(1 - theo))
    residuals = lower0(sign(tabp - theo) * sqrt(r2))
    list(res.deviance = -2 * L2, null.deviance = -2 * L0, residuals = residuals)
}
