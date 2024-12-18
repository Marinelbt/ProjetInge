d1 <- function (lambda1, lambda2, signes, tabp, nbgames, theo) 
{
    nbitems = length(lambda1)
    couples = data.frame(k = rep(1:nbitems, 2), r = rep(1:2, 
        rep(nbitems, 2)))
    nbcouples = 2 * nbitems
    ld1 = lapply(1:nbcouples, function(p, couples, lambda1, lambda2, 
        signes, tabp, nbgames, theo) {
        k = couples$k[p]
        r = couples$r[p]
        dkr(k = k, r = r, lambda1 = lambda1, lambda2 = lambda2, 
            signes = signes, tabp = tabp, nbgames = nbgames, 
            theo = theo)$dkrlogit
    }, couples = couples, lambda1 = lambda1, lambda2 = lambda2, 
        signes = signes, tabp = tabp, nbgames = nbgames, theo = theo)
    vd1 = unlist(ld1)
    return(vd1)
}
