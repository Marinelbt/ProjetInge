d2 <- function (lambda1, lambda2, signes, tabp, nbgames, theo) 
{
    nbitems = length(lambda1)
    couples = data.frame(k = rep(1:nbitems, 2), r = rep(1:2, 
        rep(nbitems, 2)))
    nbcouples = 2 * nbitems
    couples = data.frame(k = rep(couples$k, rep(nbcouples, nbcouples)), 
        r = rep(couples$r, rep(nbcouples, nbcouples)), l = rep(couples$k, 
            nbcouples), s = rep(couples$r, nbcouples))
    nbcouples = nrow(couples)
    ld2 = lapply(1:nbcouples, function(p, couples, lambda1, lambda2, 
        signes, tabp, nbgames, theo) {
        k = couples$k[p]
        r = couples$r[p]
        l = couples$l[p]
        s = couples$s[p]
        d2klrs(k = k, l = l, r = r, s = s, lambda1 = lambda1, 
            lambda2 = lambda2, signes = signes, tabp = tabp, 
            nbgames = nbgames, theo = theo)
    }, couples = couples, lambda1 = lambda1, lambda2 = lambda2, 
        signes = signes, tabp = tabp, nbgames = nbgames, theo = theo)
    md2 = matrix(unlist(ld2), nrow = 2 * nbitems, ncol = 2 * 
        nbitems, byrow = TRUE)
    return(md2)
}
