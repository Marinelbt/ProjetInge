dkr <- function (k, r, lambda1, lambda2, signes, tabp, nbgames, theo) 
{
    nbitems = length(lambda1)
    couples = data.frame(i = rep(1:nbitems, nbitems), j = rep(1:nbitems, 
        rep(nbitems, nbitems)))
    couples = couples[couples$j > couples$i, ]
    nbcouples = nrow(couples)
    dkrl = lapply(1:nbcouples, function(p, k, r, couples, lambda1, 
        lambda2, signes) {
        i = couples$i[p]
        j = couples$j[p]
        dij = sqrt((lambda1[i] - lambda1[j])^2 + (lambda2[i] - 
            lambda2[j])^2)
        if ((k != i) & (k != j)) 
            res = 0
        if ((k == i) & (r == 1)) 
            res = signes[i, j] * (lambda1[i] - lambda1[j])/dij
        if ((k == i) & (r == 2)) 
            res = signes[i, j] * (lambda2[i] - lambda2[j])/dij
        if ((k == j) & (r == 1)) 
            res = -signes[i, j] * (lambda1[i] - lambda1[j])/dij
        if ((k == j) & (r == 2)) 
            res = -signes[i, j] * (lambda2[i] - lambda2[j])/dij
        return(res)
    }, k = k, r = r, couples = couples, lambda1 = lambda1, lambda2 = lambda2, 
        signes = signes)
    obsvstheo = nbgames * lower0(tabp - theo)
    obsvstheo = obsvstheo[row(obsvstheo) < col(obsvstheo)]
    list(dkrlogit = sum(obsvstheo * unlist(dkrl)), dkr = unlist(dkrl))
}
