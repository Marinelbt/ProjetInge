d2klrs <- function (k, l, r, s, lambda1, lambda2, signes, tabp, nbgames, 
    theo) 
{
    nbitems = length(lambda1)
    couples = data.frame(i = rep(1:nbitems, nbitems), j = rep(1:nbitems, 
        rep(nbitems, nbitems)))
    couples = couples[couples$j > couples$i, ]
    nbcouples = nrow(couples)
    urs = lapply(1:nbcouples, function(p, k, l, r, s, couples, 
        lambda1, lambda2) {
        i = couples$i[p]
        j = couples$j[p]
        if (r != s) 
            res = -(lambda1[i] - lambda1[j]) * (lambda2[i] - 
                lambda2[j])
        if ((r == 1) & (s == 1)) 
            res = (lambda2[i] - lambda2[j])^2
        if ((r == 2) & (s == 2)) 
            res = (lambda1[i] - lambda1[j])^2
        return(res)
    }, k = k, l = l, r = r, s = s, couples = couples, lambda1 = lambda1, 
        lambda2 = lambda2)
    dkrl = dkr(k, r, lambda1, lambda2, signes, tabp, nbgames, 
        theo)$dkr
    dlsl = dkr(l, s, lambda1, lambda2, signes, tabp, nbgames, 
        theo)$dkr
    dkrdls = lapply(1:nbcouples, function(p, dkrl, dlsl) dkrl[[p]] * 
        dlsl[[p]], dkrl = dkrl, dlsl = dlsl)
    d2l = lapply(1:nbcouples, function(p, k, l, urs, couples, 
        lambda1, lambda2, signes) {
        i = couples$i[p]
        j = couples$j[p]
        dij = sqrt((lambda1[i] - lambda1[j])^2 + (lambda2[i] - 
            lambda2[j])^2)
        if ((k != i) & (k != j)) 
            res = 0
        if ((l != i) & (l != j)) 
            res = 0
        if ((k == i) & (l == i)) 
            res = signes[i, j] * urs[[p]]/dij^3
        if ((k == i) & (l == j)) 
            res = -signes[i, j] * urs[[p]]/dij^3
        if ((k == j) & (l == i)) 
            res = -signes[i, j] * urs[[p]]/dij^3
        if ((k == j) & (l == j)) 
            res = signes[i, j] * urs[[p]]/dij^3
        return(res)
    }, k = k, l = l, urs = urs, couples = couples, lambda1 = lambda1, 
        lambda2 = lambda2, signes = signes)
    obsvstheo = nbgames * lower0(tabp - theo)
    obsvstheo = obsvstheo[row(obsvstheo) < col(obsvstheo)]
    mp = nbgames * lower0(theo * (1 - theo))
    mp = mp[row(mp) < col(mp)]
    sum(obsvstheo * unlist(d2l)) - sum(mp * unlist(dkrdls))
}
