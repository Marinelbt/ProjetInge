bt1d <- function (data) 
{
    initbt2d = initalgo(data)
    nbgames = initbt2d$nbgames
    tabp = initbt2d$tabp
    tab = nbgames * tabp
    nbitems = nrow(tabp)
    items = row.names(tab)
    dataglm = data.frame(data, nbgames = as.vector(nbgames))
    dataglm = dataglm[dataglm$winner != dataglm$loser, ]
    bt.glm = glm(cbind(Freq, nbgames - Freq) ~ -1 + C(winner, 
        sum) + C(loser, sum), data = dataglm, family = binomial)
    sbt.glm = summary(bt.glm)$coefficients[1:nbitems, ]
    row.names(sbt.glm) = items
    lambda = sbt.glm[, 1]
    se.lambda = sbt.glm[, 2]
    matl = matrix(rep(lambda, nbitems), nrow = nbitems, ncol = nbitems)
    theo = invlogit(matl - t(matl))
    diag(theo) = 0
    obsvstheo = nbgames * tabp * log0(theo)
    lc = lchoose(nbgames, nbgames * tabp)
    L1 = sum(obsvstheo) + 0.5 * sum(lc)
    list(lambda = lambda, se.lambda = se.lambda, theo = theo, 
        res.deviance = -2 * L1, res.df = nbitems * (nbitems - 
            1)/2 - nbitems + 1, s = summary(bt.glm))
}
