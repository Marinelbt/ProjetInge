initalgo <- function (data) 
{
    tab = data2tab(data)
    nbgames = (tab + t(tab))
    tabp = tab/nbgames
    diag(tabp) = 0
    cmdp = cmdscale(abs(fctlogit(tabp)), k = 2, eig = T)$points
    list(lambda1 = cmdp[, 1], lambda2 = cmdp[, 2], nbgames = nbgames, 
        tabp = tabp)
}
