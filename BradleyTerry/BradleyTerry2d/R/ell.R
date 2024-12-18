ell <- function (moy, covar, alpha = 0.05) 
{
    theta = seq(0, 2 * pi, 0.01 * pi)
    p = length(moy)
    r = sqrt(qchisq(1 - alpha, p))
    cercle = cbind(r * cos(theta), r * sin(theta))
    moy = matrix(moy, nrow = length(moy), ncol = 1)
    eig = eigen(covar, symmetric = T)
    cercle %*% (eig$vectors %*% diag(sqrt(eig$values)) %*% t(eig$vectors)) + 
        matrix(rep(moy, nrow(cercle)), nrow = nrow(cercle), ncol = 2, 
            byrow = T)
}
