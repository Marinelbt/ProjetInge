lower0 <- function (M) {
    diag(M) <- 0
    M[lower.tri(M)] <- 0
    return(M)
}