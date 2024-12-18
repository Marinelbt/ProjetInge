invlogitmat <- function (M) {
    theo <- invlogit(M)
    return(lower0(theo))
}
