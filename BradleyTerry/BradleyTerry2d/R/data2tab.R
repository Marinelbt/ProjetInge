data2tab <- function (data) 
{
    winner = factor(data[, 1])
    loser = factor(data[, 2])
    labels = union(winner, loser)
    games.tab = paste(rep(labels, length(labels)), rep(labels, 
        rep(length(labels), length(labels))), sep = "-")
    games = paste(winner, loser, sep = "-")
    btdata = merge(data.frame(data, games = games), data.frame(games = games.tab), 
        by = "games")[, -1]
    freq = btdata[, 3]
    tab = matrix(freq, nrow = length(labels), ncol = length(labels), 
        byrow = TRUE)
    dimnames(tab) = list(labels, labels)
    diag(tab) = 0
    return(tab)
}
