tab2data <- function (tab){
    items = names(tab)
    items.r = row.names(tab)
    if (any(sort(items.r) != sort(items))) 
        items.r = items
    if (all(sort(items.r) == sort(items)) & any(items.r != items)) 
        tab = tab[pmatch(items, items.r), ]
    nbitems = length(items)
    games.tab = paste(rep(items, length(items)), rep(items, rep(length(items), 
        length(items))), sep = "-")
    winner = factor(rep(items, nbitems))
    loser = factor(rep(items, rep(nbitems, nbitems)))
    data = data.frame(winner = winner, loser = loser, Freq = unlist(tab), 
        row.names = NULL)
    return(data)
}
