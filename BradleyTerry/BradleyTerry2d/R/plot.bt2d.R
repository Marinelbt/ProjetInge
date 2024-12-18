plot.bt2d <- function (x, lab = names(x$lambda1), alpha = 0.05, deltax = 0.025, 
    deltay = 0.025, lty = 1, col = "blue", lwd = 2, ellipse = TRUE, ...) {
    nbitems = length(x$lambda1)
    lvar = lapply(1:nbitems, function(i, l1, l2, vars) {
        list(m = c(l1[i], l2[i]), v = vars[c(i, length(l1) + 
            i), c(i, length(l1) + i)])
    }, l1 = x$lambda1, l2 = x$lambda2, vars = x$varscores)
    ellipses = lapply(lvar, function(v, alpha) ell(v$m, v$v, 
        alpha), alpha = alpha)
    limx = range(unlist(lapply(ellipses, function(e) e[, 1])))
    limy = range(unlist(lapply(ellipses, function(e) e[, 2])))
    limx = range(c(limx, limy))
    limy = range(c(limx, limy))
    plot(x$lambda1, x$lambda2, bty = "l", pch = 16, cex = 1.25, 
        xlab = "Dim. 1", ylab = "Dim. 2", xlim = limx, ylim = limy, 
        col = col, cex.lab = 1.25)
    text(x$lambda1 + deltax * diff(limx), x$lambda2 + 
        deltay * diff(limy), lab, cex = 1.25, col = col)
    if (ellipse) 
        graph.ell = lapply(ellipses, function(e, lty, lwd, col = col) lines(e[, 
            1], e[, 2], lty = lty, col = col, lwd = lwd), lty = lty, 
            col = col, lwd = lwd)
    lines(x$lambda, rep(0, nbitems), type = "b", lwd = lwd, 
        col = "black", pch = 16, cex = 1.25)
    segments(limx[1], 0, min(x$lambda), 0, col = "black", 
        lwd = lwd)
    segments(max(x$lambda), 0, limx[2], 0, col = "black", 
        lwd = lwd)
    text(x$lambda, deltay * diff(limy), lab, col = "black", 
        cex = 1.25)
}
