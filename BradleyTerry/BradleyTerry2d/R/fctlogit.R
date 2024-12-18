fctlogit <- function (x) ifelse(x == 0, 0, log(x/(1 - x)))
