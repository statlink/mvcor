mantel <- function(y, x) {
 d1 <- Rfast::Dist(y, vector = TRUE)
 d2 <- Rfast::Dist(x, vector = TRUE)
 cor(y, x)
}
