mantel <- function(y, x) {
 d1 <- Rfast::Dist(y, result = "vector")
 d2 <- Rfast::Dist(x, result = "vector")
 cor(y, x)
}
