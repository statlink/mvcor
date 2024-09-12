mrv <- function(y, x) {

 y <- Rfast::standardise(y, center = TRUE, scale = FALSE)
 sy <- tcrossprod(y)
 diag(sy) <- 0
 x <- Rfast::standardise(x, center = TRUE, scale = FALSE)
 sx <- tcrossprod(x)
 diag(sx) <- 0
 sum(sy * sx) / sqrt( sum(sx^2) * sum( sy^2 ) )

}

