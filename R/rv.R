rv <- function(y, x) {

  y <- as.matrix(y)
  x <- as.matrix(x)
  p <- dim(x)[2]
  sxy <- Rfast::cova( cbind(x, y) )[1:p, -(1:p)]
  sxx <- Rfast::cova(x)
  syy <- Rfast::cova(y)

  a <- sum( sxy^2 )
  b1 <- sum( sxx^2 )
  b2 <- sum( syy^2 )

  a / sqrt(b1 * b2)

}
