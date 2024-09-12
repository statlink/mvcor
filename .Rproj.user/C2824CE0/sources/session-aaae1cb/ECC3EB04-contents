arv <- function(y, x) {

  px <- dim(x)[2]  ;    py <- dim(y)[2]
  s <- Rfast::cova( cbind(x, y) )
  sxy <- s[1:px, -(1:px)]
  up <- sum( sxy^2 )
  lx <- eigen(s[1:px, 1:px])
  ly <- eigen(s[-(1:px), -(1:px)])
  p <- min(px, py)
  up / sum( lx[1:p] * ly[1:p] )
}
