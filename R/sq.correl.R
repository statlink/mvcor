sq.correl <- function (y, x) {
  n <- dim(y)[1]
  d <- dim(y)[2]
  y <- Rfast::standardise(y, center = TRUE, scale = FALSE)
  YY <- crossprod(y)
  X <- cbind(1, x)
  U <- .lm.fit(X, y)$residuals
  if (!is.matrix(U))
    U <- matrix(U)
  UU <- crossprod(U)
  D <- solve(YY, UU)
  r2T <- mean(1 - diag(D))
  r2D <- det(diag(d) - D)
  result <- c(r2T, r2D)
  names(result) <- c("Trace R^2", "Det R^2")
  result
}

