require(Matrix)

to_laplacian <- function(A, regulariser=0) {
  degrees <- Matrix::rowSums(A)
  if (regulariser == 'auto') {
    regulariser <- mean(degrees)
  }
  d <- 1 / sqrt(degrees + regulariser)
  d[is.infinite(d)] <- 0
  D <- Matrix::Diagonal(x=d)
  L <- D %*% A %*% D
  return(L)
}