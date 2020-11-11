# Nome: Matheus Mello
# matheusferreiramello@hotmail.com

### Applying the Gram-Schmidt Process to a set of vectors

## First, we create a function that calculates the dot product of two vectors.

#' @params u, v are vectors.
#' 
#' @return is a numeric value, corresponding to <u,v>
#' 
#' 
#' @example
#' 
#' # Calculates the dot product between two vectors
#' 
#' u = rnorm(n = 5)
#' v = -2:2
#' 
#' dot_product(u, v) 
#' 
#' # Calculates a vector's norm
#' 
#' u = rnorm(n = 10)
#' 
#' dot_product(u)
#' 
#' @export

dot_product <- function(u, v = NULL) {
  if (is.vector(u) != T) {
    stop('u must be a vector')
  }
  soma <- 0
  if (is.null(v)) {
    v <- u
  }
  for (i in 1:length(u)) {
    soma = soma + u[i] * v[i]
  }
  return(sqrt(soma))
}


## Now, we are set for creating our function

#' @param A is a vectors matrix. Each vector holds a line.
#' @param type may be 'orthonormal' in which the final set of vectors is transformed, so it's euclidian norm is 1.
#' 
#' @return is a matrix, containing each of the orthogonalized vectors, one by line.
#' 
#' 
#' @example 
#' 
#' GS_process(matrix(c(-1,1,0,-1,0,1,1,1,1), nrow = 3, byrow = T), 'orthonormal')
#' 
#' @export

GS_process <- function(A, type = 'orthogonal') {
  w <- A
  for (i in 2:nrow(A)) {
    for (k in 1:(i-1)) {
      w[i,] <- w[i,] - (dot_product(A[i,], w[k,]) / dot_product(w[k,]) ** 2) * w[k,]
    }
  }
  if (type == 'orthonormal') {
    for (i in 1:nrow(w)) {
      w[i,] <- w[i,] / dot_product(w[i,])
    }
  }
  return(w)
}