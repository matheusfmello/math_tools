


norm <- function(u, v = NULL) {
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


GS_process <- function(A, type = 'ortoghonal') {
  w <- A
  for (i in 2:nrow(A)) {
    for (k in 1:(i-1)) {
      w[i,] <- w[i,] - (norm(A[i,], w[k,]) / norm(w[k,]) ** 2) * w[k,]
    }
  }
  if (type == 'orthonormal') {
    for (i in 1:nrow(w)) {
      w[i,] <- w[i,] / norm(w[i,])
    }
  }
  return(w)
}

GS_process(matrix(c(-1,1,0,-1,0,1,1,1,1), nrow = 3, byrow = T), 'orthonormal')
