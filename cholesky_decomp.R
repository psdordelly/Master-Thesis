cholesky_matrix <- function(A){
  # http://rosettacode.org/wiki/Cholesky_decomposition#C
  
  L <- matrix(0,nrow=nrow(A),ncol=ncol(A))
  colnames(L) <- colnames(A)
  rownames(L) <- rownames(A)
  
  m <- ncol(L)
  
  
  for(i in 1:m){
    for(j in 1:i){
      s <- 0
      if(j > 1){
        for(k in 1:(j-1)){
          s <- s + L[i,k]*L[j,k]
        }
      }
      if(i == j){
        L[i,j] <- sqrt(A[i,i] - s)
      } else {
        L[i,j] <- (1 / L[j,j])*(A[i,j] - s)
      }
    }
  }
  return(L)    
}

# choles = cholesky_matrix(H.estimated[[1]])

cholesky_list = list()

for (i in 1:length(H.estimated)){
    choles = cholesky_matrix(H.estimated[[i]])
    cholesky_list[[i]] = choles
}


