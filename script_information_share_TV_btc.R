install.packages(c("quantmod","rugarch","rmgarch", "vst/mgarchBEKK", "vst/mgarch"))   # only needed in case you have not yet installed these packages
library(quantmod)
library(rugarch)
library(rmgarch)
library(fGarch)
install_github("vst/mgarchBEKK")
install_github("vst/mgarch")
library(MTS)
# chartseries()
copy = ts_log_bn_cb


## TIME VARYING: INFORMATION SHARES ####

## Estimate the BEKK to replace the time-invariant covariance matrix ###
estimated <- BEKK(as.matrix(copy))
# diagnoseBEKK(estimated)
H.estimated = estimated$H.estimated


## Make Choleskey decomp for cov matrix = H.estimated and put it in a list ###
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


## After doing this, replicate script for the information shares --> but replacing the cov matrix for cholesky_list
# First do it with the supplied ordering (so)
cnames_so = colnames(copy)
cointest_so = ca.jo(copy, K = k1_bn_cb, type = "eigen", ecdet = "const", spec = "transitory")  
k_so = cointest_so@lag
vecm_so <- cajorls(cointest_so)
varm_so <- vec2var(cointest_so)
vma_so <- Psi(varm_so)
    
## the beta_orthogonal and alpha_orthogonal vectors :
beta.ort_so <- as.vector(c(-cointest_so@V[2,1], cointest_so@V[1,1]))
alpha.ort_so <- as.vector(c(-cointest_so@W[2,1], cointest_so@W[1,1]))
    
## initializing the parameters of gamma matrix
aa_so <- bb_so <- cc_so <- dd_so <- 0
for (i in 1:(k_so-1)) {
      aa_so <- aa_so + vecm_so$rlm$coefficients[2*i,1]
      bb_so <- bb_so + vecm_so$rlm$coefficients[2*i+1,1]
      cc_so <- cc_so + vecm_so$rlm$coefficients[2*i,2]
      dd_so <- dd_so + vecm_so$rlm$coefficients[2*i+1,2]
    }
gamma.1_so <- matrix(c(1-aa_so, -bb_so, -cc_so, 1-dd_so), nrow = 2, ncol = 2, byrow = TRUE) 
    
b_so <- as.numeric(t(alpha.ort_so) %*% gamma.1_so %*% beta.ort_so)
psi_so <- (beta.ort_so %*% t(alpha.ort_so))/b_so
    
## Information share is: (psi[1,]* f)_j^2) /
##                       (psi[1,]*omega*transpose(psi[1,])) 
## where f is the cholesky factorization of the omega matrix. 


f_so = list()
omega_so = list()
n_so = list()
d_so = list()
ishares_so = list()

for (i in 1:length(cholesky_list)){
    f_so[[i]] <- cholesky_list[[i]]
}

for (i in 1:length(cholesky_list)){
  omega_so[[i]] = f_so[[i]] %*% t(f_so[[i]])
}

psi_so = t(psi_so[1,])

for (i in 1:length(cholesky_list)){
  n_so[[i]] = psi_so %*% (f_so[[i]])
}

for (i in 1:length(cholesky_list)){
  d_so[[i]] = psi_so %*% omega_so[[i]] %*% t(psi_so)
}

for (i in 1:length(cholesky_list)){
  ishares_so[[i]] = c((((n_so[[i]][1:1])^2)/d_so[[i]]), 
                     (((n_so[[i]][2:2])^2)/d_so[[i]]))
}

component.share = as.data.frame(abs(alpha.ort_so)/sum(abs(alpha.ort_so)))

## REPEAT THE SAME THING AS BEFORE, WITH REVERSE ORDERING ####
copycopy = copy[,c(2,1)]

## Estimate the BEKK to replace the time-invariant covariance matrix ###
estimated <- BEKK(as.matrix(copycopy))
#diagnoseBEKK(estimated)
H.estimated_ro = estimated$H.estimated


cholesky_list_ro = list()
for (i in 1:length(H.estimated)){
  choles = cholesky_matrix(H.estimated_ro[[i]])
  cholesky_list_ro[[i]] = choles
}


# Do the reversed ordering (ro)
cnames_ro = colnames(copycopy)
cointest_ro = ca.jo(copycopy, K = k1_bn_cb, type = "eigen", ecdet = "const", spec = "transitory")  
k_ro = cointest_ro@lag
vecm_ro <- cajorls(cointest_ro)
varm_ro <- vec2var(cointest_ro)
vma_ro <- Psi(varm_ro)

## the beta_orthogonal and alpha_orthogonal vectors :
beta.ort_ro <- as.vector(c(-cointest_ro@V[2,1], cointest_ro@V[1,1]))
alpha.ort_ro <- as.vector(c(-cointest_ro@W[2,1], cointest_ro@W[1,1]))

## initializing the parameters of gamma matrix
aa_ro <- bb_ro <- cc_ro <- dd_ro <- 0
for (i in 1:(k_ro-1)) {
  aa_ro <- aa_ro + vecm_ro$rlm$coefficients[2*i,1]
  bb_ro <- bb_ro + vecm_ro$rlm$coefficients[2*i+1,1]
  cc_ro <- cc_ro + vecm_ro$rlm$coefficients[2*i,2]
  dd_ro <- dd_ro + vecm_ro$rlm$coefficients[2*i+1,2]
}
gamma.1_ro <- matrix(c(1-aa_ro, -bb_ro, -cc_ro, 1-dd_ro), nrow = 2, ncol = 2, byrow = TRUE) 

b_ro <- as.numeric(t(alpha.ort_ro) %*% gamma.1_ro %*% beta.ort_ro)
psi_ro <- (beta.ort_ro %*% t(alpha.ort_ro))/b_ro

## Information share is: (psi[1,]* f)_j^2) /
##                       (psi[1,]*omega*transpose(psi[1,])) 
## where f is the cholesky factorization of the omega matrix. 


f_ro = list()
omega_ro = list()
n_ro = list()
d_ro = list()
ishares_ro = list()
ISA_TV = list()

for (i in 1:length(cholesky_list)){
  f_ro[[i]] <- cholesky_list_ro[[i]]
}

for (i in 1:length(cholesky_list)){
  omega_ro[[i]] = f_ro[[i]] %*% t(f_ro[[i]])
}

psi_ro = t(psi_ro[1,])

for (i in 1:length(cholesky_list)){
  n_ro[[i]] = psi_ro %*% (f_ro[[i]])
}

for (i in 1:length(cholesky_list)){
  d_ro[[i]] = t(psi_ro[1,]) %*% omega_ro[[i]] %*% psi_ro[1,]
}

for (i in 1:length(cholesky_list)){
  ishares_ro[[i]] = c(((n_ro[[i]][1:1])^2)/d_ro[[i]], 
                      (((n_ro[[i]][2:2])^2)/d_ro[[i]]))
}

component.share_ro = as.data.frame(abs(alpha.ort_ro)/sum(abs(alpha.ort_ro)))

for (i in 1:length(cholesky_list)){
  ISA_TV[[i]] = c((ishares_so[[i]][1:1]+ishares_ro[[i]][2:2])/2, 
              ((ishares_so[[i]][2:2]+ishares_ro[[i]][1:1])/2))
}


isa_tv_df = data.frame(matrix(unlist(ISA_TV), nrow=length(ISA_TV), byrow=TRUE))
# sd1 = list()
# sd2 = list()
# isa_tv_1_periodo1 = list()
# isa_tv_2_periodo1 = list()
# isa_tv_1_periodo2 = list()
# isa_tv_2_periodo2 = list()

for (i in 1:length(cholesky_list)){
  if(i<=720){
    isa_tv_1_periodo1 = (sum(isa_tv_df$X1[1:720]))/720
    isa_tv_2_periodo1 = (sum(isa_tv_df$X2[1:720]))/720
  }
  else if (i >= 721 & i <= 1441) {
    isa_tv_1_periodo2 = (sum(isa_tv_df$X1[721:1441]))/720
    isa_tv_2_periodo2 = (sum(isa_tv_df$X2[721:1441]))/720
  }
  else if (i >= 1442 & i <= 2161) {
    isa_tv_1_periodo3 = (sum(isa_tv_df$X1[1442:2161]))/720
    isa_tv_2_periodo3 = (sum(isa_tv_df$X2[1442:2161]))/720
  }
  else if (i >= 2162 & i <= 2881) {
    isa_tv_1_periodo4 = (sum(isa_tv_df$X1[2162:2881]))/720
    isa_tv_2_periodo4 = (sum(isa_tv_df$X2[2162:2881]))/720
  }
  else if (i >= 2882 & i <= 3602) {
    isa_tv_1_periodo5 = (sum(isa_tv_df$X1[2882:3490]))/608
    isa_tv_2_periodo5 = (sum(isa_tv_df$X2[2882:3490]))/608
  }
  else if (i >= 3603 & i <= 4322) {
    isa_tv_1_periodo6 = (sum(isa_tv_df$X1[2162:2881]))/720
    isa_tv_2_periodo6 = (sum(isa_tv_df$X2[2162:2881]))/720
  }
  else if (i >= 4322 & i <= 5042) {
    isa_tv_1_periodo7 = (sum(isa_tv_df$X1[2162:2881]))/720
    isa_tv_2_periodo7 = (sum(isa_tv_df$X2[2162:2881]))/720
  }
  else if (i >= 5042 & i <= 5762) {
    isa_tv_1_periodo8 = (sum(isa_tv_df$X1[2162:2881]))/720
    isa_tv_2_periodo8 = (sum(isa_tv_df$X2[2162:2881]))/720
  }
  else if (i >= 6482 & i <= 7202) {
    isa_tv_1_periodo9 = (sum(isa_tv_df$X1[2162:2881]))/720
    isa_tv_2_periodo9 = (sum(isa_tv_df$X2[2162:2881]))/720
  }
  else if (i >= 7203 & i <= 7922) {
    isa_tv_1_periodo10 = (sum(isa_tv_df$X1[2162:2881]))/720
    isa_tv_2_periodo10 = (sum(isa_tv_df$X2[2162:2881]))/720
  }
  else if (i >= 7923 & i <= 8642) {
    isa_tv_1_periodo11 = (sum(isa_tv_df$X1[2162:2881]))/720
    isa_tv_2_periodo11 = (sum(isa_tv_df$X2[2162:2881]))/720
  }
  else if (i >= 8643 & i <= 9362) {
    isa_tv_1_periodo12 = (sum(isa_tv_df$X1[2162:2881]))/720
    isa_tv_2_periodo12 = (sum(isa_tv_df$X2[2162:2881]))/720
  }
  else if (i >= 9363 & i <= 10082) {
    isa_tv_1_periodo13 = (sum(isa_tv_df$X1[2162:2881]))/720
    isa_tv_2_periodo13 = (sum(isa_tv_df$X2[2162:2881]))/720
  }
  else if (i >= 10083 & i <= 10802) {
    isa_tv_1_periodo14 = (sum(isa_tv_df$X1[2162:2881]))/720
    isa_tv_2_periodo14 = (sum(isa_tv_df$X2[2162:2881]))/720
  }
  else if (i >= 10803 ) {
    isa_tv_1_periodo15 = (sum(isa_tv_df$X1[2162:2881]))/720
    isa_tv_2_periodo15 = (sum(isa_tv_df$X2[2162:2881]))/720
  }
  
  
  
  
  
  
}







# for (i in 1:length(cholesky_list)){
#   sd1 = (sd(isa_tv_df$X1))
#   sd2 = (sd(isa_tv_df$X2))
# }
