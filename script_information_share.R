# Estimate information share and component share weights.
library(vars)
library(urca)

# The function `pdshare' implements the two techniques and returns the 
# estimates of information share and component share weights 

# Args:
#   x:              matrix / data frame which has two columns with log
#                   prices of two markets 

#   override.lags:  integer specifying user-defined lags for VECM
#                   estimation 

#   lag.max:        integer specifying the maximum number of lags for
#                   VARselect 

# Returns:
#   is.original.ordering  Information shares under the supplied ordering
#   is.reversed.ordering  Information shares under the reverse ordering
#   component.share       Component share weights
#   var.covar.matrix      Var-covar matrix of residuals
#   lags.used             Lags used in VECM estimation    


pdshare <- function(x, override.lags = NULL, lag.max = 10) {
  stopifnot(ncol(x)==2)
  stopifnot(is.numeric(x[,1]))
  stopifnot(is.numeric(x[,2]))
  if (is.null(override.lags)){
    if(lag.max<2) stop("Minimum lags should be 2")
  } else {
    if(override.lags<2) stop("Minimum lags should be 2")
  }
  cnames <- colnames(x)
  pdshare.computation <- function(x, nlag) {
    cointest <- ca.jo(x, K = nlag, type = "eigen", ecdet = "const",
                      spec = "transitory")  
    k <- cointest@lag
    vecm <- cajorls(cointest)
    varm <- vec2var(cointest)
    vma <- Psi(varm)
    ## converts level VARS to VMA model and gives orthogonalised psi
    ## matrix. 
    
    ## Head towards IS
    ## We need Psi(1), the matrix that captures the long run impact 
    ## of a disturbance on each of the prices.
    ## Psi(1) = beta.orthogonal*
    ##        [inverse(transpose(alpha.orthogonal) *gamma*
    ## (beta.orthogonal))] * transpose(alpha.orthogonal)
    
    ## the beta_orthogonal and alpha_orthogonal vectors :
    beta.ort <- as.vector(c(-cointest@V[2,1], cointest@V[1,1]))
    alpha.ort <- as.vector(c(-cointest@W[2,1], cointest@W[1,1]))
    
    ## initializing the parameters of gamma matrix
    aa <- bb <- cc <- dd <- 0
    for (i in 1:(k-1)) {
      aa <- aa + vecm$rlm$coefficients[2*i,1]
      bb <- bb + vecm$rlm$coefficients[2*i+1,1]
      cc <- cc + vecm$rlm$coefficients[2*i,2]
      dd <- dd + vecm$rlm$coefficients[2*i+1,2]
    }
    gamma.1 <- matrix(c(1-aa, -bb, -cc, 1-dd), nrow = 2, ncol = 2, byrow
                      = TRUE) 
    
    b <- as.numeric(t(alpha.ort) %*% gamma.1 %*% beta.ort)
    psi <- (beta.ort %*% t(alpha.ort))/b
    
    ## Information share is: (psi[1,]* f)_j^2) /
    ##                       (psi[1,]*omega*transpose(psi[1,])) 
    ## where f is the cholesky factorization of the omega matrix. 
    
    f <- vma[,,1]
    omega <- f %*% t(f)
    psi <- t(psi[1,])
    n <- psi %*% f
    d <- psi %*% omega %*% t(psi)
    
    list(ishares = c((n[, 1]^2)/d, (n[, 2]^2)/d), alpha.ort = alpha.ort, 
         omega = omega, lags = varm$p, 
         vecm = vecm)
  }
  
  # Choosing the number of lags
  if (is.null(override.lags)) {
    nlag <- MVARselect(x, lag.max=lag.max)$selection[1] 
  } else {
    nlag <- override.lags
  }
  
  # First do the supplied ordering
  tmp <- pdshare.computation(x, nlag)
  is.original.ordering <- as.data.frame(tmp$ishares)
  component.share <- as.data.frame(abs(tmp$alpha.ort)/sum(abs(tmp$alpha.ort)))
  alphas = as.data.frame(tmp$alpha.ort)
  var.covar.matrix <- tmp$omega
  lags.used <- tmp$lags
  
  # Do the reverse ordering
  tmp <- pdshare.computation(x[,c(2,1)], nlag)
  is.reversed.ordering <- as.data.frame(tmp$ishares)
  
  
  rownames(var.covar.matrix) <- colnames(var.covar.matrix) <-
    rownames(component.share) <- rownames(is.original.ordering) <- cnames
  rownames(is.reversed.ordering) <- c(cnames[2], cnames[1])
  colnames(is.original.ordering) <- colnames(is.reversed.ordering) <-"IS" 
  colnames(component.share) <- "CS"
  colnames(alphas) <- "alphas"
  
  list(is.original.ordering = is.original.ordering,
       is.reversed.ordering = is.reversed.ordering,
       component.share = component.share,
       var.covar.matrix = var.covar.matrix,
       lags.used = lags.used,
       alphas = alphas, 
       vecm = vecm)
}

is_bn_cb = pdshare(ts_log_bn_cb, k1_bn_cb)
is_bn_cb$is.original.ordering
is_bn_cb$is.reversed.ordering
is_bn_kk = pdshare(ts_log_bn_kk, k1_bn_kk)
is_bn_kk$is.original.ordering
is_bn_kk$is.reversed.ordering
is_cb_kk = pdshare(ts_log_cb_kk, k1_cb_kk)
is_cb_kk$is.original.ordering
is_cb_kk$is.reversed.ordering

is_bn_cb$var.covar.matrix
is_bn_kk$var.covar.matrix
is_cb_kk$var.covar.matrix

is_bn_cb$alphas
is_cb_kk$alphas
is_bn_kk$alphas

is_bn_cb$component.share
is_bn_kk$component.share
is_cb_kk$component.share


test1 = ca.jo(ts_log_bn_kk, K = k1_bn_kk, type = "eigen", ecdet = "const", spec = "transitory")
alpha =  test1@W[,1]
vecm = cajorls(test1, r=1)
beta = test1@V[,1]
alpha =  test1@W[,1]
residuals = resid(vecm$rlm)
corresiduals =cor(residuals)
corresiduals
N = nrow(residuals) # 10
sigma = crossprod(residuals)/N
beta.se = sqrt(diag(kronecker(solve(crossprod(test1@RK[,-1])), solve(t(alpha)%*%solve(sigma) %*% alpha))))
beta.t = c(NA, beta[-1]/beta.se)
alpha.se = sqrt(solve(crossprod(cbind(test1@ZK %*%beta, test1@Z1)))[1,1]*diag(sigma))
alpha.t <- alpha/alpha.se

coeftest(cajorls(ca.jo(ts_log_cb_kk, K=k1_cb_kk,type="trace", ecdet="none", spec="transitory"), r=1)$rlm)

