### 3. COINTEGRATION ANALYSIS ####
#*********************************

## 3.1 Preliminary settings ####

# To deal with this part we will be using the logarithmic transformation of the series
# Create an object containing time series of the log series
ts_log = data_wide_log
ts_log_bn_cb = ts_log[,-3] ## DONE
ts_log_bn_kk = ts_log[,-2]
ts_log_cb_kk = ts_log[,-1]
##PARA BINANCE AND COINBASE
# Select the optimal number of lags 
lag_selection <- VARselect(ts_log_bn_cb, lag.max = 100, type = "trend")
lag_selection$selection 
#Assuming there is a cointegrating relationship -- need to substract 1 from the lag order determined using the regular VAR technique
k1_bn_cb = as.integer(mean(lag_selection$selection))-1

##PARA BINANCE AND KRAKEN
# Select the optimal number of lags 
lag_selection <- VARselect(ts_log_bn_kk, lag.max = 100, type = "trend")

#Assuming there is a cointegrating relationship -- need to substract 1 from the lag order determined using the regular VAR technique
k1_bn_kk = as.integer(mean(lag_selection$selection))-1

##PARA COINBASE AND KRAKEN
# Select the optimal number of lags 
lag_selection <- VARselect(ts_log_cb_kk, lag.max = 100, type = "trend")
lag_selection$selection 
#Assuming there is a cointegrating relationship -- need to substract 1 from the lag order determined using the regular VAR technique
k1_cb_kk = as.integer(mean(lag_selection$selection))-1


##PARA BINANCE AND COINBASE
## 3.2  Standard Residual Based Cointegration tests ####

## 3.2.1  Engle and Granger Cointegration test ####

# NOTE: function coint.test (from aTSA package) Performs Engle-Granger(or EG) tests
# Ho: two or more time series, each of which is I(1), are not cointegrated.

# Create empty list to store results of the tests
EG_test_list_bn_cb <- list() 

# This loop performs the E-G cointegration test for every column of the data frame a store results on a list object
for (i in 1:ncol(ts_log_bn_cb)) {
  EG_test_bn_cb <- coint.test(ts_log_bn_cb$log_bn_ada, as.matrix(ts_log_bn_cb[,i+1]), d = 0, nlag = 10, output = TRUE)
  EG_test_list_bn_cb[[i]] <- EG_test_bn_cb
  names(EG_test_list_bn_cb)[[i]] <- colnames(ts_log_bn_cb[,i])
}

## 3.2.2  Phillips-Ouliaris' Cointegration test ####

# The construction of this test's function, requires the dependent variable to be on the first column of the matrix
# Create empty list to store results of the tests
po_test_list_bn_cb <- list() 

# This loop performs the P-O cointegration test for every column of the data frame a store results on a list object
for (i in 1:(ncol(ts_log_bn_cb)-1)) {
  po_test_bn_cb <- po.test(as.matrix(ts_log_bn_cb[,c(1,i+1)], demean = FALSE, lshort = FALSE))
  po_test_list_bn_cb[[i]] <- po_test_bn_cb
  names(po_test_list_bn_cb)[[i]] <- colnames(ts_log_bn_cb[,i+1])
}

## 3.2.3  ADF type Cointegration test ####

# https://www.econometrics-with-r.org/16-3-cointegration.html

# Create empty list to store results of the tests
coint_ADF_test_list_bn_cb <- list()

# This loop performs the ADF cointegration test for every column of the data frame a store results on a list object
for (i in 1:(ncol(ts_log_bn_cb)-1)) {
  coint_ADF_test_bn_cb <- urca::ur.df(ts_log_bn_cb$log_bn_ada - as.matrix(ts_log_bn_cb[,i+1]), 
                                lags = 10, selectlags = "AIC", 
                                type = "none")
  coint_ADF_test_list_bn_cb[[i]] <- summary(coint_ADF_test_bn_cb)
  names(coint_ADF_test_list_bn_cb)[[i]] <- colnames(ts_log_bn_cb[,i+1])
}

# Create empty list to store results of the tests
coint_DFGLS_test_list_bn_cb <- list()

# This loop performs the DF-GLS cointegration test for every column of the data frame a store results on a list object
for (i in 1:(ncol(ts_log_bn_cb)-1)) {
  coint_DFGLS_test_bn_cb <- urca::ur.ers(ts_log_bn_cb$log_bn_ada - as.matrix(ts_log_bn_cb[,i+1]),
                                   model = "constant", lag.max = k1)
  coint_DFGLS_test_list_bn_cb[[i]] <- summary(coint_DFGLS_test_bn_cb)
  names(coint_DFGLS_test_list_bn_cb)[[i]] <- colnames(ts_log_bn_cb[,i+1])
}

## 3.3  Maximum Likelihood Based Cointegration tests ####

## 3.3.1  Johansen Trace Cointegration tests ####

johansen_trace_test_constant_bn_cb = summary(ca.jo(ts_log_bn_cb, type = "trace", ecdet= "const", K = k1_bn_cb))
johansen_trace_test_trend_bn_cb = summary(ca.jo(ts_log_bn_cb, type = "trace", ecdet= "trend", K = k1_bn_cb))
johansen_trace_test_constant_bn_cb # r = 2 there are at most 2 cointegrating relationship
johansen_trace_test_trend_bn_cb

## 3.3.2  Johansen Maximum Eigenvalue Cointegration tests ####
johansen_eigen_test_const_bn_cb = summary(ca.jo(ts_log_bn_cb, type = "eigen", ecdet= "const", K = k1))
johansen_eigen_test_trend_bn_cb = summary(ca.jo(ts_log_bn_cb, type = "eigen", ecdet= "trend", K = k1))
johansen_eigen_test_const_bn_cb # r = 2 there are at most 2 cointegrating relationship
johansen_eigen_test_trend_bn_cb

##PARA BINANCE AND KRAKEN
# Select the optimal number of lags 
lag_selection <- VARselect(ts_log_bn_kk, lag.max = 100, type = "trend")
lag_selection$selection 

#Assuming there is a cointegrating relationship -- need to substract 1 from the lag order determined using the regular VAR technique
k1 = as.integer(mean(lag_selection$selection))-1

## 3.2  Standard Residual Based Cointegration tests ####

## 3.2.1  Engle and Granger Cointegration test ####

# NOTE: function coint.test (from aTSA package) Performs Engle-Granger(or EG) tests
# Ho: two or more time series, each of which is I(1), are not cointegrated.

# Create empty list to store results of the tests
EG_test_list_bn_kk <- list() 

# This loop performs the E-G cointegration test for every column of the data frame a store results on a list object
for (i in 1:ncol(ts_log_bn_kk)) {
  EG_test_bn_kk <- coint.test(ts_log_bn_kk$log_bn_ada, as.matrix(ts_log_bn_kk[,i+1]), d = 0, nlag = k1, output = TRUE)
  EG_test_list_bn_kk[[i]] <- EG_test_bn_kk
  names(EG_test_list_bn_kk)[[i]] <- colnames(ts_log_bn_kk[,i])
}

## 3.2.2  Phillips-Ouliaris' Cointegration test ####

# The construction of this test's function, requires the dependent variable to be on the first column of the matrix
# Create empty list to store results of the tests
po_test_list_bn_kk <- list() 

# This loop performs the P-O cointegration test for every column of the data frame a store results on a list object
for (i in 1:(ncol(ts_log_bn_kk)-1)) {
  po_test_bn_kk <- po.test(as.matrix(ts_log_bn_kk[,c(1,i+1)]))
  po_test_list_bn_kk[[i]] <- po_test_bn_kk
  names(po_test_list_bn_kk)[[i]] <- colnames(ts_log_bn_kk[,i+1])
}

## 3.2.3  ADF type Cointegration test ####

# https://www.econometrics-with-r.org/16-3-cointegration.html

# Create empty list to store results of the tests
coint_ADF_test_list_bn_kk <- list()

# This loop performs the ADF cointegration test for every column of the data frame a store results on a list object
for (i in 1:(ncol(ts_log_bn_kk)-1)) {
  coint_ADF_test_bn_kk <- urca::ur.df(ts_log_bn_kk$log_bn_ada - as.matrix(ts_log_bn_kk[,i+1]), 
                                      lags = k1, selectlags = "AIC", 
                                      type = "drift")
  coint_ADF_test_list_bn_kk[[i]] <- summary(coint_ADF_test_bn_kk)
  names(coint_ADF_test_list_bn_kk)[[i]] <- colnames(ts_log_bn_kk[,i+1])
}

# Create empty list to store results of the tests
coint_DFGLS_test_list_bn_kk <- list()

# This loop performs the DF-GLS cointegration test for every column of the data frame a store results on a list object
for (i in 1:(ncol(ts_log_bn_kk)-1)) {
  coint_DFGLS_test_bn_kk <- urca::ur.ers(ts_log_bn_kk$log_bn_ada - as.matrix(ts_log_bn_kk[,i+1]),
                                         model = "constant", lag.max = k1)
  coint_DFGLS_test_list_bn_kk[[i]] <- summary(coint_DFGLS_test_bn_kk)
  names(coint_DFGLS_test_list_bn_kk)[[i]] <- colnames(ts_log_bn_kk[,i+1])
}

## 3.3  Maximum Likelihood Based Cointegration tests ####

## 3.3.1  Johansen Trace Cointegration tests ####

johansen_trace_test_constant_bn_kk = summary(ca.jo(ts_log_bn_kk, type = "trace", ecdet= "const", K = k1))
johansen_trace_test_trend_bn_kk = summary(ca.jo(ts_log_bn_kk, type = "trace", ecdet= "trend", K = k1))
johansen_trace_test_constant_bn_kk # r = 2 there are at most 2 cointegrating relationship
johansen_trace_test_trend_bn_kk

## 3.3.2  Johansen Maximum Eigenvalue Cointegration tests ####
johansen_eigen_test_const_bn_kk = summary(ca.jo(ts_log_bn_kk, type = "eigen", ecdet= "const", K = k1))
johansen_eigen_test_trend_bn_kk = summary(ca.jo(ts_log_bn_kk, type = "eigen", ecdet= "trend", K = k1))
johansen_eigen_test_const_bn_kk # r = 2 there are at most 2 cointegrating relationship
johansen_eigen_test_trend_bn_kk


##PARA COINBASE AND KRAKEN
# Select the optimal number of lags 
lag_selection <- VARselect(ts_log_cb_kk, lag.max = 100, type = "trend")
lag_selection$selection 

#Assuming there is a cointegrating relationship -- need to substract 1 from the lag order determined using the regular VAR technique
k1 = as.integer(mean(lag_selection$selection))-1


## 3.2  Standard Residual Based Cointegration tests ####

## 3.2.1  Engle and Granger Cointegration test ####

# NOTE: function coint.test (from aTSA package) Performs Engle-Granger(or EG) tests
# Ho: two or more time series, each of which is I(1), are not cointegrated.

# Create empty list to store results of the tests
EG_test_list_cb_kk <- list() 

# This loop performs the E-G cointegration test for every column of the data frame a store results on a list object
for (i in 1:ncol(ts_log_cb_kk)) {
  EG_test_cb_kk <- coint.test(ts_log_cb_kk$log_cb_ada, as.matrix(ts_log_cb_kk[,i+1]), d = 0, nlag = k1, output = TRUE)
  EG_test_list_cb_kk[[i]] <- EG_test_cb_kk
  names(EG_test_list_cb_kk)[[i]] <- colnames(ts_log_cb_kk[,i])
}

## 3.2.2  Phillips-Ouliaris' Cointegration test ####

# The construction of this test's function, requires the dependent variable to be on the first column of the matrix
# Create empty list to store results of the tests
po_test_list_cb_kk <- list() 

# This loop performs the P-O cointegration test for every column of the data frame a store results on a list object
for (i in 1:(ncol(ts_log_cb_kk)-1)) {
  po_test_cb_kk <- po.test(as.matrix(ts_log_cb_kk[,c(1,i+1)]))
  po_test_list_cb_kk[[i]] <- po_test_cb_kk
  names(po_test_list_cb_kk)[[i]] <- colnames(ts_log_cb_kk[,i+1])
}

## 3.2.3  ADF type Cointegration test ####

# https://www.econometrics-with-r.org/16-3-cointegration.html

# Create empty list to store results of the tests
coint_ADF_test_list_cb_kk <- list()

# This loop performs the ADF cointegration test for every column of the data frame a store results on a list object
for (i in 1:(ncol(ts_log_cb_kk)-1)) {
  coint_ADF_test_cb_kk <- urca::ur.df(ts_log_cb_kk$log_cb_ada - as.matrix(ts_log_cb_kk[,i+1]), 
                                      lags = k1, selectlags = "AIC", 
                                      type = "drift")
  coint_ADF_test_list_cb_kk[[i]] <- summary(coint_ADF_test_cb_kk)
  names(coint_ADF_test_list_cb_kk)[[i]] <- colnames(ts_log_cb_kk[,i+1])
}

# Create empty list to store results of the tests
coint_DFGLS_test_list_cb_kk <- list()

# This loop performs the DF-GLS cointegration test for every column of the data frame a store results on a list object
for (i in 1:(ncol(ts_log_cb_kk)-1)) {
  coint_DFGLS_test_cb_kk <- urca::ur.ers(ts_log_cb_kk$log_cb_ada - as.matrix(ts_log_cb_kk[,i+1]),
                                         model = "constant", lag.max = k1)
  coint_DFGLS_test_list_cb_kk[[i]] <- summary(coint_DFGLS_test_cb_kk)
  names(coint_DFGLS_test_list_cb_kk)[[i]] <- colnames(ts_log_cb_kk[,i+1])
}

## 3.3  Maximum Likelihood Based Cointegration tests ####

## 3.3.1  Johansen Trace Cointegration tests ####

johansen_trace_test_constant_cb_kk = summary(ca.jo(ts_log_cb_kk, type = "trace", ecdet= "const", K = k1))
johansen_trace_test_trend_cb_kk = summary(ca.jo(ts_log_cb_kk, type = "trace", ecdet= "trend", K = k1))
johansen_trace_test_constant_cb_kk # r = 2 there are at most 2 cointegrating relationship
johansen_trace_test_trend_cb_kk

## 3.3.2  Johansen Maximum Eigenvalue Cointegration tests ####
johansen_eigen_test_const_cb_kk = summary(ca.jo(ts_log_cb_kk, type = "eigen", ecdet= "const", K = k1))
johansen_eigen_test_trend_cb_kk = summary(ca.jo(ts_log_cb_kk, type = "eigen", ecdet= "trend", K = k1))
johansen_eigen_test_const_cb_kk # r = 2 there are at most 2 cointegrating relationship
johansen_eigen_test_trend_cb_kk



