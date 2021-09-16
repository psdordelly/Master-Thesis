#*************************************************************
#### THESIS - MSc. Econometrics and OR #####
##   Paola Sastre Dordelly
#*************************************************************
# Variables:
#Date of trade
#VWAP

# Exchanges chosen for this study:
# Coinbase
# Binance
# Kraken


### 0. HOUSEKEEPING ####
#***********************

## 0.1 Clear session ####

rm(list = ls())                       # Clear previous environment
if(!is.null(dev.list())) dev.off()    # Clear previous plots

## 0.2 Set working directory ####

setwd("~/Desktop/Thesis/Data/Kaiko/Panels(Clean)")

## 0.3 Download needed packages iff they are not installed yet  ####

pckg = c("readxl","dplyr","ggplot2","tidyverse", "forecast",
         "xts","aTSA", "urca", "tseries", "stats", "vars",
         "tsDyn", "dynlm", "cointReg", "ecm", "lubridate", "CADFtest", "devtools", 
         "MASS", "magrittr") 

is.installed = function(mypkg){
  is.element(mypkg, installed.packages()[,1])
} 
for(i in 1:length(pckg)) {
  if (!is.installed(pckg[i])){
    install.packages(pckg[i])
  }
}
lapply(pckg, require, character.only = TRUE)     # Load libraries 

## NOTE: to install punitroots, first install the CADFtest, devtools and then run the following command: install_github('rforge/punitroots/pkg')

## 0.4 Read cvs files with required data ####
bn_df = read.csv("binance_panel.csv", header=TRUE)
cb_df = read.csv("coinbase_panel.csv", header=TRUE)
kk_df = read.csv("kraken_panel.csv", header=TRUE)

# ## 0.5 Create necessary data frame: data_complete  ####
# lapply(kk_df, function(x) length(unique(x))) 
# missing_values <- kk_df %>% summarize_all(funs(sum(is.na(.))))

data = btc_panel %>%
  drop_na()

test = ymd_hms(data$V1)

data_copy = data
### 0.7. ANALYSIS OF MOMENTS ####
#*****************************************
data_copy$V1 = NULL
summary(data_copy)
sapply(data_copy, sd)
sapply(data_copy, skewness)
sapply(data_copy, kurtosis)


## 0.6 Create necessary data frame: data_complete  ####
diff_fun = function(x){
  c(NA,diff(x))
} 


data_complete = data %>% dplyr::mutate(start_time = test) %>%
  dplyr::mutate(across(where(is.numeric), 
                       ~if_else(condition = is.na(.), 
                                true = mean(., na.rm = T), 
                                false = as.numeric(.)))) %>%
  dplyr::mutate(across(where(is.numeric), 
                       ~log(.), 
                       .names = "log_{.col}")) %>%
  dplyr::mutate(across(where(is.numeric),
                       ~diff_fun(.),
                       .names = "d_{.col}"))

data_complete$V1 = data_complete$start_time
data_complete$start_time = NULL


# pdata_transformed = pdata_limpio %>% 
#   dplyr::mutate(across(where(is.numeric), 
#                        ~log(.), 
#                        .names = "log_{.col}")) %>%
#   dplyr::mutate(across(where(is.numeric),
#                        ~diff_fun(.),
#                        .names = "d_{.col}"))
# 
# pdata_level = pdata_transformed[1:7]
# pdata_log = pdata_transformed[8:14]
# pdata_diff = pdata_transformed[15:21]
# pdata_d_log = pdata_transformed[22:28]
# 
# pdata_diff =  pdata_diff %>%
#   drop_na()
# 
# pdata_d_log = pdata_d_log %>%
#   drop_na()

### 1. GRAPHICAL ANALYSIS OF THE DATA ####
#*****************************************

## 1.1 Plot the series in levels ####

# Filter main data frame (i.e. "data_complete") by country of interest and convert the data to long format using tidyverse "gather"
data_long = data_complete %>% 
  dplyr::select(V1, bn_btc, cb_btc, kk_btc) %>% #
  gather(key = "variable", value = "value", -V1)

data_long$value = as.numeric(data_long$value)

# # Data visualization using ggplot2

graph_levels = ggplot(data_long, aes(x = V1, y = value)) +
  geom_line(aes(color = variable, linetype = variable)) +
  facet_wrap(.~variable, ncol=3, scales="free")                   # multiple plots on the same layout

# mapping:
graph_levels = graph_levels + theme_minimal() +                  # add theme
  theme(legend.position = "none",
        strip.text = element_text(size = 8),
        strip.text.y = element_text(angle = 0)) +                 # legend settings
  xlab('Observations') + ylab("Values") +                         # Axes labels
  theme(plot.title = element_text(margin=margin(b = 3,
                                                unit = "mm"))) +  # title settings (margins)
  ggtitle("Ripple (btc): Level Series") +                                             # Tittle
  theme(plot.title = element_text(hjust = 0.5, size=15,
                                  face="bold", color="black"))    #Tittle setting

graph_levels

## 1.2 Plot the series in logs  ####

# Recall: To get variance stationarity: take logarithms of a series

# Filter main data frame (i.e. "data_complete") by country of interest and convert the data to long format using tidyverse "gather"
data_long_log = data_complete %>%
  dplyr::select(V1, starts_with("log_")) %>%
  gather(key = "variable", value = "value", -V1)

data_long_log$value = as.numeric(data_long_log$value)
# 
# # Data visualization using ggplot2

graph_log = ggplot(data_long_log, aes(x = V1, y = value)) +
  geom_line(aes(color = variable, linetype = variable)) +
  facet_wrap(.~variable, ncol=3, scales="free")                   # multiple plots on the same layout

# mapping:
graph_log = graph_log + theme_minimal() +                        # add theme
  theme(legend.position = "none",
        strip.text = element_text(size = 8),
        strip.text.y = element_text(angle = 0)) +                 # legend settings
  xlab('Observations') + ylab("Values") +                         # Axes labels
  theme(plot.title = element_text(margin=margin(b = 3,
                                                unit = "mm"))) +  # title settings (margins)
  ggtitle("Ripple (btc): Log Series") +                                         # Tittle
  theme(plot.title = element_text(hjust = 0.5, size=15,
                                  face="bold", color="black"))    #Tittle setting

graph_log


# ## 1.3 Plot the series in first differences  ####
# 
# # Recall: To get mean stationarity: take first differences
# # NOTE: Taking first differences produces one NA value for the first observation.

# Filter main data frame (i.e. "data_complete") by country of interest and convert the data to long format using tidyverse "gather"
data_long_diff = data_complete %>%
  dplyr::select(V1,  d_bn_btc,  d_cb_btc, d_kk_btc) %>%
  gather(key = "variable", value = "value", -V1)

data_long_diff$value = as.numeric(data_long_diff$value)

# # Data visualization using ggplot2

graph_diff = ggplot(data_long_diff, aes(x = V1, y = value)) +
  geom_line(aes(color = variable, linetype = variable)) +
  facet_wrap(.~variable, ncol=3, scales="free")                   # multiple plots on the same layout

# mapping:
graph_diff = graph_diff + theme_minimal() +                      # add theme
  theme(legend.position = "none",
        strip.text = element_text(size = 8),
        strip.text.y = element_text(angle = 0)) +                 # legend settings
  xlab('Observations') + ylab("Values") +                         # Axes labels
  theme(plot.title = element_text(margin=margin(b = 3,
                                                unit = "mm"))) +  # title settings (margins)
  ggtitle("Ripple (btc): Differentiated Series") +              # Tittle
  theme(plot.title = element_text(hjust = 0.5, size=15,
                                  face="bold", color="black"))    #Tittle setting

graph_diff


# ## 1.4 Plot the series in first differences of the log ####
# 
# # Recall: Combining both effects we get the growth rates of the series

# Filter main data frame (i.e. "data_complete") by country of interest and convert the data to long format using tidyverse "gather"
data_long_dlog = data_complete %>%
  dplyr::select(V1, starts_with("d_log_")) %>%
  gather(key = "variable", value = "value", -V1)

data_long_dlog$value = as.numeric(data_long_dlog$value)
# 
# # Data visualization using ggplot2
# 
graph_dlog = ggplot(data_long_dlog, aes(x = V1, y = value)) +
  geom_line(aes(color = variable, linetype = variable)) +
  facet_wrap(.~variable, ncol=3, scales="free")                   # multiple plots on the same layout

# mapping:
graph_dlog = graph_dlog + theme_minimal() +                      # add theme
  theme(legend.position = "none",
        strip.text = element_text(size = 8),
        strip.text.y = element_text(angle = 0)) +                 # legend settings
  xlab('Observations') + ylab("Values") +                         # Axes labels
  theme(plot.title = element_text(margin=margin(b = 3,
                                                unit = "mm"))) +  # title settings (margins)
  ggtitle("Ripple (btc): Differenciated Log Series") +                 # Tittle
  theme(plot.title = element_text(hjust = 0.5, size=15,
                                  face="bold", color="black"))    #Tittle setting

graph_dlog
# 
# 
# ## 1.6 Plot ACF and PACF functions. ####
# 
# # NOTE: acf() and pacf() functions show confident intervals at 95%.
# 
# # Convert the data to wide format using tidyverse "spread"
# # Convert the data to wide format using tidyverse "spread"
data_wide_level = data_complete[2:4]  %>% na.omit() %>% as.data.frame()
data_wide_log = data_complete[5:7]   %>% na.omit() %>% as.data.frame()
data_wide_diff  = data_complete[8:10] %>% na.omit() %>% as.data.frame()
data_wide_dlog = data_complete[11:13]  %>% na.omit() %>% as.data.frame()

# Create a list with wide format data frames
datalist = list(#data_wide_level,
                #data_wide_diff,
                data_wide_log,
                data_wide_dlog)

# Iterate over the data frames to get the ACF

par(mfrow=c(2,3)) # to display multiple plots at once

for (i in 1:length(datalist)){
  n = length(datalist[[i]])
  for(j in 1:n){
    acf(datalist[[i]][[j]],
        main = substitute(paste('ACF of ', name), list(name = names(datalist[[i]])[[j]])),
        plot = TRUE, drop.lag.0 = TRUE,
        lag.max = 800) ## 800 lags correspond to 33 days
  }}

# Iterate over the data frames to get the PACF
par(mfrow=c(2,3)) # to display one plot at a time

for (i in 1:length(datalist)){
  n = length(datalist[[i]])
  for(j in 1:n){
    pacf(datalist[[i]][[j]],
         main = substitute(paste('PACF of ', name), list(name = names(datalist[[i]])[[j]])),
         plot = TRUE, drop.lag.0 = TRUE,
         lag.max = 800) ## 800 lags correspond to 33 days
  }}


### 2. ANALYSIS OF THE ORDER OF INTEGRATION ####
#***********************************************

### 2.1 Stationarity Tests ####
#***********************************************
## 2.0.1 Ljun-Box Test for Unit Roots ####
#"Ljun-Box" test is ommited: this test the series being white noise (which is a very strong/restrictive test. Use the ADF/KPSS instead)
#box.test(lag=,type="Ljun-Box")

## 2.1.1 Standard Dickey-Fuller Test for Unit Roots ####
datalist = list(data_wide_log, data_wide_dlog)

# NOTE: function adf.test (from tseries package) runs the Standard Dickey-Fuller Test when k equals zero
# Function: adf.test(x, alternative = c("stationary", "explosive"), k = trunc((length(x)-1)^(1/3)))
# The null hypothesis for SDF tests is that the data are non-stationary.
# We want to REJECT the null hypothesis for this test, so we want a p-value of less that 0.05 (or smaller).

# Create empty list to store results of the tests
sdftest_list = list(sdftest_log = list(),
                    sdftest_dlog = list())

# Iterate over each variable and each transformation, stores results of the tests
for (i in 1:length(datalist)){
  n = length(datalist[[i]])
  for(j in 1:n){
    sdftest = tseries::adf.test(ts(datalist[[i]][[j]]), alternative = "stationary", k = 0)
    sdftest_list[[i]][[j]] = sdftest
    names(sdftest_list[[i]])[[j]] = names(datalist[[i]])[[j]]
  }}

# 2.1.1.a Conclusions of the Standard Dickey-Fuller Test for Unit Roots ####
# NOTE 1: Same happens (Null hypothesis not rejected) for the list containing the data in logs
# NOTE 2: Null failed rejected for the lists containing the data in differences
# NOTE 3: According to this test, we need to use the first difference transformation to deal with stationarity.
# In other words, the original series are integrated of order one= I(1)


## 2.1.2 Augmented Dickey-Fuller Test for Unit Roots ####
# NOTE: function adf.test (from aTSA package) runs the Augmented Dickey Fuller Test on three alternatives:
# -> Type 1: no drift no trend
# -> Type 2: with drift no trend
# -> Type 3: with drift and trend
# For each output there are three columns: "lag"; "ADF" containing ADF t-statistic; and "p.value"
# Null: Non-stationary \\ alternative: stationary

# Create empty list to store results of the tests
adftest_list = list(adftest_log = list(),
                    adftest_dlog = list())

# Iterate over each variable and each transformation, stores results of the tests

for (i in 1:length(datalist)){
  n = length(datalist[[i]])
  for(j in 1:n){
    adftest = aTSA::adf.test(datalist[[i]][[j]])
    adftest_list[[i]][[j]] = adftest
    names(adftest_list[[i]])[[j]] = names(datalist[[i]])[[j]]
  }}

# 2.1.2.a Conclusions of the Augmented Dickey-Fuller Test for Unit Roots ####
# Null hypothesis non rejected at all types (drift+no trend, no drift+trend, drift+trend) for all the series at levels
# NOTE 1: Same happens (Null hypothesis rejected) for the list containing the data in logs
# NOTE 2: Null failed to be rejected for the lists containing the data in differences
# NOTE 3: According to this test, we need to use the first difference transformation to deal with stationarity.
# In other words, the original series are integrated of order one= I(1)


#NOTE: RUN THE ADF TESTS WITH THE URCA PACKAGE => IT MIGHT PROVIDE MORE INFORMATION: https://nwfsc-timeseries.github.io/atsa-labs/sec-boxjenkins-aug-dickey-fuller.html

## 2.1.3 Phillips-Perron Test ####

#Also consists of evaluating whbtcer a time series is first order trend stationary
# NOTE: function pp.test (from tseries package) runs with null hypotheis = present unit root and was not stationary
# differencey=c+beta*t+omega*yt-1+et
# lags = "short" as a default

# Create empty list to store results of the tests
pptest_list = list(pptest_log = list(),
                   pptest_dlog = list())

# Iterate over each variable and each transformation, stores results of the tests

for (i in 1:length(datalist)){
  n = length(datalist[[i]])
  for(j in 1:n){
    pptest = tseries::pp.test(ts(datalist[[i]][[j]]), alternative='stationary',type='Z(t_alpha)')
    pptest_list[[i]][[j]] = pptest
    names(pptest_list[[i]])[[j]] = names(datalist[[i]])[[j]]
  }}


# 2.1.3.a Conclusions of the Phillip Perron Test for Unit Roots ####
# NOTE 1: Same happens (Null hypothesis not rejected) for the list containing the data in logs
# NOTE 2: Null failed to be rejected for the lists containing the data in differences
# NOTE 3: According to this test, we need to use the first difference transformation to deal with stationarity.
# In other words, the original series are integrated of order one= I(1)


## 2.1.4 KPSS Test ####
# NOTE: function ur.kpss (from urca package)
# Tests for (Ho:) trend-stationarity (e.g. stationary around a deterministic trend) against the alternative of a unit root
# Create empty list to store results of the tests
kpsstest_list = list(kpsstest_log = list(),
                     kpsstest_dlog = list())

# Iterate over each variable and each transformation, stores results of the tests
for (i in 1:length(datalist)){
  n = length(datalist[[i]])
  for(j in 1:n){
    kpsstest = summary(urca::ur.kpss(datalist[[i]][[j]], type="tau", lags="long"))
    kpsstest_list[[i]][[j]] = kpsstest
    names(kpsstest_list[[i]])[[j]] = names(datalist[[i]])[[j]]
  }}


## 2.1.5 Zivot-Andrews test ####
# NOTE: Performs the unit root test, which allows a break at an unknown point in either the intercept, the linear trend or in both.
# Create empty list to store results of the tests
zatest_list = list(zatest_log = list(),
                   zatest_dlog = list())

# Iterate over each variable and each transformation, stores results of the tests
for (i in 1:length(datalist)){
  n = length(datalist[[i]])
  for(j in 1:n){
    zatest = summary(urca::ur.za(datalist[[i]][[j]], model= c("intercept", "trend", "both"), lag = NULL)) # lag: The highest number of lagged endogenous differenced variables to be included in the test regression
    zatest_list[[i]][[j]] = zatest
    names(zatest_list[[i]])[[j]] = names(datalist[[i]])[[j]]
  }}

# ### 2.2 First Generation Panel Unit Root Tests ####
# #***********************************************
# # The command to perform panel unit root tests in plm is purtest(), whose basic usage is
# # purtest(object, test = c("levinlin", "ips", "madwu", "hadri"),
# #         exo = c("none", "intercept", "trend"),
# #         lags = c("SIC", "AIC", "Hall"), pmax = 10)
# 
# # The command to perform panel unit root test in punitroots is pCADFtest(), whose basic usage is
# # pCADFtest(Y, X=NULL, covariates=NULL, crosscorr=0.10, type="trend",
# #           max.lag.y=1, min.lag.X=0, max.lag.X=0,
# #           criterion=c("none", "BIC", "AIC", "HQC", "MAIC"), ...)
# 
# # See documentation here: https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/inst/doc/panelUnitRootWithR.pdf?root=punitroots
# 
# ## 2.2.1 Levin-Lin-Chu Unit-Root Test ####
# llctest = summary(plm::purtest(pdata_log, test = "levinlin", exo = "trend", lags = "AIC", pmax = 100)) #Trend or intercept?
# llctest_diff = summary(plm::purtest(pdata_d_log, test = "levinlin", exo = "trend", lags = "AIC", pmax = 100)) #Trend or intercept?
# 
# ## 2.2.2 Im-Pesaran-Shin Unit-Root Test ####
# ipstest = summary(plm::purtest(pdata_log, test = "ips", exo = "trend", lags = "AIC", pmax = 100))
# ipstest_diff = summary(plm::purtest(pdata_d_log, test = "ips", exo = "trend", lags = "AIC", pmax = 100))
# 
# ## 2.2.3 Inverse chi-squared test (Maddala and Wu) ####
# madwutest = summary(plm::purtest(pdata_log, test = "madwu", exo = "trend", lags = "AIC", pmax = 100))
# madwutest_diff = summary(plm::purtest(pdata_d_log, test = "madwu", exo = "trend", lags = "AIC", pmax = 100))
# 
# ## 2.2.4 Hardi Test_ Panel Data Stationarity  ####
# harditest = summary(plm::purtest(pdata_log, test = "hadri", exo = "trend"))
# harditest_diff = summary(plm::purtest(pdata_log, test = "hadri", exo = "trend"))
# 
# ## 2.2.4 Choi's (2001) modified P Unit-Root Test  ####
# # "Pm" is the modified P test proposed by Choi (2001) for large N,
# pmtest = summary(plm::purtest(pdata_log, test = "Pm", exo = "trend", lags = "AIC", pmax = 100))
# pmtest_diff = summary(plm::purtest(pdata_d_log, test = "Pm", exo = "trend", lags = "AIC", pmax = 100))
# 
# ## 2.2.4 Choi's (2001) modified P Unit-Root Test  ####
# choi = summary(pCADFtest(pdata_log, type = "trend", max.lag.y = 100, criterion = "AIC"))
# choi_diff = summary(pCADFtest(pdata_d_log, type = "trend", max.lag.y = 100, criterion = "AIC"))
# 
# ### 2.2 Second Generation Panel Unit Root Tests ####
# #***********************************************
# # Cross-sectionally augmented Im, Pesaran and Shin (IPS) test for unit roots in panel models.
# # cipstest(
# #   x,
# #   lags = 2,
# #   type = c("trend", "drift", "none"),
# #   model = c("cmg", "mg", "dmg"),
# #   truncated = FALSE,
# #   ...
# # )
# # See documentation here: https://www.rdocumentation.org/packages/plm/versions/2.4-1/topics/cipstest
# ##to do : perfom the plm::cipstest
# # datalist = list(pdata_log,
# #                 pdata_d_log)
# #
# # # Create empty list to store results of the tests
# # cips_list = list(cipstest_log = list(),
# #                  cipstest_dlog = list())
# #
# # # Iterate over each variable and each transformation, stores results of the tests
# # for (i in 1:length(datalist)){
# #   n = length(datalist[[i]])
# #   for(j in 1:n){
# #     cipstest = summary(plm::cipstest(datalist[[i]][[j]], lags = 5, type = "trend", model = "cmg"))
# #     cipstest_list[[i]][[j]] = cipstest
# #     names(cipstest_list[[i]])[[j]] = names(datalist[[i]])[[j]]
# #   }
# # }
# 
# 
# pesaran = plm::cipstest(pdata_limpio_index$bch, type = "trend", truncated = FALSE)
# pesaran = plm::cipstest(pdata_log$log_bch, lags = 2, type = "trend")
# 
# ## esto no está funcionando. Lo que sigue es un ejemplo
# # data("Produc", package = "plm")
# # Produc <- pdata.frame(Produc, index=c("state", "year"))
# # ## check whbtcer the gross state product (gsp) is trend-stationary
# # cipstest(Produc$emp, type = "trend")
# 
# ## 2.6 Serial Correlation tests ####
# # Null hypothesis of independence in a given time series
# # Create a list containing just wide format of dlog variables
# list_wide_dlog = list(data_wide_dlog)
# 
# # Create empty list to store results of the tests
# serialcorr_list <- list()
# 
# # Iterate over each variable and each transformation, stores results of the tests
# 
# for(j in 1:length(list_wide_dlog[[1]])){
#   scorrtest <- stats::Box.test(list_wide_dlog[[1]][[j]], lag = 1, type = "Ljung-Box", fitdf = 0)
#   serialcorr_list[[j]] <- scorrtest
#   names(serialcorr_list[[j]]) <- colnames(data_wide_dlog)[j]
# }
# 
# 
# ### 3. COINTEGRATION ANALYSIS ####
# #*********************************
# 
# ## 3.1 Preliminary settings ####
# 
# # To deal with this part we will be using the logarithmic transformation of the series
# # Create an object containing time series of the log series
# 
# ts_log = data_wide_log
# 
# # Select the optimal number of lags
# 
# lag_selection <- VARselect(ts_log, lag.max = 100, type = "const")
# lag_selection$selection
# 
# #Assuming there is a cointegrating relationship -- need to substract 1 from the lag order determined using the regular VAR technique
# k1 = as.integer(mean(lag_selection$selection))-1
# 
# 
# ## 3.2  Standard Residual Based Cointegration tests ####
# 
# ## 3.2.1  Engle and Granger Cointegration test ####
# 
# # NOTE: function coint.test (from aTSA package) Performs Engle-Granger(or EG) tests
# # Ho: two or more time series, each of which is I(1), are not cointegrated.
# 
# # Create empty list to store results of the tests
# EG_test_list <- list()
# 
# # This loop performs the E-G cointegration test for every column of the data frame a store results on a list object
# for (i in 1:ncol(ts_log)) {
#   EG_test <- coint.test(ts_log$log_kk_btc, as.matrix(ts_log[,i+1]), d = 0, nlag = k1, output = TRUE)
#   EG_test_list[[i]] <- EG_test
#   names(EG_test_list)[[i]] <- colnames(ts_log[,i])
# }
# 
# ## 3.2.2  Phillips-Ouliaris' Cointegration test ####
# 
# # The construction of this test's function, requires the dependent variable to be on the first column of the matrix
# ts_log_po <- ts_log %>% dplyr::relocate(log_kk_btc, .before = log_bn_btc)
# 
# # Create empty list to store results of the tests
# po_test_list <- list()
# 
# # This loop performs the P-O cointegration test for every column of the data frame a store results on a list object
# for (i in 1:(ncol(ts_log_po)-1)) {
#   po_test <- po.test(as.matrix(ts_log_po[,c(1,i+1)]))
#   po_test_list[[i]] <- po_test
#   names(po_test_list)[[i]] <- colnames(ts_log_po[,i+1])
# }
# 
# ## 3.2.3  ADF type Cointegration test ####
# 
# # https://www.econometrics-with-r.org/16-3-cointegration.html
# 
# # Create empty list to store results of the tests
# coint_ADF_test_list <- list()
# 
# # This loop performs the ADF cointegration test for every column of the data frame a store results on a list object
# for (i in 1:(ncol(ts_log_po)-1)) {
#   coint_ADF_test <- urca::ur.df(ts_log$log_kk_btc - as.matrix(ts_log_po[,i+1]),
#                                 lags = k1,
#                                 selectlags = "AIC",
#                                 type = "drift")
#   coint_ADF_test_list[[i]] <- summary(coint_ADF_test)
#   names(coint_ADF_test_list)[[i]] <- colnames(ts_log_po[,i+1])
# }
# 
# # Create empty list to store results of the tests
# coint_DFGLS_test_list <- list()
# 
# # This loop performs the DF-GLS cointegration test for every column of the data frame a store results on a list object
# for (i in 1:(ncol(ts_log_po)-1)) {
#   coint_DFGLS_test <- urca::ur.ers(ts_log$log_GDP - as.matrix(ts_log_po[,i+1]),
#                                    model = "constant", lag.max = k1)
#   coint_DFGLS_test_list[[i]] <- summary(coint_DFGLS_test)
#   names(coint_DFGLS_test_list)[[i]] <- colnames(ts_log_po[,i+1])
# }
# 
# ## 3.3  Maximum Likelihood Based Cointegration tests ####
# 
# ## 3.3.1  Johansen Trace Cointegration tests ####
# 
# johansen_trace_test = summary(ca.jo(ts_log, type = "trace", ecdet= "const", K = k1))
# johansen_trace_test # r = 2 there are at most 2 cointegrating relationship
# 
# ## 3.3.2  Johansen Maximum Eigenvalue Cointegration tests ####
# 
# johansen_eigen_test <- ca.jo(ts_log, type = "eigen", ecdet= "const", K = k1)
# summary(johansen_eigen_test) # r = 2 there are at most 2 cointegrating relationship
# 
# # The cointegrating relationships identified on both tests is 4
# rank = 2
# 
# ## 3.4  SOME OTHER Cointegration tests ####
# ## 3.4.1 Gregory and Hansen
# ## Residual-based tests for cointegration in models with regime shifts
# 
# 
# 
# 
## 3.5  Cointegrating regressions ####

#Computes either the Phillips and Hansen (1990) Fully Modified OLS estimator,
#or the Saikkonen (1990) Dynamic OLS estimator,
#or the Vogelsang and Wagner (2014) Integrated Modified OLS estimator.
#We regresses the vector #log_GDP# on the matrix containing the logs of the variables #precipitation, radiation, temperature, population, agri_land, crop_index#
y = as.data.frame(ts_log_po$log_kk_btc) #dependent
x = ts_log_po %>% dplyr::select(-log_kk_btc) #independent

## 3.5.4  ECM mbtcod ####
#Assume all predictors are needed in the equilibrium and transient terms of ecm
x_ecm = as.matrix(x)
y_ecm = as.matrix(y)
xeq = xtr = x_ecm
ECM = ecmback(ts_log$log_kk_btc, xeq, xtr, includeIntercept=TRUE)
#Assume predictors: are needed in the equilibrium and transient terms of ecm
data_vecm <- data_wide_diff %>% dplyr::relocate(d_kk_btc, .before = d_bn_btc)
vecm = VECM (data_vecm, k1, 1) ## THIS IS SENSITIVE TO THE ORDER

## DIAGNOSTIC TESTS ####
## Transform VECM to VAR
model1VAR = vec2var(johansen_eigen_test)
Serial = serial.test(model1VAR, lags.pt = k1, type = "PT.asymptotic")
## ARCH EFFECTS:: the alternative hypothesis is that ARCH(lag) effects are present
arch1 = arch.test(model1VAR, lags.multi = 15, multivariate.only = TRUE )
## Normality of residuals
normal1 = normality.test(model1VAR, multivariate.only = TRUE)
## impulse response function
irf1 = irf(model1VAR, impulse = "log_kk_btc", response = "log_bn_btc", n.ahead = 20, boot = TRUE)
plot(irf1, ylab = "log_bn_btc", main = "KK shoch to BN")

irf2 = irf(model1VAR, impulse = "log_kk_btc", response = "log_cb_btc", n.ahead = 20, boot = TRUE)
plot(irf2, ylab = "log_cb_btc", main = "KK shoch to CB")

## Variance Decomposition
FEVD1 = fevd(model1VAR, n.ahead = 10)
plot(FEVD1)

#gg1 = gon.gra(johansen_eigen_test)



