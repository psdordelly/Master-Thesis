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

setwd("/Users/paolas.dordelly/Desktop/Thesis")


## 0.3 Download needed packages iff they are not installed yet  ####

pckg = c("readxl","dplyr","ggplot2","tidyverse", "forecast",
         "xts","aTSA", "urca", "tseries", "stats", "vars",
         "tsDyn", "dynlm", "cointReg", "ecm", "plm", "Rcpp", 
         "pco", "mice", "collapse") 

is.installed <- function(mypkg){
  is.element(mypkg, installed.packages()[,1])
} 
for(i in 1:length(pckg)) {
  if (!is.installed(pckg[i])){
    install.packages(pckg[i])
  }
}
lapply(pckg, require, character.only = TRUE)     # Load libraries 

## 0.4 Read cvs files with required data ####

## 0.4.1 BINANCE CLEANING ####
setwd("/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Binance_1H/binance_2021/binance_unziped")

### BINANCE 2021 TIME SERIES
bn_adaeur2021  = read.csv("bn_adaeur_vwap_1h_2021.csv", header=FALSE)
bn_bcheur2021  = read.csv("bn_bcheur_vwap_1h_2021.csv", header=FALSE)
bn_btceur2021 = read.csv("bn_btceur_vwap_1h_2021.csv", header=FALSE)
bn_eosheur2021 = read.csv("bn_eoseur_vwap_1h_2021.csv", header=FALSE)
bn_etheur2021  = read.csv("bn_etheur_vwap_1h_2021.csv", header=FALSE)
bn_linkeur2021 = read.csv("bn_linkeur_vwap_1h_2021.csv", header=FALSE)
bn_ltceur2021  = read.csv("bn_ltceur_vwap_1h_2021.csv", header=FALSE)
bn_xlmeur2021 = read.csv("bn_xlmeur_vwap_1h_2021.csv", header=FALSE)
bn_xrpeur2021  = read.csv("bn_xrpeur_vwap_1h_2021.csv", header=FALSE)

setwd("/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Binance_1H/binance_2020/binance_unziped")
### BINANCE 2020 TIME SERIES
bn_adaeur2020  = read.csv("bn_adaeur_vwap_1h_2020.csv", header=FALSE)
bn_bcheur2020  = read.csv("bn_bcheur_vwap_1h_2020.csv", header=FALSE)
bn_btceur2020 = read.csv("bn_btceur_vwap_1h_2020.csv", header=FALSE)
bn_etheur2020  = read.csv("bn_etheur_vwap_1h_2020.csv", header=FALSE)
bn_linkeur2020 = read.csv("bn_linkeur_vwap_1h_2020.csv", header=FALSE)
bn_ltceur2020  = read.csv("bn_ltceur_vwap_1h_2020.csv", header=FALSE)
bn_xlmeur2020 = read.csv("bn_xlmeur_vwap_1h_2020.csv", header=FALSE)
bn_xrpeur2020  = read.csv("bn_xrpeur_vwap_1h_2020.csv", header=FALSE)

#rename colums of each 2020 TS
colnames(bn_adaeur2020)[2]= "bn_ada"
colnames(bn_bcheur2020)[2]= "bn_bch"
colnames(bn_btceur2020)[2]= "bn_btc"
colnames(bn_etheur2020)[2]= "bn_eth"
colnames(bn_linkeur2020)[2]="bn_link"
colnames(bn_ltceur2020)[2]= "bn_ltc"
colnames(bn_xlmeur2020)[2]= "bn_xlm"
colnames(bn_xrpeur2020)[2]= "bn_xrp"

#rename colums of each 2021 TS
colnames(bn_adaeur2021)[2]= "bn_ada"
colnames(bn_bcheur2021)[2]= "bn_bch"
colnames(bn_btceur2021)[2]= "bn_btc"
colnames(bn_eosheur2021)[2]= "bn_eos"
colnames(bn_etheur2021)[2]= "bn_eth"
colnames(bn_linkeur2021)[2]= "bn_link"
colnames(bn_ltceur2021)[2]= "bn_ltc"
colnames(bn_xlmeur2021)[2]= "bn_xlm"
colnames(bn_xrpeur2021)[2]= "bn_xrp"

### Stacking 2020 onto 2021
bn_ada = rbind(bn_adaeur2020, bn_adaeur2021)
bn_bch = rbind(bn_bcheur2020, bn_bcheur2021)
bn_btc = rbind(bn_btceur2020, bn_btceur2021)
bn_eos = bn_eosheur2021
bn_ethe = rbind(bn_etheur2020, bn_etheur2021)
bn_link = rbind(bn_linkeur2020, bn_linkeur2021)
bn_ltc = rbind(bn_ltceur2020, bn_ltceur2021)
bn_xlm = rbind(bn_xlmeur2020, bn_xlmeur2021)
bn_xrp = rbind(bn_xrpeur2020, bn_xrpeur2021)

test = merge(bn_ada, bn_bch, by="V1", all = TRUE)
test = merge(test, bn_btc, by="V1", all = TRUE)
test = merge(test, bn_eos, by="V1", all = TRUE)
test = merge(test, bn_ethe, by="V1", all = TRUE)
test = merge(test, bn_link, by="V1", all = TRUE)
test = merge(test, bn_ltc, by="V1", all = TRUE)
test = merge(test, bn_xlm, by="V1", all = TRUE)
test = merge(test, bn_xrp, by="V1", all = TRUE)

# Create a sequence of timestamps
timestamp = seq(as.POSIXct("2020-1-1 0:00"), as.POSIXct("2021-4-23 18:00"), by ="hour")

###Final data BINANCE 
test$V1 = timestamp
binance_panel = test

#Check Unique Values Per Column
lapply(binance_panel, function(x) length(unique(x))) 

not_missing_values <- binance_panel %>% dplyr::summarize_all(funs(sum(!is.na(.))))

not_missing_values <- gather(not_missing_values, key="feature", value="missing_pct")

#write.csv(binance_panel,"/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Panels(Clean)\\binance_panel.csv", row.names = FALSE)
  
## 0.4.2 COINBASE CLEANING ####
setwd("/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Coinbase_1H/coinbase_2021 /coinbase_unziped")

### COINBASE 2021 TIME SERIES
cb_adaeur2021  = read.csv("cb_adaeur_vwap_1h_2021.csv", header=FALSE)
cb_bcheur2021  = read.csv("cb_bcheur_vwap_1h_2021.csv", header=FALSE)
cb_btceur2021 = read.csv("cb_btceur_vwap_1h_2021.csv", header=FALSE)
cb_eosheur2021 = read.csv("cb_eoseur_vwap_1h_2021.csv", header=FALSE)
cb_etheur2021  = read.csv("cb_etheur_vwap_1h_2021.csv", header=FALSE)
cb_linkeur2021 = read.csv("cb_linkeur_vwap_1h_2021.csv", header=FALSE)
cb_ltceur2021  = read.csv("cb_ltceur_vwap_1h_2021.csv", header=FALSE)
cb_xlmeur2021 = read.csv("cb_xlmeur_vwap_1h_2021.csv", header=FALSE)
cb_xrpeur2021  = read.csv("cb_xrpeur_vwap_1h_2021.csv", header=FALSE)

setwd("/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Coinbase_1H/Coinbase 2020/coinbase_unziped")

### COINBASE 2020 TIME SERIES
cb_bcheur2020  = read.csv("cb_bcheur_vwap_1h_2020.csv", header=FALSE)
cb_btceur2020 = read.csv("cb_btceur_vwap_1h_2020.csv", header=FALSE)
cb_eosheur2020 = read.csv("cb_eoseur_vwap_1h_2020.csv", header=FALSE)
cb_etheur2020  = read.csv("cb_etheur_vwap_1h_2020.csv", header=FALSE)
cb_linkeur2020 = read.csv("cb_linkeur_vwap_1h_2020.csv", header=FALSE)
cb_ltceur2020  = read.csv("cb_ltceur_vwap_1h_2020.csv", header=FALSE)
cb_xlmeur2020 = read.csv("cb_xlmeur_vwap_1h_2020.csv", header=FALSE)
cb_xrpeur2020  = read.csv("cb_xrpeur_vwap_1h_2020.csv", header=FALSE)


#rename colums of each 2020 TS
colnames(cb_bcheur2020)[2]= "cb_bch"
colnames(cb_btceur2020)[2]= "cb_btc"
colnames(cb_eosheur2020)[2]="cb_eos"
colnames(cb_etheur2020)[2]= "cb_eth"
colnames(cb_linkeur2020)[2]="cb_link"
colnames(cb_ltceur2020)[2]= "cb_ltc"
colnames(cb_xlmeur2020)[2]= "cb_xlm"
colnames(cb_xrpeur2020)[2]= "cb_xrp"

#rename colums of each 2021 TS
colnames(cb_adaeur2021)[2]= "cb_ada"
colnames(cb_bcheur2021)[2]= "cb_bch"
colnames(cb_btceur2021)[2]= "cb_btc"
colnames(cb_eosheur2021)[2]="cb_eos"
colnames(cb_etheur2021)[2]= "cb_eth"
colnames(cb_linkeur2021)[2]= "cb_link"
colnames(cb_ltceur2021)[2]= "cb_ltc"
colnames(cb_xlmeur2021)[2]= "cb_xlm"
colnames(cb_xrpeur2021)[2]= "cb_xrp"


### Stacking 2020 onto 2021
cb_ada = cb_adaeur2021
cb_bch = rbind(cb_bcheur2020, cb_bcheur2021)
cb_btc = rbind(cb_btceur2020, cb_btceur2021)
cb_eos = rbind(cb_eosheur2020, cb_eosheur2021)
cb_ethe = rbind(cb_etheur2020, cb_etheur2021)
cb_link = rbind(cb_linkeur2020, cb_linkeur2021)
cb_ltc = rbind(cb_ltceur2020, cb_ltceur2021)
cb_xlm = rbind(cb_xlmeur2020, cb_xlmeur2021)
cb_xrp = rbind(cb_xrpeur2020, cb_xrpeur2021)


test = merge(cb_ada, cb_bch, by="V1", all = TRUE)
test = merge(test, cb_btc, by="V1", all = TRUE)
test = merge(test, cb_eos, by="V1", all = TRUE)
test = merge(test, cb_ethe, by="V1", all = TRUE)
test = merge(test, cb_link, by="V1", all = TRUE)
test = merge(test, cb_ltc, by="V1", all = TRUE)
test = merge(test, cb_xlm, by="V1", all = TRUE)
test = merge(test, cb_xrp, by="V1", all = TRUE)

# Create a sequence of timestamps
timestamp = seq(as.POSIXct("2020-1-1 0:00"), as.POSIXct("2021-4-27 21:00"), by ="hour")

###Final data BINANCE 
test$V1 = timestamp
coinbase_panel = test

#Check Unique Values Per Column
lapply(coinbase_panel, function(x) length(unique(x))) 

not_missing_values <- coinbase_panel %>% summarize_all(funs(sum(!is.na(.))))

not_missing_values <- gather(not_missing_values, key="feature", value="missing_pct")



#write.csv(coinbase_panel,"/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Panels(Clean)\\coinbase_panel.csv", row.names = FALSE)


## 0.4.3 KRAKEN CLEANING ####
setwd("/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Kraken_1H/kraken_2021/kraken_unziped")

### KRAKEN 2021 TIME SERIES
kk_adaeur2021  = read.csv("kk_adaeur_vwap_1h_2021.csv", header=FALSE)
kk_bcheur2021  = read.csv("kk_bcheur_vwap_1h_2021.csv", header=FALSE)
kk_btceur2021 = read.csv("kk_btceur_vwap_1h_2021.csv", header=FALSE)
kk_eosheur2021 = read.csv("kk_eoseur_vwap_1h_2021.csv", header=FALSE)
kk_etheur2021  = read.csv("kk_etheur_vwap_1h_2021.csv", header=FALSE)
kk_linkeur2021 = read.csv("kk_linkeur_vwap_1h_2021.csv", header=FALSE)
kk_ltceur2021  = read.csv("kk_ltceur_vwap_1h_2021.csv", header=FALSE)
kk_xlmeur2021 = read.csv("kk_xlmeur_vwap_1h_2021.csv", header=FALSE)
kk_xrpeur2021  = read.csv("kk_xrpeur_vwap_1h_2021.csv", header=FALSE)

setwd("/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Kraken_1H/kraken_2020/kraken_unziped")

### KRAKEN 2020 TIME SERIES
kk_adaeur2020  = read.csv("kk_adaeur_vwap_1h_2020.csv", header=FALSE)
kk_bcheur2020  = read.csv("kk_bcheur_vwap_1h_2020.csv", header=FALSE)
kk_btceur2020 = read.csv("kk_btceur_vwap_1h_2020.csv", header=FALSE)
kk_eosheur2020 = read.csv("kk_eoseur_vwap_1h_2020.csv", header=FALSE)
kk_etheur2020  = read.csv("kk_etheur_vwap_1h_2020.csv", header=FALSE)
kk_linkeur2020 = read.csv("kk_linkeur_vwap_1h_2020.csv", header=FALSE)
kk_ltceur2020  = read.csv("kk_ltceur_vwap_1h_2020.csv", header=FALSE)
kk_xlmeur2020 = read.csv("kk_xlmeur_vwap_1h_2020.csv", header=FALSE)
kk_xrpeur2020  = read.csv("kk_xrpeur_vwap_1h_2020.csv", header=FALSE)


#rename colums of each 2020 TS
colnames(kk_adaeur2020)[2] = "kk_ada"
colnames(kk_bcheur2020)[2]= "kk_bch"
colnames(kk_btceur2020)[2]= "kk_btc"
colnames(kk_eosheur2020)[2]="kk_eos"
colnames(kk_etheur2020)[2]= "kk_eth"
colnames(kk_linkeur2020)[2]="kk_link"
colnames(kk_ltceur2020)[2]= "kk_ltc"
colnames(kk_xlmeur2020)[2]= "kk_xlm"
colnames(kk_xrpeur2020)[2]= "kk_xrp"

#rename colums of each 2021 TS
colnames(kk_adaeur2021)[2]= "kk_ada"
colnames(kk_bcheur2021)[2]= "kk_bch"
colnames(kk_btceur2021)[2]= "kk_btc"
colnames(kk_eosheur2021)[2]="kk_eos"
colnames(kk_etheur2021)[2]= "kk_eth"
colnames(kk_linkeur2021)[2]= "kk_link"
colnames(kk_ltceur2021)[2]= "kk_ltc"
colnames(kk_xlmeur2021)[2]= "kk_xlm"
colnames(kk_xrpeur2021)[2]= "kk_xrp"

### Stacking 2020 onto 2021
kk_ada = rbind(kk_adaeur2020, kk_adaeur2021)
kk_bch = rbind(kk_bcheur2020, kk_bcheur2021)
kk_btc = rbind(kk_btceur2020, kk_btceur2021)
kk_eos = rbind(kk_eosheur2020, kk_eosheur2021)
kk_ethe = rbind(kk_etheur2020, kk_etheur2021)
kk_link = rbind(kk_linkeur2020, kk_linkeur2021)
kk_ltc = rbind(kk_ltceur2020, kk_ltceur2021)
kk_xlm = rbind(kk_xlmeur2020, kk_xlmeur2021)
kk_xrp = rbind(kk_xrpeur2020, kk_xrpeur2021)


test = merge(kk_ada, kk_bch, by="V1", all = TRUE)
test = merge(test, kk_btc, by="V1", all = TRUE)
test = merge(test, kk_eos, by="V1", all = TRUE)
test = merge(test, kk_ethe, by="V1", all = TRUE)
test = merge(test, kk_link, by="V1", all = TRUE)
test = merge(test, kk_ltc, by="V1", all = TRUE)
test = merge(test, kk_xlm, by="V1", all = TRUE)
test = merge(test, kk_xrp, by="V1", all = TRUE)

# Create a sequence of timestamps
timestamp = seq(as.POSIXct("2020-1-1 0:00"), as.POSIXct("2021-4-27 23:00"), by ="hour")

###Final data BINANCE 
test$V1 = timestamp
kraken_panel = test


#Check Unique Values Per Column
lapply(kraken_panel, function(x) length(unique(x))) 

not_missing_values <- kraken_panel %>% summarize_all(funs(sum(!is.na(.))))

not_missing_values <- gather(not_missing_values, key="feature", value="missing_pct")



#write.csv(kraken_panel,"/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Panels(Clean)\\kraken_panel.csv", row.names = FALSE)

###################
# NOW I WANT TO KEEP EACH INSTRUMENT FOR THE THREE EXCHANGES

### ADA ####
test = merge(bn_ada, cb_ada, by="V1", all = TRUE)
test = merge(test, kk_ada, by="V1", all = TRUE)
timestamp = seq(as.POSIXct("2020-1-1 0:00"), as.POSIXct("2021-4-27 4:00"), by ="hour")
test$V1 = timestamp
ada_panel = test
#write.csv(kraken_panel,"/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Panels(Clean)\\ada_panel.csv", row.names = FALSE)

### BCH ####
test = merge(bn_bch, cb_bch, by="V1", all = TRUE)
test = merge(test, kk_bch, by="V1", all = TRUE)
timestamp = seq(as.POSIXct("2020-1-1 0:00"), as.POSIXct("2021-4-28 1:00"), by ="hour")
test$V1 = timestamp
bch_panel = test
#write.csv(kraken_panel,"/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Panels(Clean)\\bch_panel.csv", row.names = FALSE)

### BTC ####
test = merge(bn_btc, cb_btc, by="V1", all = TRUE)
test = merge(test, kk_btc, by="V1", all = TRUE)
timestamp = seq(as.POSIXct("2020-1-1 0:00"), as.POSIXct("2021-4-28 1:00"), by ="hour")
test$V1 = timestamp
btc_panel = test
#write.csv(kraken_panel,"/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Panels(Clean)\\btc_panel.csv", row.names = FALSE)

### EOS ####
test = merge(bn_eos, cb_eos, by="V1", all = TRUE)
test = merge(test, kk_eos, by="V1", all = TRUE)
timestamp = seq(as.POSIXct("2020-1-1 0:00"), as.POSIXct("2021-4-27 17:00"), by ="hour")
test$V1 = timestamp
eos_panel = test
#write.csv(kraken_panel,"/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Panels(Clean)\\eos_panel.csv", row.names = FALSE)

### ETH ####
test = merge(bn_ethe, cb_ethe, by="V1", all = TRUE)
test = merge(test, kk_ethe, by="V1", all = TRUE)
timestamp = seq(as.POSIXct("2020-1-1 0:00"), as.POSIXct("2021-4-28 1:00"), by ="hour")
test$V1 = timestamp
eth_panel = test
#write.csv(kraken_panel,"/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Panels(Clean)\\eth_panel.csv", row.names = FALSE)

### LINK ####
test = merge(bn_link, cb_link, by="V1", all = TRUE)
test = merge(test, kk_link, by="V1", all = TRUE)
timestamp = seq(as.POSIXct("2020-1-1 0:00"), as.POSIXct("2021-4-21 15:00"), by ="hour")
test$V1 = timestamp
link_panel = test
#write.csv(kraken_panel,"/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Panels(Clean)\\link_panel.csv", row.names = FALSE)

### LTC ####
test = merge(bn_ltc, cb_ltc, by="V1", all = TRUE)
test = merge(test, kk_ltc, by="V1", all = TRUE)
timestamp = seq(as.POSIXct("2020-1-1 0:00"), as.POSIXct("2021-4-28 1:00"), by ="hour")
test$V1 = timestamp
ltc_panel = test
#write.csv(kraken_panel,"/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Panels(Clean)\\ltc_panel.csv", row.names = FALSE)

### XLM ####
test = merge(bn_xlm, cb_xlm, by="V1", all = TRUE)
test = merge(test, kk_xlm, by="V1", all = TRUE)
timestamp = seq(as.POSIXct("2020-1-1 0:00"), as.POSIXct("2021-4-28 1:00"), by ="hour")
test$V1 = timestamp
xlm_panel = test
#write.csv(kraken_panel,"/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Panels(Clean)\\xlm_panel.csv", row.names = FALSE)


### XRP ####
test = merge(bn_xrp, cb_xrp, by="V1", all = TRUE)
test = merge(test, kk_xrp, by="V1", all = TRUE)
timestamp = seq(as.POSIXct("2020-1-1 0:00"), as.POSIXct("2021-4-28 1:00"), by ="hour")
test$V1 = timestamp
xrp_panel = test
#write.csv(kraken_panel,"/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Panels(Clean)\\xrp_panel.csv", row.names = FALSE)

# delete_var = c("bn_ada","bn_eos","cb_ada", "cb_eos","kk_ada", "kk_eos")
# 
# panel_limpio_binance = binance_panel[ , -which(names(binance_panel) %in% delete_var)]
# panel_limpio_coinbase = coinbase_panel[ , -which(names(coinbase_panel) %in% delete_var)]
# panel_limpio_kraken = kraken_panel[ , -which(names(kraken_panel) %in% delete_var)]
# 
# all = merge(panel_limpio_binance, panel_limpio_coinbase, by="V1", all = TRUE)
# all = merge(all, panel_limpio_kraken, by="V1", all = TRUE)
# 
# all=all[8200:9241,]
# 
# mice = mice(all, m=5,maxit=50, meth='cart', seed=500) #tried methods: bayesian, pmm, but cart seems to work best, see Help() for the methods
# summary(mice)
# mice_results = complete(mice)
# 
# # # Plot age distributions to check the distribution of the imputed values
# # par(mfrow=c(1,2))
# # hist(data$Age, freq=F, main='Age: Original Data')
# # hist(mice_results$Age, freq=F, main='Age: Imputation')
# 
# 
# 
# lapply(all, function(x) length(unique(x)))
# # Replace mice results in NAs
# all$bn_bch = mice_results$bn_bch
# all$bn_xlm = mice_results$bn_xlm
# all$cb_xrp = mice_results$cb_xrp
# 
# sum(is.na(all))
# 
# panel_limpio_binance=all%>%
#   dplyr::select("V1", starts_with("bn_"))
# panel_limpio_coinbase=all%>%
#   dplyr::select("V1", starts_with("cb_"))
# panel_limpio_kraken=all%>%
#   dplyr::select("V1", starts_with("kk_"))

