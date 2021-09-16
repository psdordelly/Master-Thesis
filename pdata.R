####### Panel data ########
panel <- panel_limpio_binance %>% mutate(exchange = "binance") 
  
oldcolnames = colnames(panel)
newcolnames = c("V1", "bch", "btc", "eth", "link", "ltc", "xlm", "xrp", "exchange")
names(panel) = newcolnames
binance_panel = panel 

panel <- panel_limpio_coinbase %>% mutate(exchange = "coinbase") 
oldcolnames = colnames(panel)
newcolnames = c("V1", "bch", "btc", "eth", "link", "ltc", "xlm", "xrp", "exchange")
names(panel) = newcolnames
coinbase_panel = panel 

panel <- panel_limpio_kraken %>% mutate(exchange = "kraken") 
oldcolnames = colnames(panel)
newcolnames = c("V1", "bch", "btc","eth", "link", "ltc", "xlm", "xrp", "exchange")
names(panel) = newcolnames
kraken_panel = panel 

panel = rbind(binance_panel, coinbase_panel, kraken_panel)
###################
# pdata_limpio_index = pdata.frame(test2, index=c("day", "time", "exchange"))
pdata_limpio = pdata.frame(panel, index=c("exchange","V1"), drop.index = TRUE)

write.csv(panel,"/Users/paolas.dordelly/Desktop/Thesis/Data/Kaiko/Panels(Clean)\\panel_data_cris.csv", row.names = FALSE)

#####
#test
binance_test = binance_panel[1:10,]
coinbase_test = coinbase_panel[1:10,]
kraken_test = kraken_panel[1:10,]

test3 = seq(1971,1980)
binance_test$V1 = test3
coinbase_test$V1 = test3
kraken_test$V1 = test3

panel_test = rbind(binance_test, coinbase_test, kraken_test)
pdata_test = pdata.frame(panel_test, index=c("V1","exchange"))
cipstest(pdata_test$btc, type = "none")

###################
pcdtest(pdata_limpio$bch, "lm")
pcdtest(pdata_limpio$btc, "lm")
pcdtest(pdata_limpio$eth, "lm")
pcdtest(pdata_limpio$link, "lm")
pcdtest(pdata_limpio$ltc, "lm")
pcdtest(pdata_limpio$xlm, "lm")
pcdtest(pdata_limpio$xrp, "lm")

pcdtest(pdata_limpio$bch)
pcdtest(pdata_limpio$btc)
pcdtest(pdata_limpio$eth)
pcdtest(pdata_limpio$link)
pcdtest(pdata_limpio$ltc)
pcdtest(pdata_limpio$xlm)
pcdtest(pdata_limpio$xrp)


