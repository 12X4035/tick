#三菱東京UFJ（2016とリニューアル前）

library(dplyr)
library(data.table)
library(texreg)

stock_name<-read.csv("~/Desktop/stock.csv", fileEncoding = "cp932", header=FALSE, col.names = c("stock", "stock_name"), colClasses = "character")

#bid
#2016
setwd("/Volumes/HDD2 1/tick_225/2016/bid/all_day_30_minutes_clm_obj")
file <- dir()

df_bid_16<-data.table()
for ( i in 1:length(file)){
  d <- data.table(list(readRDS(file[i])))
  stock<-substr(file[i], 27, 30)
  #add_stock <- d[, stock:=substr(file[i], 27,30)] #16,19/27,30 
  add_stock <-cbind(stock, d)
  add_stock_name<-dplyr::inner_join(add_stock, stock_name, by="stock")
  df_bid_16<-dplyr::bind_rows(df_bid_16, add_stock_name)
  
}
#2015_after
setwd("/Volumes/HDD2 1/tick_225/2015_after/bid/all_day_30_minutes_after_bid_15_clm_obj")
file <- dir()

df_bid_15_after<-data.table()
for ( i in 1:length(file)){
  d <- data.table(list(readRDS(file[i])))
  stock<-substr(file[i], 33, 36)
  #add_stock <- d[, stock:=substr(file[i], 27,30)] #16,19/27,30 
  add_stock <-cbind(stock, d)
  add_stock_name<-dplyr::inner_join(add_stock, stock_name, by="stock")
  df_bid_15_after<-dplyr::bind_rows(df_bid_15_after, add_stock_name)
}
#2015_before
setwd("/Volumes/HDD2 1/tick_225/2015_before/bid/all_day_30_minutes_before_bid_15_clm_obj")
file <- dir()

df_bid_15_before<-data.table()
for ( i in 1:length(file)){
  d <- data.table(list(readRDS(file[i])))
  stock<-substr(file[i], 34, 37)
  #add_stock <- d[, stock:=substr(file[i], 27,30)] #16,19/27,30 
  add_stock <-cbind(stock, d)
  add_stock_name<-dplyr::inner_join(add_stock, stock_name, by="stock")
  df_bid_15_before<-dplyr::bind_rows(df_bid_15_before, add_stock_name)
}

#2016
setwd("/Volumes/HDD2 1/tick_225/2016/ask/all_day_ask_30_minutes_clm_obj")
file <- dir()

df_ask_16<-data.table()
for ( i in 1:length(file)){
  d <- data.table(list(readRDS(file[i])))
  stock<-substr(file[i], 27, 30)
  #add_stock <- d[, stock:=substr(file[i], 27,30)] #16,19/27,30 
  add_stock <-cbind(stock, d)
  add_stock_name<-dplyr::inner_join(add_stock, stock_name, by="stock")
  df_ask_16<-dplyr::bind_rows(df_ask_16, add_stock_name)
}
#2015_after
setwd("/Volumes/HDD2 1/tick_225/2015_after/ask/all_day_30_minutes_ask_after_15_clm_obj")
file <- dir()

df_ask_15_after<-data.table()
for ( i in 1:length(file)){
  d <- data.table(list(readRDS(file[i])))
  stock<-substr(file[i], 33, 36)
  #add_stock <- d[, stock:=substr(file[i], 27,30)] #16,19/27,30 
  add_stock <-cbind(stock, d)
  add_stock_name<-dplyr::inner_join(add_stock, stock_name, by="stock")
  df_ask_15_after<-dplyr::bind_rows(df_ask_15_after, add_stock_name)
}
#2015_before
setwd("/Volumes/HDD2 1/tick_225/2015_before/ask/all_day_30_minutes_ask_before_15_clm_obj")
file <- dir()

df_ask_15_before<-data.table()
for ( i in 1:length(file)){
  d <- data.table(list(readRDS(file[i])))
  stock<-substr(file[i], 34, 37)
  #add_stock <- d[, stock:=substr(file[i], 27,30)] #16,19/27,30 
  add_stock <-cbind(stock, d)
  add_stock_name<-dplyr::inner_join(add_stock, stock_name, by="stock")
  df_ask_15_before<-dplyr::bind_rows(df_ask_15_before, add_stock_name)
}


ufj_bid_16<-df_bid_16%>%
  dplyr::filter(stock==7203)

ufj_ask_16<-df_ask_16%>%
  dplyr::filter(stock==7203)

ufj_bid_15_after<-df_bid_15_after%>%
  dplyr::filter(stock==7203)

ufj_ask_15_after<-df_ask_15_after%>%
  dplyr::filter(stock==7203)

ufj_bid_15_before<-df_bid_15_before%>%
  dplyr::filter(stock==7203)

ufj_ask_15_before<-df_ask_15_before%>%
  dplyr::filter(stock==7203)

model<-c(ufj_bid_15_before$V1[1], ufj_bid_15_after$V1[1], ufj_bid_16$V1[1], ufj_ask_15_before$V1[1], ufj_ask_15_after$V1[1], ufj_ask_16$V1[1])
model_names<-c("リニューアル前（買い）", "リニューアル後（買い）", "2016年（買い）", "リニューアル前（売り）", "リニューアル後（売り）", "2016年（売り）")

texreg(model, caption="トヨタ自動車", digits=3, booktabs=, dcolumn=T, center=T, caption.above=T, custom.model.names=model_names)
