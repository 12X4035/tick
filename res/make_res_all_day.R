#=============================================#
#分析用のcsv作成スクリプト(all_day_30_minutes)#
#=============================================#

library(dplyr)
library(data.table)

#銘柄名
stock<-read.csv("~/Desktop/stock.csv", fileEncoding = "cp932", header=FALSE, col.names = c("stock", "stock_name"), colClasses = "character")

#bid
#2016（7186なし, 3407, 4689は後付けで処理）
setwd("/Volumes/HDD2 1/tick_225/2016/bid/all_day_30_minutes_clm")
file <- dir()

df_bid_16<-data.table()
for ( i in 1:length(file)){
  d <- fread(file[i], header = TRUE)
  add_stock <- d[, stock:=substr(file[i], 27,30)] #16,19/27,30 
  
  df_bid_16<-dplyr::bind_rows(df_bid_16, add_stock)
}
df_bid_16<-dplyr::inner_join(df_bid_16, stock, by="stock")%>%
  dplyr::mutate(category=ifelse(stock=="1332"|stock=="1333", "水産", #
                       ifelse(stock=="1605", "鉱業", #
                       ifelse(stock=="1721"|stock=="1801"|stock=="1802"|stock=="1803"|stock=="1808"|stock=="1812"|stock=="1925"|stock=="1928"|stock=="1963", "建設", #
                       ifelse(stock=="2002"|stock=="2269"|stock=="2282"|stock=="2501"|stock=="2502"|stock=="2503"|stock=="2531"|stock=="2801"|stock=="2802"|stock=="2871"|stock=="2914", "食品", # 
                       ifelse(stock=="3101"|stock=="3103"|stock=="3401"|stock=="3402", "繊維", #
                       ifelse(stock=="3861"|stock=="3863"|stock=="3865", "パルプ・紙", #
                       ifelse(stock=="3405"|stock=="3407"|stock=="4004"|stock=="4005"|stock=="4021"|stock=="4042"|stock=="4043"|stock=="4061"|stock=="4063"|stock=="4183"|stock=="4188"|stock=="4208"|stock=="4272"|stock=="4452"|stock=="4901"|stock=="4911"|stock=="6988", "化学", #
                       ifelse(stock=="4151"|stock=="4502"|stock=="4503"|stock=="4506"|stock=="4507"|stock=="4519"|stock=="4523"|stock=="4568", "医薬品", #
                       ifelse(stock=="5002"|stock=="5020", "石油", #
                       ifelse(stock=="5101"|stock=="5108", "ゴム", #
                       ifelse(stock=="5201"|stock=="5202"|stock=="5214"|stock=="5232"|stock=="5233"|stock=="5301"|stock=="5332"|stock=="5333", "窯業", #
                       ifelse(stock=="5401"|stock=="5406"|stock=="5411"|stock=="5413"|stock=="5541", "鉄鋼", #
                       ifelse(stock=="3436"|stock=="5703"|stock=="5706"|stock=="5707"|stock=="5711"|stock=="5713"|stock=="5714"|stock=="5715"|stock=="5801"|stock=="5802"|stock=="5803"|stock=="5901", "非鉄金属製品",
                       ifelse(stock=="5631"|stock=="6103"|stock=="6113"|stock=="6301"|stock=="6302"|stock=="6305"|stock=="6326"|stock=="6361"|stock=="6366"|stock=="6367"|stock=="6471"|stock=="6472"|stock=="6473"|stock=="7004"|stock=="7011"|stock=="7013", "機械",
                       ifelse(stock=="3105"|stock=="6479"|stock=="6501"|stock=="6502"|stock=="6503"|stock=="6504"|stock=="6506"|stock=="6508"|stock=="6674"|stock=="6701"|stock=="6702"|stock=="6703"|stock=="6752"|stock=="6758"|stock=="6762"|stock=="6767"|stock=="6770"|stock=="6773"|stock=="6841"|stock=="6857"|stock=="6902"|stock=="6952"|stock=="6954"|stock=="6971"|stock=="6976"|stock=="7735"|stock=="7751"|stock=="7752"|stock=="8035", "電気機器",
                       ifelse(stock=="7003"|stock=="7012", "造船",
                       ifelse(stock=="7201"|stock=="7202"|stock=="7203"|stock=="7205"|stock=="7211"|stock=="7261"|stock=="7267"|stock=="7269"|stock=="7270"|stock=="7272", "自動車",
                       ifelse(stock=="4543"|stock=="4902"|stock=="7731"|stock=="7733"|stock=="7762", "精密機器",
                       ifelse(stock=="7911"|stock=="7912"|stock=="7951", "その他製造",
                       ifelse(stock=="2768"|stock=="8001"|stock=="8002"|stock=="8015"|stock=="8031"|stock=="8053"|stock=="8058", "商社",
                       ifelse(stock=="3086"|stock=="3099"|stock=="3382"|stock=="8028"|stock=="8233"|stock=="8252"|stock=="8267"|stock=="9983", "小売業",
                       ifelse(stock=="7186"|stock=="8303"|stock=="8304"|stock=="8306"|stock=="8308"|stock=="8309"|stock=="8316"|stock=="8331"|stock=="8354"|stock=="8355"|stock=="8411", "銀行",
                       ifelse(stock=="8601"|stock=="8604"|stock=="8628", "証券",
                       ifelse(stock=="8630"|stock=="8725"|stock=="8729"|stock=="8750"|stock=="8766"|stock=="8795", "保険",
                       ifelse(stock=="8253", "その他金融",
                       ifelse(stock=="3289"|stock=="8801"|stock=="8802"|stock=="8804"|stock=="8830", "不動産",
                       ifelse(stock=="9001"|stock=="9005"|stock=="9007"|stock=="9008"|stock=="9009"|stock=="9020"|stock=="9021"|stock=="9022", "鉄道・バス",
                       ifelse(stock=="9062"|stock=="9064", "陸運",
                       ifelse(stock=="9101"|stock=="9104"|stock=="9107", "海運",
                       ifelse(stock=="9202", "空運",
                       ifelse(stock=="9301", "倉庫",
                       ifelse(stock=="9412"|stock=="9432"|stock=="9433"|stock=="9437"|stock=="9613"|stock=="9984", "通信",
                       ifelse(stock=="9501"|stock=="9502"|stock=="9503", "電力",
                       ifelse(stock=="9531"|stock=="9532", "ガス",
                       ifelse(stock=="2432"|stock=="4324"|stock=="4689"|stock=="4704"|stock=="4755"|stock=="6098"|stock=="9602"|stock=="9681"|stock=="9735"|stock=="9766", "サービス", NA
                      ))))))))))))))))))))))))))))))))))))%>%
  dplyr::filter(stock!="3110"&stock!="8803"&stock!="2432"&stock!="1808"&stock!="8332"&stock!="7186"&stock!="6753"&stock!="7272"&stock!="8270"&stock!="8028"&stock!="4041"&stock!="4755"&stock!="6724")
#（１2銘柄）
#3110 （日東紡：2015/10/01 除外）
#8803 （平和不動産：2015/10/01 除外）
#2432 （DeNA：2015/10/01 追加）
#1808 （長谷工：2015/10/01 追加）
#8332 （横浜銀行：2016/03/29 除外）
#7186 （コンコルディア・フィナンシャルグループ：2016/04/04 追加）
#6753 （シャープ：2016/08/01 除外）
#7272 （ヤマハ電機：2016/08/01　追加）
#8270 （ユニーグループ・ホールディングス：2016/08/29 除外）
#8028 （ユニー・ファミリーマートホールディングス：2016/09/01 追加）
#4041 （日本曹達：2016/10/03 除外）
#4755 （楽天：2016/10/03 追加）

#2015_before（7186なし）
setwd("/Volumes/HDD2 1/tick_225/2015_before/bid/all_day_30_minutes_before_bid_15_clm")
file <- dir()

df_bid_15_before<-data.table()
for ( i in 1:length(file)){
  d <- fread(file[i], header = TRUE)
  add_stock <- d[, stock:=substr(file[i], 34,37)] #16,19/27,30 
  
  df_bid_15_before<-dplyr::bind_rows(df_bid_15_before, add_stock)
}
df_bid_15_before<-dplyr::inner_join(df_bid_15_before, stock, by="stock")%>%
  dplyr::mutate(category=ifelse(stock=="1332"|stock=="1333", "水産", #
                       ifelse(stock=="1605", "鉱業", #
                       ifelse(stock=="1721"|stock=="1801"|stock=="1802"|stock=="1803"|stock=="1808"|stock=="1812"|stock=="1925"|stock=="1928"|stock=="1963", "建設", #
                       ifelse(stock=="2002"|stock=="2269"|stock=="2282"|stock=="2501"|stock=="2502"|stock=="2503"|stock=="2531"|stock=="2801"|stock=="2802"|stock=="2871"|stock=="2914", "食品", # 
                       ifelse(stock=="3101"|stock=="3103"|stock=="3401"|stock=="3402", "繊維", #
                       ifelse(stock=="3861"|stock=="3863"|stock=="3865", "パルプ・紙", #
                       ifelse(stock=="3405"|stock=="3407"|stock=="4004"|stock=="4005"|stock=="4021"|stock=="4042"|stock=="4043"|stock=="4061"|stock=="4063"|stock=="4183"|stock=="4188"|stock=="4208"|stock=="4272"|stock=="4452"|stock=="4901"|stock=="4911"|stock=="6988", "化学", #
                       ifelse(stock=="4151"|stock=="4502"|stock=="4503"|stock=="4506"|stock=="4507"|stock=="4519"|stock=="4523"|stock=="4568", "医薬品", #
                       ifelse(stock=="5002"|stock=="5020", "石油", #
                       ifelse(stock=="5101"|stock=="5108", "ゴム", #
                       ifelse(stock=="5201"|stock=="5202"|stock=="5214"|stock=="5232"|stock=="5233"|stock=="5301"|stock=="5332"|stock=="5333", "窯業", #
                       ifelse(stock=="5401"|stock=="5406"|stock=="5411"|stock=="5413"|stock=="5541", "鉄鋼", #
                       ifelse(stock=="3436"|stock=="5703"|stock=="5706"|stock=="5707"|stock=="5711"|stock=="5713"|stock=="5714"|stock=="5715"|stock=="5801"|stock=="5802"|stock=="5803"|stock=="5901", "非鉄金属製品",
                       ifelse(stock=="5631"|stock=="6103"|stock=="6113"|stock=="6301"|stock=="6302"|stock=="6305"|stock=="6326"|stock=="6361"|stock=="6366"|stock=="6367"|stock=="6471"|stock=="6472"|stock=="6473"|stock=="7004"|stock=="7011"|stock=="7013", "機械",
                       ifelse(stock=="3105"|stock=="6479"|stock=="6501"|stock=="6502"|stock=="6503"|stock=="6504"|stock=="6506"|stock=="6508"|stock=="6674"|stock=="6701"|stock=="6702"|stock=="6703"|stock=="6752"|stock=="6758"|stock=="6762"|stock=="6767"|stock=="6770"|stock=="6773"|stock=="6841"|stock=="6857"|stock=="6902"|stock=="6952"|stock=="6954"|stock=="6971"|stock=="6976"|stock=="7735"|stock=="7751"|stock=="7752"|stock=="8035", "電気機器",
                       ifelse(stock=="7003"|stock=="7012", "造船",
                       ifelse(stock=="7201"|stock=="7202"|stock=="7203"|stock=="7205"|stock=="7211"|stock=="7261"|stock=="7267"|stock=="7269"|stock=="7270"|stock=="7272", "自動車",
                       ifelse(stock=="4543"|stock=="4902"|stock=="7731"|stock=="7733"|stock=="7762", "精密機器",
                       ifelse(stock=="7911"|stock=="7912"|stock=="7951", "その他製造",
                       ifelse(stock=="2768"|stock=="8001"|stock=="8002"|stock=="8015"|stock=="8031"|stock=="8053"|stock=="8058", "商社",
                       ifelse(stock=="3086"|stock=="3099"|stock=="3382"|stock=="8028"|stock=="8233"|stock=="8252"|stock=="8267"|stock=="9983", "小売業",
                       ifelse(stock=="7186"|stock=="8303"|stock=="8304"|stock=="8306"|stock=="8308"|stock=="8309"|stock=="8316"|stock=="8331"|stock=="8354"|stock=="8355"|stock=="8411", "銀行",
                       ifelse(stock=="8601"|stock=="8604"|stock=="8628", "証券",
                       ifelse(stock=="8630"|stock=="8725"|stock=="8729"|stock=="8750"|stock=="8766"|stock=="8795", "保険",
                       ifelse(stock=="8253", "その他金融",
                       ifelse(stock=="3289"|stock=="8801"|stock=="8802"|stock=="8804"|stock=="8830", "不動産",
                       ifelse(stock=="9001"|stock=="9005"|stock=="9007"|stock=="9008"|stock=="9009"|stock=="9020"|stock=="9021"|stock=="9022", "鉄道・バス",
                       ifelse(stock=="9062"|stock=="9064", "陸運",
                       ifelse(stock=="9101"|stock=="9104"|stock=="9107", "海運",
                       ifelse(stock=="9202", "空運",
                       ifelse(stock=="9301", "倉庫",
                       ifelse(stock=="9412"|stock=="9432"|stock=="9433"|stock=="9437"|stock=="9613"|stock=="9984", "通信",
                       ifelse(stock=="9501"|stock=="9502"|stock=="9503", "電力",
                       ifelse(stock=="9531"|stock=="9532", "ガス",
                       ifelse(stock=="2432"|stock=="4324"|stock=="4689"|stock=="4704"|stock=="4755"|stock=="6098"|stock=="9602"|stock=="9681"|stock=="9735"|stock=="9766", "サービス", NA
                      ))))))))))))))))))))))))))))))))))))%>%
  dplyr::filter(stock!="3110"&stock!="8803"&stock!="2432"&stock!="1808"&stock!="8332"&stock!="7186"&stock!="6753"&stock!="7272"&stock!="8270"&stock!="8028"&stock!="4041"&stock!="4755"&stock!="6724")
#2015_after（7186なし）
setwd("/Volumes/HDD2 1/tick_225/2015_after/bid/all_day_30_minutes_after_bid_15_clm")

file <- dir()

df_bid_15_after<-data.table()
for ( i in 1:length(file)){
  d <- fread(file[i], header = TRUE)
  add_stock <- d[, stock:=substr(file[i], 33,36)] #16,19/27,30 
  
  df_bid_15_after<-dplyr::bind_rows(df_bid_15_after, add_stock)
}
df_bid_15_after<-dplyr::inner_join(df_bid_15_after, stock, by="stock")%>%
  dplyr::mutate(category=ifelse(stock=="1332"|stock=="1333", "水産", #
                       ifelse(stock=="1605", "鉱業", #
                       ifelse(stock=="1721"|stock=="1801"|stock=="1802"|stock=="1803"|stock=="1808"|stock=="1812"|stock=="1925"|stock=="1928"|stock=="1963", "建設", #
                       ifelse(stock=="2002"|stock=="2269"|stock=="2282"|stock=="2501"|stock=="2502"|stock=="2503"|stock=="2531"|stock=="2801"|stock=="2802"|stock=="2871"|stock=="2914", "食品", # 
                       ifelse(stock=="3101"|stock=="3103"|stock=="3401"|stock=="3402", "繊維", #
                       ifelse(stock=="3861"|stock=="3863"|stock=="3865", "パルプ・紙", #
                       ifelse(stock=="3405"|stock=="3407"|stock=="4004"|stock=="4005"|stock=="4021"|stock=="4042"|stock=="4043"|stock=="4061"|stock=="4063"|stock=="4183"|stock=="4188"|stock=="4208"|stock=="4272"|stock=="4452"|stock=="4901"|stock=="4911"|stock=="6988", "化学", #
                       ifelse(stock=="4151"|stock=="4502"|stock=="4503"|stock=="4506"|stock=="4507"|stock=="4519"|stock=="4523"|stock=="4568", "医薬品", #
                       ifelse(stock=="5002"|stock=="5020", "石油", #
                       ifelse(stock=="5101"|stock=="5108", "ゴム", #
                       ifelse(stock=="5201"|stock=="5202"|stock=="5214"|stock=="5232"|stock=="5233"|stock=="5301"|stock=="5332"|stock=="5333", "窯業", #
                       ifelse(stock=="5401"|stock=="5406"|stock=="5411"|stock=="5413"|stock=="5541", "鉄鋼", #
                       ifelse(stock=="3436"|stock=="5703"|stock=="5706"|stock=="5707"|stock=="5711"|stock=="5713"|stock=="5714"|stock=="5715"|stock=="5801"|stock=="5802"|stock=="5803"|stock=="5901", "非鉄金属製品",
                       ifelse(stock=="5631"|stock=="6103"|stock=="6113"|stock=="6301"|stock=="6302"|stock=="6305"|stock=="6326"|stock=="6361"|stock=="6366"|stock=="6367"|stock=="6471"|stock=="6472"|stock=="6473"|stock=="7004"|stock=="7011"|stock=="7013", "機械",
                       ifelse(stock=="3105"|stock=="6479"|stock=="6501"|stock=="6502"|stock=="6503"|stock=="6504"|stock=="6506"|stock=="6508"|stock=="6674"|stock=="6701"|stock=="6702"|stock=="6703"|stock=="6752"|stock=="6758"|stock=="6762"|stock=="6767"|stock=="6770"|stock=="6773"|stock=="6841"|stock=="6857"|stock=="6902"|stock=="6952"|stock=="6954"|stock=="6971"|stock=="6976"|stock=="7735"|stock=="7751"|stock=="7752"|stock=="8035", "電気機器",
                       ifelse(stock=="7003"|stock=="7012", "造船",
                       ifelse(stock=="7201"|stock=="7202"|stock=="7203"|stock=="7205"|stock=="7211"|stock=="7261"|stock=="7267"|stock=="7269"|stock=="7270"|stock=="7272", "自動車",
                       ifelse(stock=="4543"|stock=="4902"|stock=="7731"|stock=="7733"|stock=="7762", "精密機器",
                       ifelse(stock=="7911"|stock=="7912"|stock=="7951", "その他製造",
                       ifelse(stock=="2768"|stock=="8001"|stock=="8002"|stock=="8015"|stock=="8031"|stock=="8053"|stock=="8058", "商社",
                       ifelse(stock=="3086"|stock=="3099"|stock=="3382"|stock=="8028"|stock=="8233"|stock=="8252"|stock=="8267"|stock=="9983", "小売業",
                       ifelse(stock=="7186"|stock=="8303"|stock=="8304"|stock=="8306"|stock=="8308"|stock=="8309"|stock=="8316"|stock=="8331"|stock=="8354"|stock=="8355"|stock=="8411", "銀行",
                       ifelse(stock=="8601"|stock=="8604"|stock=="8628", "証券",
                       ifelse(stock=="8630"|stock=="8725"|stock=="8729"|stock=="8750"|stock=="8766"|stock=="8795", "保険",
                       ifelse(stock=="8253", "その他金融",
                       ifelse(stock=="3289"|stock=="8801"|stock=="8802"|stock=="8804"|stock=="8830", "不動産",
                       ifelse(stock=="9001"|stock=="9005"|stock=="9007"|stock=="9008"|stock=="9009"|stock=="9020"|stock=="9021"|stock=="9022", "鉄道・バス",
                       ifelse(stock=="9062"|stock=="9064", "陸運",
                       ifelse(stock=="9101"|stock=="9104"|stock=="9107", "海運",
                       ifelse(stock=="9202", "空運",
                       ifelse(stock=="9301", "倉庫",
                       ifelse(stock=="9412"|stock=="9432"|stock=="9433"|stock=="9437"|stock=="9613"|stock=="9984", "通信",
                       ifelse(stock=="9501"|stock=="9502"|stock=="9503", "電力",
                       ifelse(stock=="9531"|stock=="9532", "ガス",
                       ifelse(stock=="2432"|stock=="4324"|stock=="4689"|stock=="4704"|stock=="4755"|stock=="6098"|stock=="9602"|stock=="9681"|stock=="9735"|stock=="9766", "サービス", NA
                      ))))))))))))))))))))))))))))))))))))%>%
  dplyr::filter(stock!="3110"&stock!="8803"&stock!="2432"&stock!="1808"&stock!="8332"&stock!="7186"&stock!="6753"&stock!="7272"&stock!="8270"&stock!="8028"&stock!="4041"&stock!="4755"&stock!="6724")
#ask
#2016
setwd("/Volumes/HDD2 1/tick_225/2016/ask/all_day_ask_30_minutes_clm")

file <- dir()

df_ask_16<-data.table()
for ( i in 1:length(file)){
  d <- fread(file[i], header=TRUE)
  add_stock <- d[, stock:=substr(file[i], 27, 30)]
  
  df_ask_16<-dplyr::bind_rows(df_ask_16, add_stock)
}
df_ask_16<-dplyr::inner_join(df_ask_16, stock, by="stock")%>%
  dplyr::mutate(category=ifelse(stock=="1332"|stock=="1333", "水産", #
                       ifelse(stock=="1605", "鉱業", #
                       ifelse(stock=="1721"|stock=="1801"|stock=="1802"|stock=="1803"|stock=="1808"|stock=="1812"|stock=="1925"|stock=="1928"|stock=="1963", "建設", #
                       ifelse(stock=="2002"|stock=="2269"|stock=="2282"|stock=="2501"|stock=="2502"|stock=="2503"|stock=="2531"|stock=="2801"|stock=="2802"|stock=="2871"|stock=="2914", "食品", # 
                       ifelse(stock=="3101"|stock=="3103"|stock=="3401"|stock=="3402", "繊維", #
                       ifelse(stock=="3861"|stock=="3863"|stock=="3865", "パルプ・紙", #
                       ifelse(stock=="3405"|stock=="3407"|stock=="4004"|stock=="4005"|stock=="4021"|stock=="4042"|stock=="4043"|stock=="4061"|stock=="4063"|stock=="4183"|stock=="4188"|stock=="4208"|stock=="4272"|stock=="4452"|stock=="4901"|stock=="4911"|stock=="6988", "化学", #
                       ifelse(stock=="4151"|stock=="4502"|stock=="4503"|stock=="4506"|stock=="4507"|stock=="4519"|stock=="4523"|stock=="4568", "医薬品", #
                       ifelse(stock=="5002"|stock=="5020", "石油", #
                       ifelse(stock=="5101"|stock=="5108", "ゴム", #
                       ifelse(stock=="5201"|stock=="5202"|stock=="5214"|stock=="5232"|stock=="5233"|stock=="5301"|stock=="5332"|stock=="5333", "窯業", #
                       ifelse(stock=="5401"|stock=="5406"|stock=="5411"|stock=="5413"|stock=="5541", "鉄鋼", #
                       ifelse(stock=="3436"|stock=="5703"|stock=="5706"|stock=="5707"|stock=="5711"|stock=="5713"|stock=="5714"|stock=="5715"|stock=="5801"|stock=="5802"|stock=="5803"|stock=="5901", "非鉄金属製品",
                       ifelse(stock=="5631"|stock=="6103"|stock=="6113"|stock=="6301"|stock=="6302"|stock=="6305"|stock=="6326"|stock=="6361"|stock=="6366"|stock=="6367"|stock=="6471"|stock=="6472"|stock=="6473"|stock=="7004"|stock=="7011"|stock=="7013", "機械",
                       ifelse(stock=="3105"|stock=="6479"|stock=="6501"|stock=="6502"|stock=="6503"|stock=="6504"|stock=="6506"|stock=="6508"|stock=="6674"|stock=="6701"|stock=="6702"|stock=="6703"|stock=="6752"|stock=="6758"|stock=="6762"|stock=="6767"|stock=="6770"|stock=="6773"|stock=="6841"|stock=="6857"|stock=="6902"|stock=="6952"|stock=="6954"|stock=="6971"|stock=="6976"|stock=="7735"|stock=="7751"|stock=="7752"|stock=="8035", "電気機器",
                       ifelse(stock=="7003"|stock=="7012", "造船",
                       ifelse(stock=="7201"|stock=="7202"|stock=="7203"|stock=="7205"|stock=="7211"|stock=="7261"|stock=="7267"|stock=="7269"|stock=="7270"|stock=="7272", "自動車",
                       ifelse(stock=="4543"|stock=="4902"|stock=="7731"|stock=="7733"|stock=="7762", "精密機器",
                       ifelse(stock=="7911"|stock=="7912"|stock=="7951", "その他製造",
                       ifelse(stock=="2768"|stock=="8001"|stock=="8002"|stock=="8015"|stock=="8031"|stock=="8053"|stock=="8058", "商社",
                       ifelse(stock=="3086"|stock=="3099"|stock=="3382"|stock=="8028"|stock=="8233"|stock=="8252"|stock=="8267"|stock=="9983", "小売業",
                       ifelse(stock=="7186"|stock=="8303"|stock=="8304"|stock=="8306"|stock=="8308"|stock=="8309"|stock=="8316"|stock=="8331"|stock=="8354"|stock=="8355"|stock=="8411", "銀行",
                       ifelse(stock=="8601"|stock=="8604"|stock=="8628", "証券",
                       ifelse(stock=="8630"|stock=="8725"|stock=="8729"|stock=="8750"|stock=="8766"|stock=="8795", "保険",
                       ifelse(stock=="8253", "その他金融",
                       ifelse(stock=="3289"|stock=="8801"|stock=="8802"|stock=="8804"|stock=="8830", "不動産",
                       ifelse(stock=="9001"|stock=="9005"|stock=="9007"|stock=="9008"|stock=="9009"|stock=="9020"|stock=="9021"|stock=="9022", "鉄道・バス",
                       ifelse(stock=="9062"|stock=="9064", "陸運",
                       ifelse(stock=="9101"|stock=="9104"|stock=="9107", "海運",
                       ifelse(stock=="9202", "空運",
                       ifelse(stock=="9301", "倉庫",
                       ifelse(stock=="9412"|stock=="9432"|stock=="9433"|stock=="9437"|stock=="9613"|stock=="9984", "通信",
                       ifelse(stock=="9501"|stock=="9502"|stock=="9503", "電力",
                       ifelse(stock=="9531"|stock=="9532", "ガス",
                       ifelse(stock=="2432"|stock=="4324"|stock=="4689"|stock=="4704"|stock=="4755"|stock=="6098"|stock=="9602"|stock=="9681"|stock=="9735"|stock=="9766", "サービス", NA
                      ))))))))))))))))))))))))))))))))))))%>%
  dplyr::filter(stock!="3110"&stock!="8803"&stock!="2432"&stock!="1808"&stock!="8332"&stock!="7186"&stock!="6753"&stock!="7272"&stock!="8270"&stock!="8028"&stock!="4041"&stock!="4755"&stock!="6724")
#2015_before（7186なし）
setwd("/Volumes/HDD2 1/tick_225/2015_before/ask/all_day_30_minutes_ask_before_15_clm")

file <- dir()

df_ask_15_before<-data.table()
for ( i in 1:length(file)){
  d <- fread(file[i], header=TRUE)
  add_stock <- d[, stock:=substr(file[i], 34,37)]
  
  df_ask_15_before<-dplyr::bind_rows(df_ask_15_before, add_stock)
}
df_ask_15_before<-dplyr::inner_join(df_ask_15_before, stock, by="stock")%>%
  dplyr::mutate(category=ifelse(stock=="1332"|stock=="1333", "水産", #
                       ifelse(stock=="1605", "鉱業", #
                       ifelse(stock=="1721"|stock=="1801"|stock=="1802"|stock=="1803"|stock=="1808"|stock=="1812"|stock=="1925"|stock=="1928"|stock=="1963", "建設", #
                       ifelse(stock=="2002"|stock=="2269"|stock=="2282"|stock=="2501"|stock=="2502"|stock=="2503"|stock=="2531"|stock=="2801"|stock=="2802"|stock=="2871"|stock=="2914", "食品", # 
                       ifelse(stock=="3101"|stock=="3103"|stock=="3401"|stock=="3402", "繊維", #
                       ifelse(stock=="3861"|stock=="3863"|stock=="3865", "パルプ・紙", #
                       ifelse(stock=="3405"|stock=="3407"|stock=="4004"|stock=="4005"|stock=="4021"|stock=="4042"|stock=="4043"|stock=="4061"|stock=="4063"|stock=="4183"|stock=="4188"|stock=="4208"|stock=="4272"|stock=="4452"|stock=="4901"|stock=="4911"|stock=="6988", "化学", #
                       ifelse(stock=="4151"|stock=="4502"|stock=="4503"|stock=="4506"|stock=="4507"|stock=="4519"|stock=="4523"|stock=="4568", "医薬品", #
                       ifelse(stock=="5002"|stock=="5020", "石油", #
                       ifelse(stock=="5101"|stock=="5108", "ゴム", #
                       ifelse(stock=="5201"|stock=="5202"|stock=="5214"|stock=="5232"|stock=="5233"|stock=="5301"|stock=="5332"|stock=="5333", "窯業", #
                       ifelse(stock=="5401"|stock=="5406"|stock=="5411"|stock=="5413"|stock=="5541", "鉄鋼", #
                       ifelse(stock=="3436"|stock=="5703"|stock=="5706"|stock=="5707"|stock=="5711"|stock=="5713"|stock=="5714"|stock=="5715"|stock=="5801"|stock=="5802"|stock=="5803"|stock=="5901", "非鉄金属製品",
                       ifelse(stock=="5631"|stock=="6103"|stock=="6113"|stock=="6301"|stock=="6302"|stock=="6305"|stock=="6326"|stock=="6361"|stock=="6366"|stock=="6367"|stock=="6471"|stock=="6472"|stock=="6473"|stock=="7004"|stock=="7011"|stock=="7013", "機械",
                       ifelse(stock=="3105"|stock=="6479"|stock=="6501"|stock=="6502"|stock=="6503"|stock=="6504"|stock=="6506"|stock=="6508"|stock=="6674"|stock=="6701"|stock=="6702"|stock=="6703"|stock=="6752"|stock=="6758"|stock=="6762"|stock=="6767"|stock=="6770"|stock=="6773"|stock=="6841"|stock=="6857"|stock=="6902"|stock=="6952"|stock=="6954"|stock=="6971"|stock=="6976"|stock=="7735"|stock=="7751"|stock=="7752"|stock=="8035", "電気機器",
                       ifelse(stock=="7003"|stock=="7012", "造船",
                       ifelse(stock=="7201"|stock=="7202"|stock=="7203"|stock=="7205"|stock=="7211"|stock=="7261"|stock=="7267"|stock=="7269"|stock=="7270"|stock=="7272", "自動車",
                       ifelse(stock=="4543"|stock=="4902"|stock=="7731"|stock=="7733"|stock=="7762", "精密機器",
                       ifelse(stock=="7911"|stock=="7912"|stock=="7951", "その他製造",
                       ifelse(stock=="2768"|stock=="8001"|stock=="8002"|stock=="8015"|stock=="8031"|stock=="8053"|stock=="8058", "商社",
                       ifelse(stock=="3086"|stock=="3099"|stock=="3382"|stock=="8028"|stock=="8233"|stock=="8252"|stock=="8267"|stock=="9983", "小売業",
                       ifelse(stock=="7186"|stock=="8303"|stock=="8304"|stock=="8306"|stock=="8308"|stock=="8309"|stock=="8316"|stock=="8331"|stock=="8354"|stock=="8355"|stock=="8411", "銀行",
                       ifelse(stock=="8601"|stock=="8604"|stock=="8628", "証券",
                       ifelse(stock=="8630"|stock=="8725"|stock=="8729"|stock=="8750"|stock=="8766"|stock=="8795", "保険",
                       ifelse(stock=="8253", "その他金融",
                       ifelse(stock=="3289"|stock=="8801"|stock=="8802"|stock=="8804"|stock=="8830", "不動産",
                       ifelse(stock=="9001"|stock=="9005"|stock=="9007"|stock=="9008"|stock=="9009"|stock=="9020"|stock=="9021"|stock=="9022", "鉄道・バス",
                       ifelse(stock=="9062"|stock=="9064", "陸運",
                       ifelse(stock=="9101"|stock=="9104"|stock=="9107", "海運",
                       ifelse(stock=="9202", "空運",
                       ifelse(stock=="9301", "倉庫",
                       ifelse(stock=="9412"|stock=="9432"|stock=="9433"|stock=="9437"|stock=="9613"|stock=="9984", "通信",
                       ifelse(stock=="9501"|stock=="9502"|stock=="9503", "電力",
                       ifelse(stock=="9531"|stock=="9532", "ガス",
                       ifelse(stock=="2432"|stock=="4324"|stock=="4689"|stock=="4704"|stock=="4755"|stock=="6098"|stock=="9602"|stock=="9681"|stock=="9735"|stock=="9766", "サービス", NA
                      ))))))))))))))))))))))))))))))))))))%>%
  dplyr::filter(stock!="3110"&stock!="8803"&stock!="2432"&stock!="1808"&stock!="8332"&stock!="7186"&stock!="6753"&stock!="7272"&stock!="8270"&stock!="8028"&stock!="4041"&stock!="4755"&stock!="6724")

#2015_after（7186なし）
setwd("/Volumes/HDD2 1/tick_225/2015_after/ask/all_day_30_minutes_ask_after_15_clm")
file <- dir()

df_ask_15_after<-data.table()
for ( i in 1:length(file)){
  d <- fread(file[i], header=TRUE)
  add_stock <- d[, stock:=substr(file[i], 33,36)]
  
  df_ask_15_after<-dplyr::bind_rows(df_ask_15_after, add_stock)
}
df_ask_15_after<-dplyr::inner_join(df_ask_15_after, stock, by="stock")%>%
  dplyr::mutate(category=ifelse(stock=="1332"|stock=="1333", "水産", #
                       ifelse(stock=="1605", "鉱業", #
                       ifelse(stock=="1721"|stock=="1801"|stock=="1802"|stock=="1803"|stock=="1808"|stock=="1812"|stock=="1925"|stock=="1928"|stock=="1963", "建設", #
                       ifelse(stock=="2002"|stock=="2269"|stock=="2282"|stock=="2501"|stock=="2502"|stock=="2503"|stock=="2531"|stock=="2801"|stock=="2802"|stock=="2871"|stock=="2914", "食品", # 
                       ifelse(stock=="3101"|stock=="3103"|stock=="3401"|stock=="3402", "繊維", #
                       ifelse(stock=="3861"|stock=="3863"|stock=="3865", "パルプ・紙", #
                       ifelse(stock=="3405"|stock=="3407"|stock=="4004"|stock=="4005"|stock=="4021"|stock=="4042"|stock=="4043"|stock=="4061"|stock=="4063"|stock=="4183"|stock=="4188"|stock=="4208"|stock=="4272"|stock=="4452"|stock=="4901"|stock=="4911"|stock=="6988", "化学", #
                       ifelse(stock=="4151"|stock=="4502"|stock=="4503"|stock=="4506"|stock=="4507"|stock=="4519"|stock=="4523"|stock=="4568", "医薬品", #
                       ifelse(stock=="5002"|stock=="5020", "石油", #
                       ifelse(stock=="5101"|stock=="5108", "ゴム", #
                       ifelse(stock=="5201"|stock=="5202"|stock=="5214"|stock=="5232"|stock=="5233"|stock=="5301"|stock=="5332"|stock=="5333", "窯業", #
                       ifelse(stock=="5401"|stock=="5406"|stock=="5411"|stock=="5413"|stock=="5541", "鉄鋼", #
                       ifelse(stock=="3436"|stock=="5703"|stock=="5706"|stock=="5707"|stock=="5711"|stock=="5713"|stock=="5714"|stock=="5715"|stock=="5801"|stock=="5802"|stock=="5803"|stock=="5901", "非鉄金属製品",
                       ifelse(stock=="5631"|stock=="6103"|stock=="6113"|stock=="6301"|stock=="6302"|stock=="6305"|stock=="6326"|stock=="6361"|stock=="6366"|stock=="6367"|stock=="6471"|stock=="6472"|stock=="6473"|stock=="7004"|stock=="7011"|stock=="7013", "機械",
                       ifelse(stock=="3105"|stock=="6479"|stock=="6501"|stock=="6502"|stock=="6503"|stock=="6504"|stock=="6506"|stock=="6508"|stock=="6674"|stock=="6701"|stock=="6702"|stock=="6703"|stock=="6752"|stock=="6758"|stock=="6762"|stock=="6767"|stock=="6770"|stock=="6773"|stock=="6841"|stock=="6857"|stock=="6902"|stock=="6952"|stock=="6954"|stock=="6971"|stock=="6976"|stock=="7735"|stock=="7751"|stock=="7752"|stock=="8035", "電気機器",
                       ifelse(stock=="7003"|stock=="7012", "造船",
                       ifelse(stock=="7201"|stock=="7202"|stock=="7203"|stock=="7205"|stock=="7211"|stock=="7261"|stock=="7267"|stock=="7269"|stock=="7270"|stock=="7272", "自動車",
                       ifelse(stock=="4543"|stock=="4902"|stock=="7731"|stock=="7733"|stock=="7762", "精密機器",
                       ifelse(stock=="7911"|stock=="7912"|stock=="7951", "その他製造",
                       ifelse(stock=="2768"|stock=="8001"|stock=="8002"|stock=="8015"|stock=="8031"|stock=="8053"|stock=="8058", "商社",
                       ifelse(stock=="3086"|stock=="3099"|stock=="3382"|stock=="8028"|stock=="8233"|stock=="8252"|stock=="8267"|stock=="9983", "小売業",
                       ifelse(stock=="7186"|stock=="8303"|stock=="8304"|stock=="8306"|stock=="8308"|stock=="8309"|stock=="8316"|stock=="8331"|stock=="8354"|stock=="8355"|stock=="8411", "銀行",
                       ifelse(stock=="8601"|stock=="8604"|stock=="8628", "証券",
                       ifelse(stock=="8630"|stock=="8725"|stock=="8729"|stock=="8750"|stock=="8766"|stock=="8795", "保険",
                       ifelse(stock=="8253", "その他金融",
                       ifelse(stock=="3289"|stock=="8801"|stock=="8802"|stock=="8804"|stock=="8830", "不動産",
                       ifelse(stock=="9001"|stock=="9005"|stock=="9007"|stock=="9008"|stock=="9009"|stock=="9020"|stock=="9021"|stock=="9022", "鉄道・バス",
                       ifelse(stock=="9062"|stock=="9064", "陸運",
                       ifelse(stock=="9101"|stock=="9104"|stock=="9107", "海運",
                       ifelse(stock=="9202", "空運",
                       ifelse(stock=="9301", "倉庫",
                       ifelse(stock=="9412"|stock=="9432"|stock=="9433"|stock=="9437"|stock=="9613"|stock=="9984", "通信",
                       ifelse(stock=="9501"|stock=="9502"|stock=="9503", "電力",
                       ifelse(stock=="9531"|stock=="9532", "ガス",
                       ifelse(stock=="2432"|stock=="4324"|stock=="4689"|stock=="4704"|stock=="4755"|stock=="6098"|stock=="9602"|stock=="9681"|stock=="9735"|stock=="9766", "サービス", NA
                      ))))))))))))))))))))))))))))))))))))%>%
  dplyr::filter(stock!="3110"&stock!="8803"&stock!="2432"&stock!="1808"&stock!="8332"&stock!="7186"&stock!="6753"&stock!="7272"&stock!="8270"&stock!="8028"&stock!="4041"&stock!="4755"&stock!="6724")
write.csv(df_bid_16, "bid_16_all_day_30_minutes_res.csv", quote=FALSE, row.names=FALSE)
write.csv(df_bid_15_before, "bid_15_before_all_day_30_minutes_res.csv", quote=FALSE, row.names=FALSE)
write.csv(df_bid_15_after, "bid_15_after_all_day_30_minutes_res.csv", quote=FALSE, row.names=FALSE)
write.csv(df_ask_16, "ask_16_all_day_30_minutes_res.csv", quote=FALSE, row.names=FALSE)
write.csv(df_ask_15_before, "ask_15_before_all_day_30_minutes_res.csv", quote=FALSE, row.names=FALSE)
write.csv(df_ask_15_after, "ask_15_after_all_day_30_minutes_res.csv", quote=FALSE, row.names=FALSE)
