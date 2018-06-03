library(dplyr)
library(data.table)
library(MASS)
library(ordinal)

args <- commandArgs()
#コマンドライン引数
#1.読み込みdata
data<-args[6]

#書き出しCSVのファイル名
all_day_name<-paste("all_day", data, sep="_")
one_day_name<-paste("one_day", data, sep="_")
all_day_30_minutes_name<-paste("all_day_30_minutes", data, sep="_")
one_day_30_minutes_name<-paste("one_day_30_minutes", data, sep="_")

#データの読み込み
d<-fread(data,
         select=c("row_num", "v2", "v6", "v5", "ask_vol", "bid_vol", "actual_spread", "wait", "bid_order", "time_flag"))

df<-d%>%
  dplyr::select(v2, bid_order, bid_vol, ask_vol, actual_spread, wait, time_flag)%>%
  dplyr::filter(v2>=20160928)

order<-data.table(c("1", "2", "3", "4", "5"))
count_order<- data.table(table(df$bid_order))
order_table<-dplyr::left_join(order, count_order, by="V1")

day<-df%>%
  dplyr::mutate(v2=as.integer(v2))%>%
  dplyr::select(v2)%>%
  dplyr::distinct()

Day<-c()
for (i in 1:nrow(day)){
  Day<-append(Day, rep(day[i,1], 5))
}
day_order<-cbind(Day, order)

Day_time<-c()
for (i in 1:nrow(day)){
  Day_time<-append(Day_time, rep(day[i,1], 50))
}

time_order<-data.table(c("11", "12", "13", "14", "15", "21", "22", "23", "24", "25", "31", "32", "33", "34", "35", "41", "42", "43", "44", "45", "51", "52", "53", "54", "55",
                         "61", "62", "63", "64", "65", "71", "72", "73", "74", "75", "81", "82", "83", "84", "85", "91", "92", "93", "94", "95", "101", "102", "103", "104", "105"))
toc_order<-cbind(Day_time, time_order)
  
time_order_cnt<-df%>%
  dplyr::mutate(time_order=time_flag*10+bid_order)
time_order_cnt<-data.table(table(time_order_cnt$time_order))

time_order_table<-dplyr::left_join(time_order, time_order_cnt, by="V1")

df$bid_order<-factor(df$bid_order,levels=c("5","4","3","2","1"),ordered=TRUE)

#全部の時間
##全部の日を使う
all_day_h<-polr(bid_order~bid_vol+ask_vol+actual_spread+wait,data=df,method="probit",start=c(rep(0,4),rep(1:4)), Hess=T)

all_day_coeff<-all_day_h$coefficients
all_day_prob<-c(pnorm(all_day_h$zeta[1]), pnorm(all_day_h$zeta[2])-pnorm(all_day_h$zeta[1]), pnorm(all_day_h$zeta[3])-pnorm(all_day_h$zeta[2]), pnorm(all_day_h$zeta[4])-pnorm(all_day_h$zeta[3]), 1-pnorm(all_day_h$zeta[4]))

all_day<-t(data.table(c(all_day_coeff, all_day_prob, order_table$N, time_order_table$N)))
colnames(all_day)<-c("bid_vol", "ask_vol", "spread", "wait", "order5", "order4", "order3", "order2", "order1", "count_order1", "count_order2", "count_order3", "count_order4", "count_order5",
                     "toc11", "toc12", "toc13", "toc14", "toc15", "toc21", "toc22", "toc23", "toc24", "toc25", "toc31", "toc32", "toc33", "toc34", "toc35","toc41", "toc42", "toc43", "toc44", "toc45","toc51", "toc52", "toc53", "toc54", "toc55",
                     "toc61", "toc62", "toc63", "toc64", "toc65", "toc71", "toc72", "toc73", "toc74", "toc75", "toc81", "toc82", "toc83", "toc84", "toc85","toc91", "toc92", "toc93", "toc94", "toc95","toc101", "toc102", "toc103", "toc104", "toc105"
                     )

##一日ずつ
cnt_one_day<-d%>%
  dplyr::group_by(v2)%>%
  dplyr::mutate(cnt_one_day = n())%>%
  dplyr::ungroup()%>%
  dplyr::select(v2, cnt_one_day)%>%
  dplyr::distinct() 

  
cnt_order_one_day<-df%>%
  dplyr::select(v2, bid_order, bid_vol, ask_vol, actual_spread, wait, time_flag)%>%
  dplyr::filter(v2>=20160928)%>%
  dplyr::mutate(bid_order = as.character(bid_order))%>%
  dplyr::group_by(v2, bid_order)%>%
  dplyr::mutate(cnt_order_day = n())%>%
  dplyr::ungroup()%>%
  dplyr::select(v2, bid_order, cnt_order_day)%>%
  dplyr::distinct()

cnt_order_one_day_table<-dplyr::left_join(day_order, cnt_order_one_day, by=c("Day"="v2", "V1"="bid_order"))%>%
    tidyr::spread(key = V1, value = cnt_order_day)

cnt_order_time<-df%>%
  dplyr::group_by(v2, bid_order, time_flag)%>%
  dplyr::mutate(cnt_order_time = n())%>%
  dplyr::ungroup()%>%
  dplyr::mutate(toc = paste(as.character(time_flag), as.character(bid_order), sep = ""))%>%
  dplyr::select(v2, toc, cnt_order_time)%>%
  dplyr::distinct()

cnt_order_time_table<-dplyr::left_join(toc_order, cnt_order_time, by=c("Day_time"="v2","V1"="toc"))%>%
    tidyr::spread(key = V1, cnt_order_time)
  
  
one_day_h<-df%>%
  dplyr::group_by(v2)%>%
  tidyr::nest()%>%
  dplyr::mutate(model=purrr::map(data, ~polr(bid_order~bid_vol+ask_vol+actual_spread+wait,data=.,method="probit",start=c(rep(0,4),rep(1:4)))))

one_day<-data.table()
for (i in 1:nrow(one_day_h)){
  date<-one_day_h[[i,1]]
  coeff<-one_day_h$model[[i]]$coefficients
  prob<-c(pnorm(one_day_h$model[[i]]$zeta[1]),
          pnorm(one_day_h$model[[i]]$zeta[2])-pnorm(one_day_h$model[[i]]$zeta[1]),
          pnorm(one_day_h$model[[i]]$zeta[3])-pnorm(one_day_h$model[[i]]$zeta[2]),
          pnorm(one_day_h$model[[i]]$zeta[4])-pnorm(one_day_h$model[[i]]$zeta[3]),
          1-pnorm(one_day_h$model[[i]]$zeta[4]))
  li1<-data.table(c(date,coeff,prob))%>%
    dplyr::mutate(key=row_number())%>%
    tidyr::spread(key, V1)
  
  one_day<-dplyr::bind_rows(one_day,li1)
}

one_day<-one_day%>%
  dplyr::rename(Date=`1`,  bid_vol=`2`, ask_vol=`3`, actual_spread=`4`, wait=`5`, order5=`6`, order4=`7`, order3=`8`, order2=`9`, order1=`10`)


join_one_day_cnt<-dplyr::inner_join(one_day, cnt_one_day, by = c("Date" = "v2"))
join_cnt_order_one_day<-dplyr::inner_join(join_one_day_cnt, cnt_order_one_day_table, by = c("Date" = "Day"))%>%
  dplyr::rename(count_order1 = `1`, count_order2 = `2`, count_order3 = `3`, count_order4 = `4`, count_order5 = `5`)

one_day<-dplyr::inner_join(join_cnt_order_one_day, cnt_order_time_table, by = c("Date" = "Day_time"))%>%
  dplyr::rename(toc11 = `11`, toc12 = `12`, toc13 = `13`, toc14 = `14`, toc15 = `15`, toc21 = `21`, toc22 = `22`, toc23 = `23`, toc24 = `24`, toc25 = `25`, toc31 = `31`, toc32 = `32`, toc33 = `33`, toc34 = `34`, toc35 = `35`,toc41 = `41`, toc42 = `42`, toc43 = `43`, toc44 = `44`, toc45 = `45`,toc51 = `51`, toc52 = `52`, toc53 = `53`, toc54 = `54`, toc55 = `55`,
                toc61 = `61`, toc62 = `62`, toc63 = `63`, toc64 = `64`, toc65 = `65`, toc71 = `71`, toc72 = `72`, toc73 = `73`, toc74 = `74`, toc75 = `75`, toc81 = `81`, toc82 = `82`, toc83 = `83`, toc84 = `84`, toc85 = `85`,toc91 = `91`, toc92 = `92`, toc93 = `93`, toc94 = `94`, toc95 = `95`,toc101 = `101`, toc102 = `102`, toc103 = `103`, toc104 = `104`, toc105 = `105`)

#最初の30分のみ
first_30_minutes<-d%>%
  dplyr::filter(time_flag==1)%>%
  dplyr::filter(v2>=20160928)%>%
  dplyr::select(v2, bid_order, bid_vol, ask_vol, actual_spread, wait, time_flag)

count_order_30_minutes<- data.table(table(first_30_minutes$bid_order))
count_order_30_minutes_table<-dplyr::left_join(order, count_order_30_minutes, by="V1")

first_30_minutes$bid_order<-factor(first_30_minutes$bid_order,levels=c("5","4","3","2","1"),ordered=TRUE)

##全部の日を使う
all_day_30_minutes_h<-polr(bid_order~bid_vol+ask_vol+actual_spread+wait,data=first_30_minutes,method="probit",start=c(rep(0,4),rep(1:4)), Hess=T)

all_day_30_minutes_coeff<-all_day_30_minutes_h$coefficients
all_day_30_minutes_prob<-c(pnorm(all_day_30_minutes_h$zeta[1]), pnorm(all_day_30_minutes_h$zeta[2])-pnorm(all_day_30_minutes_h$zeta[1]), pnorm(all_day_30_minutes_h$zeta[3])-pnorm(all_day_30_minutes_h$zeta[2]), pnorm(all_day_30_minutes_h$zeta[4])-pnorm(all_day_30_minutes_h$zeta[3]), 1-pnorm(all_day_30_minutes_h$zeta[4]))
all_day_30_minutes<-t(data.table(c(all_day_30_minutes_coeff, all_day_30_minutes_prob, count_order_30_minutes_table$N)))
colnames(all_day_30_minutes)<-c("bid_vol", "ask_vol", "spread", "wait", "order5", "order4", "order3", "order2", "order1", "count_order1", "count_order2", "count_order3", "count_order4", "count_order5")

##一日ずつ
cnt_one_day_30_minutes<-first_30_minutes%>%
  dplyr::group_by(v2)%>%
  dplyr::mutate(cnt_one_day = n())%>%
  dplyr::ungroup()%>%
  dplyr::select(v2, cnt_one_day)%>%
  dplyr::distinct() 

cnt_order_one_day_30_minutes<-d%>%
  dplyr::filter(time_flag==1)%>%
  dplyr::filter(v2>=20160928)%>%
  dplyr::select(v2, bid_order, bid_vol, ask_vol, actual_spread, wait, time_flag)%>%
  dplyr::mutate(bid_order=as.character(bid_order))%>%
  dplyr::group_by(v2, bid_order)%>%
  dplyr::mutate(cnt_order_day = n())%>%
  dplyr::ungroup()%>%
  dplyr::select(v2, bid_order, cnt_order_day)%>%
  dplyr::distinct()

cnt_order_one_day_30_minutes_table<-dplyr::left_join(day_order, cnt_order_one_day_30_minutes, by=c("Day"="v2", "V1"="bid_order"))%>%
    tidyr::spread(key = V1, value = cnt_order_day)
  
one_day_30_minutes_h<-first_30_minutes%>%
  dplyr::group_by(v2)%>%
  tidyr::nest()%>%
  dplyr::mutate(model=purrr::map(data, ~polr(bid_order~bid_vol+ask_vol+actual_spread+wait,data=.,method="probit",start=c(rep(0,4),rep(1:4)))))

one_day_30_minutes<-data.table()
for (i in 1:nrow(one_day_30_minutes_h)){
  date<-one_day_30_minutes_h[[i,1]]
  coeff<-one_day_30_minutes_h$model[[i]]$coefficients
  prob<-c(pnorm(one_day_30_minutes_h$model[[i]]$zeta[1]),
          pnorm(one_day_30_minutes_h$model[[i]]$zeta[2])-pnorm(one_day_30_minutes_h$model[[i]]$zeta[1]),
          pnorm(one_day_30_minutes_h$model[[i]]$zeta[3])-pnorm(one_day_30_minutes_h$model[[i]]$zeta[2]),
          pnorm(one_day_30_minutes_h$model[[i]]$zeta[4])-pnorm(one_day_30_minutes_h$model[[i]]$zeta[3]),
          1-pnorm(one_day_30_minutes_h$model[[i]]$zeta[4]))
  li1<-data.table(c(date,coeff,prob))%>%
    dplyr::mutate(key=row_number())%>%
    tidyr::spread(key, V1)
  
  one_day_30_minutes<-dplyr::bind_rows(one_day_30_minutes,li1)
}

one_day_30_minutes<-one_day_30_minutes%>%
  dplyr::rename(Date=`1`,  bid_vol=`2`, ask_vol=`3`, actual_spread=`4`, wait=`5`, order5=`6`, order4=`7`, order3=`8`, order2=`9`, order1=`10`)

join_one_day_cnt_30_minutes<-dplyr::inner_join(one_day_30_minutes, cnt_one_day_30_minutes, by = c("Date" = "v2"))
one_day_30_minutes<-dplyr::inner_join(join_one_day_cnt_30_minutes, cnt_order_one_day_30_minutes_table, by = c("Date" = "Day"))%>%
  dplyr::rename(count_order1 = `1`, count_order2 = `2`, count_order3 = `3`, count_order4 = `4`, count_order5 = `5`)

options(scipen=100)
options(digits=3)


write.csv(all_day, all_day_name, quote=FALSE, row.names=FALSE)
write.csv(one_day, one_day_name, quote=FALSE, row.names=FALSE)
write.csv(all_day_30_minutes, all_day_30_minutes_name, quote=FALSE, row.names=FALSE)
write.csv(one_day_30_minutes, one_day_30_minutes_name, quote=FALSE, row.names=FALSE)


