library(data.table)
library(dplyr)
library(MASS)
library(ggplot2)
library(tidyverse)
library(car)
library(reshape2)
library(scales)
library(grid)
library(quantmod)
library(TTR)
library(ordinal)

library(arm)
source("fixed-polr.R")

B_bid<-fread("Before_bid_mod.csv")
B_ask<-fread("Before_ask_day.csv")
d<-fread("Before-new.csv")
write.csv(uS,"After_ask_mod",quote=FALSE, row.names=FALSE)



time<-uB%>%
  dplyr::mutate(V8=formatC(V8,width=6,flag="0"))%>%
  dplyr::mutate(time=as.POSIXct(paste(V2,V8),format="%Y%m%d %H%M%S"))

#とりあえず1日のグラフを作る（1株, 全株）
#Before_bid
Bb_time_1stock<-B_bid_day%>%
  dplyr::filter(V2==20150824)%>%
  dplyr::filter(V6==7203)%>%
  dplyr::mutate(flag=ifelse(V8>=90000&V8<93000,1,
                     ifelse(V8>=93000&V8<100000,2,
                     ifelse(V8>=100000&V8<103000,3,
                     ifelse(V8>=103000&V8<110000,4,
                     ifelse(V8>=110000&V8<=113000,5,
                     ifelse(V8>=123000&V8<130000,6,
                     ifelse(V8>=130000&V8<133000,7,
                     ifelse(V8>=133000&V8<140000,8,
                     ifelse(V8>=140000&V8<143000,9,
                     ifelse(V8>=143000&V8<=150000,10,NA
                      )))))))))))%>%
  dplyr::select(-a,-b)
  dplyr::group_by(flag)%>%
  dplyr::summarise(sum(svol))%>%


##1日の中でのorderの割合
order_percent_Bb<-Bb_time_1stock%>%
    dplyr::group_by(flag,o)%>%
    dplyr::summarise(count=n())

#単純なorderのカウント
count_order_Bb<-tidyr::spread(order_percent_Bb, key = o, value=count) 



g<-ggplot(order_percent_Bb, aes(x=o,y=count,na.rm=TRUE))
g<-g+geom_histogram(position = "identity",stat="identity")
print(g)

#１日の価格変化
%p_Bb<-Bb_time_1stock%>%
%  dplyr::mutate(as.POSIXct.Date(concat(V2,V8), %Y%m%d %H%M%s))
#その割合（もうチョットうまい書き方あると思う）
count_rate_Bb<-Bb_time_1stock%>%
  dplyr::group_by(flag,o)%>%
  dplyr::summarise(count=n())%>%
  dplyr::group_by(flag)%>%
  dplyr::mutate(total=count/sum(count)*100)%>%
  dplyr::select(-count)%>%
  tidyr::spread(key = o, value=total)


  
#Before_ask
Ba_time_1stock<-B_ask_day%>%
  dplyr::filter(V2==20150824)%>%
  dplyr::filter(V6==7203)%>%
  dplyr::mutate(flag=ifelse(V8>=90000&V8<93000,1,
                    ifelse(V8>=93000&V8<100000,2,
                    ifelse(V8>=100000&V8<103000,3,
                    ifelse(V8>=103000&V8<110000,4,
                    ifelse(V8>=110000&V8<=113000,5,
                    ifelse(V8>=123000&V8<130000,6,
                    ifelse(V8>=130000&V8<133000,7,
                    ifelse(V8>=133000&V8<140000,8,
                    ifelse(V8>=140000&V8<143000,9,
                    ifelse(V8>=143000&V8<=150000,10,NA
                )))))))))))
a<-Ba_time_1stock%>%
  dplyr::group_by(flag)%>%
  dplyr::summarise(sum(svol))

##1日の中でのorderの割合
order_percent_Ba<-Ba_time_1stock%>%
  dplyr::group_by(flag,o)%>%
  dplyr::summarise(count=n())

#カウント
percent_Ba<-tidyr::spread(order_percent_Ba, key = o, value=count) 

#割合
count_rate_Bb<-Bb_time_1stock%>%
  dplyr::group_by(flag,o)%>%
  dplyr::summarise(count=n())%>%
  dplyr::group_by(flag)%>%
  dplyr::mutate(total=count/sum(count)*100)%>%
  dplyr::select(-count)%>%
  tidyr::spread(key = o, value=total)

#Table5
Bb_dummy<-B_bid%>%
  dplyr::mutate(flag=ifelse(V8>=90000&V8<93000,1,
                          ifelse(V8>=93000&V8<100000,2,
                           ifelse(V8>=100000&V8<103000,3,
                           ifelse(V8>=103000&V8<110000,4,
                           ifelse(V8>=110000&V8<=113000,5,
                           ifelse(V8>=123000&V8<130000,6,
                           ifelse(V8>=130000&V8<133000,7,
                           ifelse(V8>=133000&V8<140000,8,
                          ifelse(V8>=140000&V8<143000,9,
                          ifelse(V8>=143000&V8<=150000,10,NA
                            )))))))))))

#dummy変数導入の準備
Bb_dummy_flag<-Bb_dummy%>%
  dplyr::mutate(t=formatC(V8,width=6,flag="0"))%>%
  dplyr::mutate(time=paste(V2,t,sep=""))%>%
  dplyr::group_by(V6, V2, flag)%>%
  dplyr::mutate(start=min(V8), end=max(V8))%>%
  dplyr::filter(V8==start|V8==end)%>%
  ungroup()%>%
  dplyr::group_by(V6,time,flag)%>%
  dplyr::summarise(mid=mean(m))%>%
  ungroup()%>%
  dplyr::mutate(V2=substring(time, 1, 8))

#価格変化グラフの確認（トヨタ：1日）
Bb_p_m<-B_bid%>%
  dplyr::filter(V6==7203)%>%
  dplyr::filter(V2==20150824)%>%
  dplyr::mutate(t=formatC(V8,width=6,flag="0"))%>%
  dplyr::mutate(time=paste(V2,t,sep=""))%>%
  dplyr::mutate(time=as.POSIXct(time,format="%Y%m%d %H%M%S"))%>%
  dplyr::select(t,time,m)%>%
  rename(midquote=m)%>%
  dplyr::mutate(t=as.integer(t))%>%
  dplyr::filter(t<=113000)

Bb_p_a<-B_bid%>%
  dplyr::filter(V6==7203)%>%
  dplyr::filter(V2==20150824)%>%
  dplyr::mutate(t=formatC(V8,width=6,flag="0"))%>%
  dplyr::mutate(time=paste(V2,t,sep=""))%>%
  dplyr::mutate(time=as.POSIXct(time,format="%Y%m%d %H%M%S"))%>%
  dplyr::select(t,time,m)%>%
  rename(midquote=m)%>%
  dplyr::mutate(t=as.integer(t))%>%
  dplyr::filter(t>=123000)

g<-ggplot(NULL)
g<-g+geom_line(data=Bb_p_m, aes(x=time, y=midquote))
g<-g+geom_line(data=Bb_p_a, aes(x=time, y=midquote))

print(g)
#奇数行のデータ
n <- seq(1, nrow(Bb_dummy_flag), by = 2)
#奇数
Bb_dummy_odd<-Bb_dummy_flag[n,]
#偶数
Bb_dummy_even<-Bb_dummy_flag[-n,]

#dummy変数の導入
Bb_make_dummy<-dplyr::inner_join(Bb_dummy_odd, Bb_dummy_even, by=c("V6","V2","flag"))%>%
  dplyr::mutate(dummy=ifelse(mid.x<mid.y,1,0))%>%
  dplyr::select(V6,V2,flag,dummy)
Bb_make_dummy$V2<-as.integer(Bb_make_dummy$V2)

#table5の形（完成形？）
Bb_table5<-dplyr::inner_join(Bb_dummy,Bb_make_dummy, by=c("V6","V2","flag"))

#Bb_upward  
Bb_upward<-Bb_table5%>%
  dplyr::filter(dummy==1)

Bb_upward$o<-factor(Bb_upward$o,levels=c("5","4","3","2","1"),ordered=TRUE)
h<-polr(o~svol+ovol+spread+wait,data=Bb_upward,method="probit",start=c(rep(0,4),rep(1:4)))

#Bb_downward  
Bb_downward<-Bb_table5%>%
  dplyr::filter(dummy==0)

Bb_downward$o<-factor(Bb_downward$o,levels=c("5","4","3","2","1"),ordered=TRUE)
h<-polr(o~svol+ovol+spread+wait,data=Bb_downward,method="probit",start=c(rep(0,4),rep(1:4)))


