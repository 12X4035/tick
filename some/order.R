
library(dplyr)
library(data.table)
library(RcppRoll)
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



d_bid<-fread("table16_bid_mod.csv")


d_bid$a<-as.POSIXct(d_bid$a,format="%Y-%m-%d %H:%M:%OS")
  
d_statistic<-stat_15%>%
  dplyr::group_by(V3)%>%
  dplyr::mutate(bid_val=V19/10000, ask_val=V21/10000, rs=V24*1000)%>%
  dplyr::rename(wait=V28, bid_vol=V15, ask_vol=V11, midquote=V22, as=V23)%>%
  dplyr::summarise_each(funs(mean),  wait, bid_vol, bid_val, ask_vol, ask_val, midquote, as, rs)

d_stat2<-stat_15%>%
  dplyr::mutate(sot=ifelse(V3==9983, V23/10,
                    ifelse(V3==7203, V23/1,
                    ifelse(V3==9984, V23/1,
                    ifelse(V3==6758, V23/1,
                    ifelse(V3==7267, V23/1,
                    ifelse(V3==7261, V23/1,
                    ifelse(V3==6752, V23/0.5,
                    ifelse(V3==7201, V23/0.1,
                    ifelse(V3==6501, V23/0.1,
                    ifelse(V3==6702, V23/0.1,NA
                    )))))))))))
  dplyr::mutate(r=ROC(V22,type="continuous"))%>%
  dplyr::mutate(mean=rollapply(r,20,mean,fill=NA))%>%
  dplyr::mutate(volat=rollapply(mean,20,sd,fill=NA))%>%
  dplyr::mutate(mean=lag(mean,n=10))%>%
  dplyr::mutate(volat=lag(volat,n=20))

write.csv(d_statistic, "15_stat.csv")

tmp1<-d_ask%>%
  dplyr::filter(V2==20160923)

stat_15<-dplyr::bind_rows(d_bid, d_ask)

d_bid<-d_bid%>%
  dplyr::select( V11, V15, V22, V23, V24, V25, V28)%>%
  dplyr::mutate(svol=V15/10000)%>%
  dplyr::mutate(ovol=V11/10000)%>%
  dplyr::mutate(spread=V24*1000)%>%
  dplyr::rename(order=V25, wait=V28)
  
d_ask<-d_ask%>%
  dplyr::select( V11, V15, V22, V23, V24, V25, V28)%>%
  dplyr::mutate(svol=V11/10000)%>%
  dplyr::mutate(ovol=V15/10000)%>%
  dplyr::mutate(spread=V24*1000)%>%
  dplyr::rename(order=V25, wait=V28)

d_bid$wait<-ifelse(is.na(d_bid$wait), 0, d_bid$wait)
d_ask$wait<-ifelse(is.na(d_ask$wait), 0, d_ask$wait)
%論文にはrsを100で割ると書いているが, 1000で割る. 単位を合わせるため%
%仮に100にした場合数値は変わらないが単純に10倍される%
%つまり,変数同士のパラメーターの大きさによる比較は出来ない%

d_bid$wait<-ifelse(is.na(d_bid$wait), 0, d_bid$wait)
d$V1<-as.POSIXct(d$V1,format="%Y-%m-%d %H:%M:%OS")##時間への変換##
d_bid$order<-factor(d_bid$order,levels=c("5","4","3","2","1"),ordered=TRUE)
d_ask$order<-factor(d_ask$order,levels=c("5","4","3","2","1"),ordered=TRUE)

h<-polr(order~svol+ovol+spread+wait,data=check,method="probit",start=c(rep(0,4),rep(1:4)))
11h<-polr(order~svol+ovol+spread+wait,data=d_ask,method="probit",start=c(rep(0,4),rep(1:4)))


make_time<-d_bid%>%
  dplyr::mutate(a=substr(V1,12,13))%>%
  dplyr::mutate(b=substr(V1,15,16))%>%
  dplyr::mutate(c=paste(a,b, sep=""))

make_time$c<-as.integer(make_time$c)

Bb_dummy<-make_time%>%
  dplyr::mutate(flag=ifelse(c>=900&c<930,1,
                     ifelse(c>=930&c<1000,2,
                     ifelse(c>=1000&c<1030,3,
                     ifelse(c>=1030&c<1100,4,
                     ifelse(c>=1100&c<=1130,5,
                     ifelse(c>=1230&c<1300,6,
                     ifelse(c>=1300&c<1330,7,
                     ifelse(c>=1330&c<1400,8,
                     ifelse(c>=1400&c<1430,9,
                     ifelse(c>=1430&c<=1500,10,NA
                     )))))))))))

#dummy変数導入の準備
Bb_dummy_flag<-Bb_dummy%>%
  dplyr::mutate(t=formatC(c,width=4,flag="0"))%>%
  dplyr::mutate(time=paste(V2,t,sep=""))%>%
  dplyr::group_by(V3, V2, flag)%>%
  dplyr::mutate(start=min(c), end=max(c))%>%
  dplyr::filter(c==start|c==end)%>%
  ungroup()%>%
  dplyr::group_by(V3,time,flag)%>%
  dplyr::summarise(mid=mean(V22))%>%
  ungroup()%>%
  dplyr::mutate(V2=substring(time, 1, 8))


#奇数行のデータ
n <- seq(1, nrow(Bb_dummy_flag), by = 2)
#奇数
Bb_dummy_odd<-Bb_dummy_flag[n,]
#偶数
Bb_dummy_even<-Bb_dummy_flag[-n,]

#dummy変数の導入
Bb_make_dummy<-dplyr::inner_join(Bb_dummy_odd, Bb_dummy_even, by=c("V3","V2","flag"))%>%
  dplyr::mutate(dummy=ifelse(mid.x<mid.y,1,0))%>%
  dplyr::select(V3,V2,flag,dummy)
Bb_make_dummy$V2<-as.integer(Bb_make_dummy$V2)

#table5の形（完成形？）
Bb_table5<-dplyr::inner_join(Bb_dummy,Bb_make_dummy, by=c("V3","V2","flag"))

#Bb_upward  
Bb_upward<-Bb_table5%>%
  dplyr::filter(dummy==1)%>%
  dplyr::mutate(svol=V15/10000)%>%
  dplyr::mutate(ovol=V11/10000)%>%
  dplyr::mutate(spread=V24*1000)%>%
  dplyr::rename(order=V25, wait=V28)

Bb_upward$order<-factor(Bb_upward$order,levels=c("5","4","3","2","1"),ordered=TRUE)
h<-polr(order~svol+ovol+spread+wait,data=Bb_upward,method="probit",start=c(rep(0,4),rep(1:4)))

#Bb_downward  
Bb_downward<-Bb_table5%>%
  dplyr::filter(dummy==0)%>%
  dplyr::mutate(svol=V15/10000)%>%
  dplyr::mutate(ovol=V11/10000)%>%
  dplyr::mutate(spread=V24*1000)%>%
  dplyr::rename(order=V25, wait=V28)

Bb_downward$order<-factor(Bb_downward$order,levels=c("5","4","3","2","1"),ordered=TRUE)
p<-polr(order~svol+ovol+spread+wait,data=Bb_downward,method="probit",start=c(rep(0,4),rep(1:4)))


#orderの割合
table(d_bid$V25)

sum(table(d_bid$V25))

o1<-d_bid%>%
  dplyr::group_by(V6)%>%
  dplyr::summarise(mean(wait), var(wait), sd(wait))
  dplyr::mutate(wait=V1-lag(V1))%>%
  dplyr::filter(V3==7203)%>%
  dplyr::filter(V5==2)
  dplyr::summarise(mean(wait,na.rm=T), sd(wait, na.rm=T))
  dplyr::mutate(r=ROC(V22,type="continuous"))
  dplyr::mutate(mean=rollapply(r,20,mean,fill=NA))%>%
  dplyr::mutate(volat=rollapply(mean,20,sd,fill=NA))%>%
  dplyr::summarise_each(funs(mean(., na.rm=T)), V15, V11, V24, V28, wait, volat)


t<-split(d_bid, d_bid$V3)

t<-d_bid%>%
  dplyr::group_by(V3)%>%
  dplyr::select(V28)%>%
  do(val=data.frame(.))

#timeをorderごとにする
count<-d_bid%>%
  dplyr::group_by(order)%>%
  dplyr::summarise(count=n())%>%
  dplyr::mutate(percent=count/sum(count)*100)
wait<-d_bid%>%
  dplyr::group_by(order)%>%
  dplyr::summarise_each(funs(mean),  svol, ovol, spread, wait)


  

fill_na$o<-factor(fill_na$o,levels=c("5","4","3","2","1"),ordered=TRUE)
d_ask$order<-factor(d_ask$order,levels=c("5","4","3","2","1"),ordered=TRUE)

h<-polr(o~svol+ovol+spread+wait_re,data=fill_na,method="probit",start=c(rep(0,4),rep(1:4)))
h<-clm(order~svol+ovol+spread+wait,data=d_bid,link="probit")


#15年版の時間
d_bid$a<-as.POSIXct(d_bid$a,format="%Y-%m-%d %H:%M:%OS")
time<-d_bid%>%
  dplyr::mutate(flag=ifelse(V8<123000, 1, 2))%>%
  dplyr::group_by(V2, flag, o)%>%
  dplyr::mutate(wait_n=rollapply(a-lag(a, default=0),3,mean,fill=0))

count<-d_bid%>%
  dplyr::group_by(o)%>%
  dplyr::summarise(count=n())%>%
  dplyr::mutate(percent=count/sum(count)*100)
wait<-d_bid%>%
  dplyr::group_by(o)%>%
  dplyr::summarise_each(funs(mean),  svol, ovol, spread)

#寄付きと引けのみ
check<-Bb_dummy%>%
  dplyr::select( V11, V15, V22, V23, V24, V25, V28, flag)%>%
  dplyr::filter(flag==1|flag==5|flag==6|flag==10)%>%
  dplyr::mutate(svol=V15/10000)%>%
  dplyr::mutate(ovol=V11/10000)%>%
  dplyr::mutate(spread=V24*1000)%>%
  dplyr::rename(order=V25, wait=V28)