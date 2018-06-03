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
B_bid<-fread("Before-bid.csv")
d<-fread("Before-bid.csv",header=FALSE)

d<-transform(d,date=as.integer(V9))
d<-transform(d,stock=as.integer(V6))
d<-transform(d,day=as.integer(V2))



d2<-d4%>%	
	dplyr::filter(date>=90000&date<=113000|date>=123000&date<=150000)%>%
	dplyr::filter(stock==9984)%>%	
	dplyr::mutate(a=as.POSIXct(paste(V2,V9),format="%Y%m%d %H%M%S"))%>%
	dplyr::select(a,V11,V12,V13,V14,V15,V17,V18,V19,V21,V22,V28,V29,V64,V65)%>%


	
d1$date<-as.character(d1$date)

d2<-d1%>%
	dplyr::mutate(a=as.POSIXct(paste(V2,V9),format="%Y%m%d %H%M%S"))%>% ##時間への変換##
	dplyr::filter(V13==1|V13==16|V13==32|V13==48|V13==64)%>%
	dplyr::mutate(t1=lead(a))%>%
	dplyr::mutate(t=(t1-a))
	
	
d2$t<-ifelse(is.na(d3$t),0,d3$t)
d3<-d2%>%
	dplyr::group_by(t)%>%
	dplyr::mutate(q=mean(t))
	
	dplyr::summarise(d3,diff(a))
dplyr::summarise(ask=mean(V19),bid=mean(V22),askv=mean(V18*V19),bidv=mean(V21*V22))%>%

=======================================================================================
d1<-d%>%	
	dplyr::select(date,V2,stock,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21,V22,V23)%>%
	dplyr::filter(date>=90000&date<=113000|date>=123000&date<=150000)%>%
	dplyr::filter(stock==7203|stock==9984|stock==6752|stock==6501|stock==6701|stock==7267|stock==6758|stock==7201|stock==6702|stock==7261)
d1$date<-as.character(d1$date)

d2<-d1%>%
	dplyr::mutate(a=as.POSIXct(paste(V2,V9),format="%Y%m%d %H%M%S"))%>%
	dplyr::select(a,stock,V11,V12,V13,V14,V15,V16,V17,V18,V19,V21,V22)%>%
	dplyr::filter(V13==1|V13==16|V13==32|V13==48|V13==64)


#
d3<-d2%>%
	dplyr::group_by(a)%>%
	dplyr::summarise(ask=mean(V19),bid=mean(V22),askv=mean(V18*V19/10000),bidv=mean(V21*V22/10000))%>%
	dplyr::mutate(t1=lag(a))%>%
	dplyr::mutate(t=(a-t1))

d3$t<-ifelse(is.na(d3$t),1,d3$t)

mean(d3$t)
mean(d3$ask)
mean(d3$askv)
mean(d3$bid)
mean(d3$bidv)

mean(sum(d3$ask*d3$t)/sum(d3$t))
mean(sum(d3$bid*d3$t)/sum(d3$t))		
==========================================================================================
#midquote 	
dm<-d2%>%
	dplyr::mutate(m=(V18+V21)/2)
	
mean(dm$m)	
==========================================================================================
#P



==========================================================================================
#Actual Spread & Spread Over tick
das<-d2%>%
	dplyr::mutate(as=(V18-V21))
mean(das$as)
mean(das,as/1)#←tick size
==========================================================================================
#Relative Spread		#spreadは１日単位でだすのか？

drs<-d2%>%
	dplyr::mutate(m=(V18+V21)/2)%>%
	dplyr::mutate(as=(V18-V21))%>%
	dplyr::mutate(rs=as/m*100)
	
		
mean(drs$rs)	
==========================================================================================
#vol
dv1<-dm%>%
	dplyr::mutate(r=ROC(m,type="continuous"))
dv1$r<-ifelse(is.na(dv1$r),0,dv1$r)


dv<-dv1%>%
	dplyr::mutate(mean=rollapply(r,20,mean,fill=NA))%>%
	dplyr::mutate(volat=rollapply(mean,20,sd,fill=NA))%>%
	dplyr::mutate(mean=lag(mean,n=10))%>%
	dplyr::mutate(volat=lag(volat,n=20))
	
	
	
sd(dv$mean,na.rm=TRUE)*1000
========================================
#table2
#order
g<-dv%>%
dplyr::mutate(order=ifelse(V13==16,
					ifelse(V12>V18,1,
					ifelse(V12==V18,2,
					ifelse(V12<V18 & V12>V21,3,
					ifelse(V12==V21,4,
					ifelse(V12<V21,5,NA))))),
				ifelse(V13==48,
					ifelse(V12<V21,6,
					ifelse(V12==V21,7,
					ifelse(V12<V18 & V12>V21,8,
					ifelse(V12==V18,9,
					ifelse(V12>V18,10,NA))))),
				0)))
#一応		
g2<-dv1%>%
dplyr::filter(V13==32)%>%				
dplyr::mutate(s=ifelse(V12<V21,1,
				ifelse(V12==V21,2,
				ifelse(V12<V18 & V12>V21,3,
				ifelse(V12==V18,4,
				ifelse(V12>V18,5,NA))))))

g1<-dv1%>%
dplyr::filter(V13==48)%>%				
dplyr::mutate(s=ifelse(V12<V21,1,
				ifelse(V12==V21,2,
				ifelse(V12<V18 & V12>V21,3,
				ifelse(V12==V18,4,
				ifelse(V12>V18,5,NA))))))
				g1<-g%>%
#
g1<-g%>%
dplyr::filter(order==3)
mean(g1$V19)#Buy depth
mean(g1$V22)#Sell depth
mean(g1$V18-g1$V21)#spread

g2<-g%>%
dplyr::group_by(order,stock)%>%
dplyr::mutate(wait=(a-lag(a)))%>%
ungroup()
g3<-g2%>%
dplyr::filter(order==1)

==========================================
#table3
uB<-g%>%
	dplyr::filter(V13==16)%>%
	dplyr::mutate(svol=ifelse(V13==16,V21/10000,V18/10000))%>%
	dplyr::mutate(ovol=ifelse(V13==16,V18/10000,V21/10000))%>%
	dplyr::mutate(spread=(V18-V21)/m)%>%
	dplyr::group_by(order)%>%
	dplyr::mutate(wait=(a-lag(a,n=3))/100)%>%
	ungroup()

uS<-g%>%
	dplyr::filter(V13==48)%>%
	dplyr::mutate(svol=ifelse(V13==16,V21/10000,V18/10000))%>%
	dplyr::mutate(ovol=ifelse(V13==16,V18/10000,V21/10000))%>%
	dplyr::mutate(spread=V18-V21)%>%
	dplyr::group_by(order)%>%
	dplyr::mutate(wait=(a-lag(a,n=3))/100)%>%
	ungroup()

uB$volat<-ifelse(is.na(uB$volat),1,uB$volat)
uB$wait<--ifelse(is.na(uB$wait),0,uB$wait)
	
uB$order<-factor(uB$order,levels=c("1","2","3","4"),ordered=TRUE)

h<-polr(order~svol+ovol+spread+wait+volat,data=uB,method="probit")
h<-clm(order~svol+ovol+spread+wait,data=uB,link="probit")

============================================
t<-das%>%
dplyr::group_by(a)%>%
dplyr::summarise(av=sum(V19),bv=sum(V22),asp=mean(as))
==============================================
#注文の判定
d1<-d%>%	
	dplyr::select(date,V2,stock,V9,V10,V11,V12,V13,V14,V15,V16,V17,V18,V19,V20,V21,V22,V23,V28,V29,V64,V65)%>%
	dplyr::filter(date>=90000&date<=113000|date>=123000&date<=150000)%>%
	dplyr::filter(stock==9984)%>%
	dplyr::mutate(p1=ifelse(V12==lag(V18),2,ifelse(V12==lag(V21),1,NA)))%>%
	dplyr::mutate(p2=ifelse(V18==lag(V18),4,ifelse(V21==lag(V21),3,NA)))%>%
	dplyr::mutate(p3=ifelse(V18<=lag(V18),V19-lag(V19)+lag(V15),ifelse(V21>=lag(V21),V22-lag(V22)+lag(V15),0)))
	
	
#NEW
d1<-d%>%	
	dplyr::filter(date>=90000&date<=113000|date>=123000&date<=150000)%>%
	#dplyr::filter(stock==9984)%>%	
	dplyr::mutate(a=as.POSIXct(paste(V2,V9),format="%Y%m%d %H%M%S"))%>%
	dplyr::select(a,stock,V5,V11,V12,V13,V14,V15,V17,V18,V19,V20,V21,V22,V23,V28,V29,V64,V65)
	
ds<-d1%>%
dplyr::mutate(Ap=V18-lag(V18))%>%
dplyr::mutate(Av=V19-lag(V19))%>%
dplyr::mutate(Bp=V21-lag(V21))%>%
dplyr::mutate(Bv=V22-lag(V22))%>%
dplyr::mutate(A1=V19-lag(V19)+V15)%>%
dplyr::mutate(B1=V22-lag(V22)+V15)%>%
dplyr::mutate(o=ifelse(Ap==0&Av==0&Bp==0&Bv==0,8,
				ifelse((is.na(V15)&Ap==0&Av!=0)|(is.na(V15)&Bp==0&Bv!=0),7,
				ifelse((is.na(V15)&Ap!=0&Av==0)|(is.na(V15)&Bp!=0&Bv==0),6,
				ifelse((is.na(V15)&Ap!=0&Av!=0)|(is.na(V15)&Bp!=0&Bv!=0),5,	
				ifelse((is.na(V28)&V13==16&V12==V18)|(is.na(V64)&V13==48&V12==V18),4,
				ifelse((is.na(V28)&V13==16&V12!=V18)|(is.na(V64)&V13==48&V12!=V18),3,
				ifelse((is.na(V28)&V64<V19)|(is.na(V64)&V28<V22),2,
				ifelse((is.na(V28)&V64>=V19)|(is.na(V64)&V28>=V22),1,NA)))))))))%>%
dplyr::filter(o==1|o==2|o==3|o==4|o==5|o==6|o==7|o==8)%>%
dplyr::mutate(Bdv=(V18*V19)/10000)%>%
dplyr::mutate(Adv=(V21*V22)/10000)%>%
dplyr::mutate(m=(V18+V21)/2)%>%
dplyr::mutate(as=(V18-V21))%>%
dplyr::mutate(rs=as/m*100)%>%
dplyr::mutate(r=ROC(m,type="continuous"))%>%
dplyr::filter(V20==128,V23==128)%>%
dplyr::mutate(t=a-lag(a))%>%
dplyr::mutate(t=ifelse(t>=3600,0,a-lag(a)))%>%
dplyr::mutate(t=ifelse(t<0,0,a-lag(a)))

ds$r<-ifelse(is.na(ds$r),0,ds$r)


ds1<-ds%>%
	dplyr::mutate(mean=rollapply(r,20,mean,fill=NA))%>%
	dplyr::mutate(volat=rollapply(mean,20,sd,fill=NA))%>%
	dplyr::mutate(mean=lag(mean,n=10))%>%
	dplyr::mutate(volat=lag(volat,n=20))
	
	
	
sd(dv$mean,na.rm=TRUE)*1000
		
dplyr::filter(Ap!=0&Av!=0&Bp!=0&Bv!=0)
db<-d1%>%
dplyr::filter(V13==48)


uB<-d%>%
	dplyr::filter(o==1|o==2|o==3|o==4|o==5)%>%
	dplyr::mutate(svol=V22/10000)%>%
	dplyr::mutate(ovol=V19/10000)%>%
	dplyr::mutate(spread=((V18-V21)/m)*1000)%>%
	dplyr::mutate(wait=lag(rollapply(tb,3,mean,fill=NA)))%>%
	dplyr::mutate(volat=volat/1000)%>%
	dplyr::select(a,b,V6,o,svol,ovol,spread,volat,wait)
	
	
	uB$wait<-ifelse(is.na(uB$wait),0,uB$wait)
	uB$volat<-ifelse(is.na(uB$volat),0.0001,uB$volat)
	
	uB$o<-factor(uB$o,levels=c("1","2","3","4","5"),ordered=TRUE)
	h<-polr(o~svol+ovol+spread+wait,data=uB,method="probit")
	
	uS<-d%>%
	dplyr::filter(o==6|o==7|o==8|o==9|o==10)%>%
	dplyr::mutate(svol=V19/10000)%>%
	dplyr::mutate(ovol=V22/10000)%>%
	dplyr::mutate(spread=((V18-V21)/m)*1000)%>%
	dplyr::mutate(wait=lag(rollapply(ta,3,mean,fill=NA)))%>%
	dplyr::mutate(volat=volat/1000)%>%
	dplyr::select(a,b,V6,o,svol,ovol,spread,volat,wait)
	uS$wait<-ifelse(is.na(uS$wait),0,uS$wait)
	uS$volat<-ifelse(is.na(uS$volat),0.0001,uS$volat)
	uS$o<-factor(uS$o,levels=c("6","7","8","9","10"),ordered=TRUE)
	