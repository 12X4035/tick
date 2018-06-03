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

d<-fread("Before.csv",header=FALSE)

d<-transform(d,date=as.integer(V10))
d<-transform(d,stock=as.integer(V6))
d<-transform(d,day=as.integer(V2))



d2<-d4%>%	
	dplyr::filter(date>=90000&date<=113000|date>=123000&date<=150000)%>%
	dplyr::filter(stock==9984)%>%	
	dplyr::mutate(a=as.POSIXct(paste(V2,V9),format="%Y%m%d %H%M%S"))%>%
	dplyr::select(a,V11,V12,V13,V14,V15,V17,V18,V19,V21,V22,V28,V29,V64,V65)%>%


	
d1$date<-as.character(d1$date)

d2<-d1%>%
	dplyr::mutate(a=as.POSIXct(paste(V2,V9),format="%Y%m%d %H%M%S"))%>% ##譎る俣縺ｸ縺ｮ螟画鋤##
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
mean(das,as/1)#竊腎ick size
==========================================================================================
#Relative Spread		#spread縺ｯ・第律蜊倅ｽ阪〒縺縺吶・縺具ｼ・

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
#荳蠢・	
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
g1<-ds%>%
dplyr::filter(o==1)
mean(g1$V19)#Buy depth
mean(g1$V22)#Sell depth
mean(g1$V18-g1$V21)#spread

g2<-ds%>%
dplyr::group_by(o,stock)%>%
dplyr::mutate(wait=(a-lag(a)))%>%
ungroup()
g3<-g2%>%
dplyr::filter(order==1)

==========================================
#table3
uB<-ds%>%
	dplyr::filter(o==1|o==2|o==3|o==4|o==5)%>%
	dplyr::mutate(svol=V21/10000)%>%
	dplyr::mutate(ovol=V18/10000)%>%
	dplyr::mutate(spread=(V18-V21)/m)%>%
	dplyr::mutate(r=ROC(t,type="continuous"))%>%
	dplyr::mutate(wait=rollapply(r,3,mean,fill=NA))%>%

uS<-g%>%
	dplyr::filter(V13==48)%>%
	dplyr::mutate(svol=ifelse(V13==16,V21/10000,V18/10000))%>%
	dplyr::mutate(ovol=ifelse(V13==16,V18/10000,V21/10000))%>%
	dplyr::mutate(spread=V18-V21)%>%
	dplyr::mutate(wait=(a-lag(a,n=3))/100)%>%


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
#豕ｨ譁・・蛻､螳・
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
	dplyr::filter(stock==6702)%>%	
	dplyr::mutate(a=as.POSIXct(paste(V2,V9),format="%Y%m%d %H%M%S"))%>%
	dplyr::select(a,stock,V5,V11,V12,V13,V14,V15,V17,V18,V19,V20,V21,V22,V23,V28,V29,V64,V65)
	
ds1<-d1%>%
dplyr::mutate(Ap=V18-lag(V18))%>%
dplyr::mutate(Av=V19-lag(V19))%>%
dplyr::mutate(Bp=V21-lag(V21))%>%
dplyr::mutate(Bv=V22-lag(V22))

ds1$V28<-ifelse(is.na(ds1$V28),0,ds1$V28)
ds1$V64<-ifelse(is.na(ds1$V64),0,ds1$V64)
ds2<-ds1%>%
dplyr::mutate(c=ifelse(V13==16&Bp>=0,Bv+V15,
				ifelse(V13==48&Ap<=0,Av+V15
								,NA)))%>%
dplyr::mutate(d=ifelse(is.na(c)&V28!=0,V28-lag(V28)+V15,
				ifelse(is.na(c)&V64!=0,V64-lag(V64)+V15
								,c)))%>%
dplyr::mutate(e=ifelse(is.na(d)&Bp<0,0,
				ifelse(is.na(d)&Ap>0,0
								,d)))
#ds2$c<-ifelse(is.na(ds2$c),0,ds$c)
ds3<-ds2%>%
dplyr::mutate(o=ifelse(Ap==0&Av==0&Bp==0&Bv==0,15,
				ifelse(is.na(V15)&Bp>0,3,	#豌鈴・繧呈峩譁ｰ縺吶ｋ霑ｽ蜉雋ｷ縺・欠蛟､豕ｨ譁・
				ifelse(is.na(V15)&Bp==0&Bv>0,4,#豌鈴・繧呈峩譁ｰ縺励↑縺・ｿｽ蜉雋ｷ縺・欠蛟､豕ｨ譁・			
				ifelse((is.na(V15)&Bp<0)|(is.na(V13)&Bp==0&Bv<0),5,	#繧ｭ繝｣繝ｳ繧ｻ繝ｫ
				ifelse(is.na(V15)&Ap<0,8,	#豌鈴・繧呈峩譁ｰ縺吶ｋ霑ｽ蜉螢ｲ繧頑欠蛟､豕ｨ譁・
				ifelse(is.na(V15)&Ap==0&Av>0,9,	#豌鈴・繧呈峩譁ｰ縺励↑縺・ｿｽ蜉螢ｲ繧頑欠蛟､豕ｨ譁・
				ifelse((is.na(V15)&Ap>0)|(is.na(V15)&Ap==0&Av<0),10,#繧ｭ繝｣繝ｳ繧ｻ繝ｫ
				ifelse(V13==16&e==0,0,
				ifelse(V13==48&e==0,0,
				ifelse(e>0&V28==0&V29==0&V64>=V19,1,	#豌鈴・繧呈峩譁ｰ縺吶ｋ謌占｡瑚ｲｷ縺・ｳｨ譁・
				ifelse(e>0&V28==0&V29==0&V64<V19,2,	#豌鈴・繧呈峩譁ｰ縺励↑縺・・陦瑚ｲｷ縺・ｳｨ譁・
				ifelse(e>0&V64==0&V65==0&V28>=V22,6,	#豌鈴・繧呈峩譁ｰ縺吶ｋ謌占｡悟｣ｲ繧頑ｳｨ譁・
				ifelse(e>0&V64==0&V65==0&V28<V22,7,	#豌鈴・繧呈峩譁ｰ縺励↑縺・・陦悟｣ｲ繧頑ｳｨ譁・
				NA))))))))))))))	%>%	
dplyr::mutate(Adv=(V18*V19)/10000)%>%
dplyr::mutate(Bdv=(V21*V22)/10000)%>%
dplyr::mutate(m=(V18+V21)/2)%>%
dplyr::mutate(as=(V18-V21))%>%
dplyr::mutate(rs=as/m*100)%>%
dplyr::filter(V20==128|V23==128)%>%
dplyr::mutate(r=ROC(m,type="continuous"))%>%
dplyr::mutate(t=a-lag(a))%>%
dplyr::mutate(t=ifelse(t>=3600,0,ifelse(t<0,0,a-lag(a))))



ds3$V13<-ifelse(is.na(ds3$V13),0,ds3$V13)



ds<-ds3%>%
dplyr::filter(V13!=1)%>%
dplyr::filter(V17==0)%>%
dplyr::filter(o==1|o==2|o==3|o==4|o==5|o==6|o==7|o==8|o==9|o==10)%>%
dplyr::mutate(r=ROC(m,type="discrete"))%>%
dplyr::mutate(t=a-lag(a))%>%
dplyr::mutate(t=ifelse(t>=3600,0,ifelse(t<0,0,a-lag(a))))

ds$r<-ifelse(is.na(ds$r),0,ds$r)
ds$t<-ifelse(is.na(ds$t),0,ds$t)

ds1<-ds%>%
	dplyr::mutate(r=ROC(m,type="discrete"))%>%
	dplyr::mutate(volat=rollapply(r,20,sd,fill=NA))%>%
	dplyr::mutate(volat=lag(volat,n=10)*1000)

mean(ds1$t)
mean(ds1$V22)
mean(ds1$Bdv)
mean(ds1$V19)
mean(ds1$Adv)
mean(ds1$m)
mean(ds1$as)
mean(ds1$rs)
da<-dplyr::mutate(ds1,so=as/0.1)
mean(da$so)
	
mean(ds1$volat,na.rm=TRUE)
	
sd(dv$mean,na.rm=TRUE)*1000
		
dplyr::filter(Ap!=0&Av!=0&Bp!=0&Bv!=0)
db<-d1%>%
dplyr::filter(V13==48)
--------------------------
dplyr::mutate(o=ifelse(Ap==0&Av==0&Bp==0&Bv==0,8,
				ifelse((is.na(V15)&Ap==0&Av!=0)|(is.na(V15)&Bp==0&Bv!=0),7,
				ifelse((is.na(V15)&Ap!=0&Av==0)|(is.na(V15)&Bp!=0&Bv==0),6,
				ifelse((is.na(V15)&Ap!=0&Av!=0)|(is.na(V15)&Bp!=0&Bv!=0),5,	
				ifelse((is.na(V28)&V13==16&V12==V21)|(is.na(V64)&V13==48&V12==V18),4,
				ifelse((is.na(V28)&V13==16&V12!=V21)|(is.na(V64)&V13==48&V12!=V18),3,
				ifelse((is.na(V28)&V64<V19)|(is.na(V64)&V28<V22),2,
				ifelse((is.na(V28)&V64>=V19)|(is.na(V64)&V28>=V22),1,NA)))))))))%>%
dplyr::mutate(p=ifelse(o==1,1,
				ifelse(o==2,2,
				ifelse(o==3,1,
				ifelse(o==4,2,
				ifelse(o==5&(Ap>0|Bp>0),3,
				ifelse((o==6|o==7)&(Ap>0|Av>0|Bp>0|Bv>0),4,
				ifelse((o==5|o==6|o==7)&(Ap<0|Av<0|Bp<0|Bv<0),5,
				ifelse(o==8,6,NA)))))))))%>%


dplyr::mutate(o=ifelse(is.na(V28)&V64>=V19,1
				ifelse(is.na(V28)&V64<V19,2
				ifelse(is.na(V28&V64)&Bp>0,3
				ifelse(is.na(V28)&V64>=V19,1
				ifelse(is.na(V28)&V64>=V19,1
				ifelse(is.na(V64)&V28>=V22,6
				ifelse(is.na(V64)&V28<V22,7
				ifelse(is.na(V28&V64)&Ap<0,8
				ifelse(is.na(V28)&V64>=V19),1				
				ifelse(Ap==0&Av==0&Bp==0&Bv==0,8,

#dplyr::mutate(A1=V19-lag(V19)+V15)%>%
#dplyr::mutate(B1=V22-lag(V22)+V15)%>%
dplyr::mutate(o=ifelse(V13==48&V12!=V18,1,
				ifelse(V13==48&V12==V18,2,
				ifelse(is.na(V15)&Bp>0,3,
				ifelse(is.na(V15)&Bp==0&Bv>0,4,
				ifelse((is.na(V15)&Bp<0)|(is.na(V15)&Bp==0&Bv<0),5,NA))))))
				ifelse(V13==16&V12!=V21,6,
				ifelse(V13==16&V12==V21,7,
				ifelse(is.na(V15)&Ap<0,8,
				ifelse(is.na(V15)&Ap==0&Av>0,9,
				ifelse((is.na(V15)&Ap>0)|(is.na(V15)&Ap==0&Av<0),10,
				ifelse(Ap==0&Av==0&Bp==0&Bv==0,11,NA))))))))))))%>%

#table2====================================
g1<-ds%>%
dplyr::filter(o==1)
mean(g1$V19)#Buy depth
mean(g1$V22)#Sell depth
mean(g1$V18-g1$V21)#spread

g2<-g1%>%
dplyr::group_by(o,stock)%>%
dplyr::mutate(wait=(a-lag(a)))%>%
ungroup()%>%
dplyr::select(a,stock,V11,V12,V13,V15,V18,V19,V21,V22,V28,V29,V64,V65,Ap,Av,Bp,Bv,o,Bdv,Adv,m,as,rs,r,wait)

#============================================
d1<-d%>%	
	dplyr::filter(date>=90000&date<=113000|date>=123000&date<=150000)%>%
	dplyr::filter(V2==20150824)%>%
	dplyr::filter(stock==7203)%>%
	dplyr::mutate(x=ifelse(date>=90000&date<=92999,0930,
					ifelse(date>=93000&date<=95999,1000,
					ifelse(date>=100000&date<=102999,1030,
					ifelse(date>=103000&date<=105999,1100,
					ifelse(date>=110000&date<=113000,1130,
					ifelse(date>=123000&date<=125999,1300,
					ifelse(date>=130000&date<=132999,1330,
					ifelse(date>=133000&date<=135999,1400,
					ifelse(date>=140000&date<=142999,1430,
					ifelse(date>=143000&date<=150000,1500,NA)))))))))))%>%

						
	dplyr::mutate(a=as.POSIXct(paste(V2,V9),format="%Y%m%d %H%M%S"))%>%
	dplyr::select(a,stock,V5,V11,V12,V13,V14,V15,V17,V18,V19,V20,V21,V22,V23,V28,V29,V64,V65,x)

dm<-ds%>%
dplyr::group_by(x)%>%
dplyr::summarise(bid=sum(V22),ask=sum(V19),s=mean(as))%>%
dplyr::mutate(buyvol=scale(bid),sellvol=scale(ask),spread=scale(s))%>%
dplyr::select(x,buyvol,sellvol,spread)%>%
dplyr::mutate(t=formatC(x,width=4,flag="0"))

dm1<-melt(dm,id.vars="t",measure.vars=c("buyvol","sellvol","spread"))
dm1<-transform(dm1, date= as.POSIXct(as.character(t), format="%H%M"))


g<-ggplot(dm1,aes(x=date,y=value))
g<-g+geom_line(aes(linetype=variable))
g<-g+geom_point(aes(shape=variable),size=3)+scale_shape(solid=FALSE)
g<-g+theme_bw()+xlab("time")+ylab("Standardized Level")

