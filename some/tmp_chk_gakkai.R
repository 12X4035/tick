g1<-(-0.213)
g2<-(1.356)
g3<-(2.650)
g4<-(3.092)

pnorm(g1)
pnorm(g2)-pnorm(g1)
pnorm(g3)-pnorm(g2)
pnorm(g4)-pnorm(g3)
1-pnorm(g4)

d_15_after<-fread("/Volumes/HDD2 1/tick_225/2015_after/bid/after_bid_15_csv/after_15_bid_7203.csv",
         col.names =c("row_num", "v2", "v6", "v5", "v10", "ask_vol", "bid_vol", "midquote", "actual_spread", "relative_spread", "bid_order", "wait", "timeorder_cnt", "fil_wait", "m_wait",
                      "diff_p_1", "diff_p_2", "diff_p_3", "diff_p_4", "diff_p_5", "diff_p_order", "m_diff_p_1min", "diff_p_m3", "diff_p_m3_order","time_flag"))

d_16<-fread("/Volumes/HDD2 1/tick_225/2016/bid/bid_16_csv/bid_16_7203.csv")
#0.1ミリ秒
df_15<-d_15_after%>%
  dplyr::filter(time_flag==1)%>%
  dplyr::select(wait)%>%
  filter(wait<=0.01)

df_16<-d_16%>%
  dplyr::filter(time_flag==1)%>%
  dplyr::select(wait)%>%
  filter(wait<=0.01)

g<-ggplot()
g<-g+geom_freqpoly(data=df_15, aes(x=wait, color="2015"), binwidth = 0.0001)+scale_x_continuous(breaks=seq(0,0.01, 0.001))
g<-g+geom_freqpoly(data=df_16, aes(x=wait, color="2016"), binwidth = 0.0001)+theme_bw(base_family = "HiraKakuPro-W3")+ggtitle("トヨタ自動車（買い注文）")

#+scale_x_continuous(breaks=seq(0,0.02))

