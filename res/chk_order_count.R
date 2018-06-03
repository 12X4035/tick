#注文データのsummary
library(dplyr)
library(data.table)

bid_16<-fread("/Volumes/HDD2 1/tick_225/res/res_all_day_30_minutes/bid_16_all_day_30_minutes_res.csv")
bid_15_after<-fread("/Volumes/HDD2 1/tick_225/res/res_all_day_30_minutes/bid_15_after_all_day_30_minutes_res.csv")
bid_15_before<-fread("/Volumes/HDD2 1/tick_225/res/res_all_day_30_minutes/bid_15_before_all_day_30_minutes_res.csv")

ask_16<-fread("/Volumes/HDD2 1/tick_225/res/res_all_day_30_minutes/ask_16_all_day_30_minutes_res.csv")
ask_15_after<-fread("/Volumes/HDD2 1/tick_225/res/res_all_day_30_minutes/ask_15_after_all_day_30_minutes_res.csv")
ask_15_before<-fread("/Volumes/HDD2 1/tick_225/res/res_all_day_30_minutes/ask_15_before_all_day_30_minutes_res.csv")

bid_16[is.na(bid_16)] <- 0
bid_15_after[is.na(bid_15_after)] <- 0
bid_15_before[is.na(bid_15_before)]<-0

ask_16[is.na(ask_16)] <- 0
ask_15_after[is.na(ask_15_after)] <- 0
ask_15_before[is.na(ask_15_before)]<-0

bid_16<-bid_16%>%
  dplyr::mutate(bid_16 = count_order1+count_order2+count_order3+count_order4+count_order5)%>%
  dplyr::filter(stock==7203)
  dplyr::mutate(rank=min_rank(desc(bid_16)))
  dplyr::filter(rank<=10)
  #dplyr::select(stock, stock_name, category, order5_prob, order4_prob, order3_prob, order2_prob, order1_prob, bid_16)%>%
  #dplyr::mutate(order5 = round(order5_prob*bid_16))%>%
  #dplyr::mutate(order4 = round(order4_prob*bid_16))%>%
  #dplyr::mutate(order3 = round(order3_prob*bid_16))%>%
  #dplyr::mutate(order2 = round(order2_prob*bid_16))%>%
  #dplyr::mutate(order1 = round(order1_prob*bid_16))

bid_16_ctg<-bid_16%>%
  #dplyr::group_by(category)%>%
  dplyr::summarise_each(funs(sum), bid_16, count_order1, count_order2, count_order3, count_order4, count_order5)%>%
  #dplyr::ungroup()%>%
  dplyr::mutate(order_rate1 = (count_order1/bid_16)*100)%>%
  dplyr::mutate(order_rate2 = (count_order2/bid_16)*100)%>%
  dplyr::mutate(order_rate3 = (count_order3/bid_16)*100)%>%
  dplyr::mutate(order_rate4 = (count_order4/bid_16)*100)%>%
  dplyr::mutate(order_rate5 = (count_order5/bid_16)*100)%>%
  dplyr::select( bid_16, order_rate1, order_rate2, order_rate3, order_rate4, order_rate5)

bid_15_after<-bid_15_after%>%
  dplyr::mutate(bid_15_after = count_order1+count_order2+count_order3+count_order4+count_order5)%>%
  dplyr::filter(stock==7203)
  dplyr::mutate(rank=min_rank(desc(bid_15_after)))%>%
  dplyr::filter(rank<=10)
  #dplyr::select(stock, stock_name, category, order5_prob, order4_prob, order3_prob, order2_prob, order1_prob, bid_15_after)%>%
  #dplyr::mutate(order5 = round(order5_prob*bid_15_after))%>%
  #dplyr::mutate(order4 = round(order4_prob*bid_15_after))%>%
  #dplyr::mutate(order3 = round(order3_prob*bid_15_after))%>%
  #dplyr::mutate(order2 = round(order2_prob*bid_15_after))%>%
  #dplyr::mutate(order1 = round(order1_prob*bid_15_after))

bid_15_after_ctg<-bid_15_after%>%
  dplyr::group_by(category)%>%
  dplyr::summarise_each(funs(sum), bid_15_after, count_order1, count_order2, count_order3, count_order4, count_order5)%>%
  dplyr::ungroup()%>%
  dplyr::mutate(order_rate1 = (count_order1/bid_15_after)*100)%>%
  dplyr::mutate(order_rate2 = (count_order2/bid_15_after)*100)%>%
  dplyr::mutate(order_rate3 = (count_order3/bid_15_after)*100)%>%
  dplyr::mutate(order_rate4 = (count_order4/bid_15_after)*100)%>%
  dplyr::mutate(order_rate5 = (count_order5/bid_15_after)*100)%>%
  dplyr::select(category, bid_15_after, order_rate1, order_rate2, order_rate3, order_rate4, order_rate5)

bid_15_before<-bid_15_before%>%
  dplyr::mutate(bid_15_before = count_order1+count_order2+count_order3+count_order4+count_order5)%>%
  dplyr::filter(stock==7203)
  dplyr::mutate(rank=min_rank(desc(bid_15_before)))%>%
  dplyr::filter(rank<=10)
  #dplyr::select(stock, stock_name, category, order5_prob, order4_prob, order3_prob, order2_prob, order1_prob, bid_15_before)%>%
  #dplyr::mutate(order5 = round(order5_prob*bid_15_before))%>%
  #dplyr::mutate(order4 = round(order4_prob*bid_15_before))%>%
  #dplyr::mutate(order3 = round(order3_prob*bid_15_before))%>%
  #dplyr::mutate(order2 = round(order2_prob*bid_15_before))%>%
  #dplyr::mutate(order1 = round(order1_prob*bid_15_before))

bid_15_before_ctg<-bid_15_before%>%
  #dplyr::group_by(category)%>%
  dplyr::summarise_each(funs(sum), bid_15_before, count_order1, count_order2, count_order3, count_order4, count_order5)%>%
  #dplyr::ungroup()%>%
  dplyr::mutate(order_rate1 = (count_order1/bid_15_before)*100)%>%
  dplyr::mutate(order_rate2 = (count_order2/bid_15_before)*100)%>%
  dplyr::mutate(order_rate3 = (count_order3/bid_15_before)*100)%>%
  dplyr::mutate(order_rate4 = (count_order4/bid_15_before)*100)%>%
  dplyr::mutate(order_rate5 = (count_order5/bid_15_before)*100)%>%
  dplyr::select(bid_15_before, order_rate1, order_rate2, order_rate3, order_rate4, order_rate5)

ask_16<-ask_16%>%
  dplyr::mutate(ask_16 = count_order1+count_order2+count_order3+count_order4+count_order5)%>%
  dplyr::filter(stock==7203)
  dplyr::mutate(rank=min_rank(desc(ask_16)))%>%
  dplyr::filter(rank<=10)
  #dplyr::select(stock, stock_name, category, order5_prob, order4_prob, order3_prob, order2_prob, order1_prob, ask_16)%>%
  #dplyr::mutate(order5 = round(order5_prob*ask_16))%>%
  #dplyr::mutate(order4 = round(order4_prob*ask_16))%>%
  #dplyr::mutate(order3 = round(order3_prob*ask_16))%>%
  #dplyr::mutate(order2 = round(order2_prob*ask_16))%>%
  #dplyr::mutate(order1 = round(order1_prob*ask_16))

ask_16_ctg<-ask_16%>%
  #dplyr::group_by(category)%>%
  dplyr::summarise_each(funs(sum), ask_16, count_order1, count_order2, count_order3, count_order4, count_order5)%>%
  #dplyr::ungroup()%>%
  dplyr::mutate(order_rate1 = (count_order1/ask_16)*100)%>%
  dplyr::mutate(order_rate2 = (count_order2/ask_16)*100)%>%
  dplyr::mutate(order_rate3 = (count_order3/ask_16)*100)%>%
  dplyr::mutate(order_rate4 = (count_order4/ask_16)*100)%>%
  dplyr::mutate(order_rate5 = (count_order5/ask_16)*100)%>%
  dplyr::select(ask_16, order_rate1, order_rate2, order_rate3, order_rate4, order_rate5)

ask_15_after<-ask_15_after%>%
  dplyr::mutate(ask_15_after = count_order1+count_order2+count_order3+count_order4+count_order5)%>%
  dplyr::filter(stock==7203)
  dplyr::mutate(rank=min_rank(desc(ask_15_after)))%>%
  dplyr::filter(rank<=10)
  #dplyr::select(stock, stock_name, category, order5_prob, order4_prob, order3_prob, order2_prob, order1_prob, ask_15_after)%>%
  #dplyr::mutate(order5 = round(order5_prob*ask_15_after))%>%
  #dplyr::mutate(order4 = round(order4_prob*ask_15_after))%>%
  #dplyr::mutate(order3 = round(order3_prob*ask_15_after))%>%
  #dplyr::mutate(order2 = round(order2_prob*ask_15_after))%>%
  #dplyr::mutate(order1 = round(order1_prob*ask_15_after))

ask_15_after_ctg<-ask_15_after%>%
  dplyr::group_by(category)%>%
  dplyr::summarise_each(funs(sum), ask_15_after, count_order1, count_order2, count_order3, count_order4, count_order5)%>%
  dplyr::ungroup()%>%
  dplyr::mutate(order_rate1 = (count_order1/ask_15_after)*100)%>%
  dplyr::mutate(order_rate2 = (count_order2/ask_15_after)*100)%>%
  dplyr::mutate(order_rate3 = (count_order3/ask_15_after)*100)%>%
  dplyr::mutate(order_rate4 = (count_order4/ask_15_after)*100)%>%
  dplyr::mutate(order_rate5 = (count_order5/ask_15_after)*100)%>%
  dplyr::select(category, ask_15_after, order_rate1, order_rate2, order_rate3, order_rate4, order_rate5)

ask_15_before<-ask_15_before%>%
  dplyr::mutate(ask_15_before = count_order1+count_order2+count_order3+count_order4+count_order5)%>%
  dplyr::filter(stock==7203)
  dplyr::mutate(rank=min_rank(desc(ask_15_before)))%>%
  dplyr::filter(rank<=10)
  #dplyr::select(stock, stock_name, category, order5_prob, order4_prob, order3_prob, order2_prob, order1_prob, ask_15_before)%>%
  #dplyr::mutate(order5 = round(order5_prob*ask_15_before))%>%
  #dplyr::mutate(order4 = round(order4_prob*ask_15_before))%>%
  #dplyr::mutate(order3 = round(order3_prob*ask_15_before))%>%
  #dplyr::mutate(order2 = round(order2_prob*ask_15_before))%>%
  #dplyr::mutate(order1 = round(order1_prob*ask_15_before))

ask_15_before_ctg<-ask_15_before%>%
  #dplyr::group_by(category)%>%
  dplyr::summarise_each(funs(sum), ask_15_before, count_order1, count_order2, count_order3, count_order4, count_order5)%>%
  #dplyr::ungroup()%>%
  dplyr::mutate(order_rate1 = (count_order1/ask_15_before)*100)%>%
  dplyr::mutate(order_rate2 = (count_order2/ask_15_before)*100)%>%
  dplyr::mutate(order_rate3 = (count_order3/ask_15_before)*100)%>%
  dplyr::mutate(order_rate4 = (count_order4/ask_15_before)*100)%>%
  dplyr::mutate(order_rate5 = (count_order5/ask_15_before)*100)%>%
  dplyr::select( ask_15_before, order_rate1, order_rate2, order_rate3, order_rate4, order_rate5)


count_order<-dplyr::inner_join(bid_15_before_ctg, ask_15_before_ctg, by="category")%>%
  dplyr::mutate(rank=min_rank(desc(bid_15_before+ask_15_before)))%>%
  dplyr::summarise_each(funs(sum), bid_15_before, ask_15_before)

count_order<-format(count_order, digits=4)

count_order<-dplyr::inner_join(bid_16_ctg, ask_16_ctg, by="category")%>%
  dplyr::mutate(rank=min_rank(desc(bid_15_before+ask_15_before)))%>%
  dplyr::summarise_each(funs(sum), bid_15_before, ask_15_before)


options(scipen=100)
options(digits=4)
write.csv(count_order, "count_order.csv", quote=FALSE, row.names = FALSE, fileEncoding = "cp932")
