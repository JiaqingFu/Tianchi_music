library(data.table)
library(forecast)
e<-read.table('./ts_ets.csv',sep = ',',stringsAsFactors = F)
e<-data.table(e)
a<-read.table('./ts_arima.csv',sep = ',',stringsAsFactors = F)
a<-data.table(a)

join<-a[e,on=c('V1','V3')]
a_w<-14650/(14837+14650)
e_w<-1-a_w
join<-join[,Pred:=V2*a_w+i.V2*e_w,by=.(V1,V3)]
result<-join[,list(V1,Pred,V3)]
result$Pred<-round(result$Pred)
write.table(result,file='./ts_em_arima_ets.csv',row.names = F,col.names = F,quote = F,sep = ',')
