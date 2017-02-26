library(data.table)
library(forecast)
library(plyr)
library(xgboost)
x<-read.table('./p2_mars_tianchi_songs.csv',sep=",",stringsAsFactors = F)
x<-data.table(x)
colnames(x)<-c('song_id','artist_id','publish_time','song_init_plays','Language','Gender')
y<-read.table('./p2_mars_tianchi_user_actions.csv',sep=",",stringsAsFactors = F)
colnames(y)<-c('user_id','song_id','gmt_create','action_type','Ds')
y<-data.table(y)

y1<-setDT(y)[y$action_type==1]
y1$count<-1
y1$gmt_create<-NULL
y1<-y1[,count:=sum(count),by=.(song_id,Ds)]
y1$user_id<-NULL
y1<-unique(y1)
y1<-y1[order(song_id,Ds)]
o_d<-1:61+as.Date("20150830","%Y%m%d")

ts_fit = function(x) {
  a=vector(mode="integer",length = 183)
  index=as.Date(as.character(x$Ds), "%Y%m%d")-as.Date("20150228","%Y%m%d")
  print(index)
  a[index]=x$count
  b<-ts(a,frequency = 7)
  #fit <- auto.arima(b)
  #fit <- tslm(b~trend+season)
  #p<-hw(b,h=61,seasonal = 'additive')
  p<-stlf(b,h=61)
#   fit<-ets(b)
#   p<-forecast(fit,h=61)
  o<-p$mean
  o[o<0]<-0
  o<-round(o)
  #fit <- tslm(Sales ~ trend + season + DayOfWeek + Open + Promo + StateHoliday + SchoolHoliday)
  return(data.table(ds=o_d,pred=o))
}

out<-y1[,ts_fit(.SD),by=.(song_id)]
out1<-out
out1$ds<-format(out1$ds, "%Y%m%d")
out1<-out1[ds!="20150831"]

out1<-out1[x,on='song_id']
out1<-out1[,sum(pred),by=.(artist_id,ds)]
out1<-out1[,list(artist_id,V1,ds)]
out1<-out1[!is.na(ds)]
write.table(out1,file='./ts_stlf.csv',row.names = F,col.names = F,quote = F,sep = ',')
