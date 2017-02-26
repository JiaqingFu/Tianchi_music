library(data.table)
library(forecast)
library(plyr)
library(xgboost)
x<-read.table('./mars_tianchi_songs.csv',sep=",",stringsAsFactors = F)
colnames(x)<-c('song_id','artist_id','publish_time','song_init_plays','Language','Gender')
y<-read.table('./mars_tianchi_user_actions.csv',sep=",",stringsAsFactors = F)

y1<-setDT(y)[y$V4==1]
y1$count<-1
y1<-y1[,sum(count),by=.(V2,V5)]
y1<-y1[order(V2,V5)]
colnames(y1)<-c('song_id','date','count')

y2<-y1[x, on="song_id"]

r<-y2[,sum(count),by=.(artist_id,date)]

r[,date:=as.Date(as.character(date), "%Y%m%d")]
r1<-r
r1$date<-r1$date+70
r1<-r1[r1$date>=as.Date('20150901',"%Y%m%d") & r1$date<=as.Date('20151030',"%Y%m%d")]

r1$date<-format(r1$date, "%Y%m%d")
r1<-r1[,list(artist_id,V1,date)]
write.table(r1,file='./ave2.csv',row.names = F,col.names = F,quote = F,sep = ',')

# r1_1<-r1[r1$date>=as.Date('20150701',"%Y%m%d") & r1$date<=as.Date('20150830',"%Y%m%d")]
# r1_2<-r1[r1$date>=as.Date('20150501',"%Y%m%d") & r1$date<=as.Date('20150630',"%Y%m%d")]
# r1_1<-r1_1[order(date,artist_id)]
# r1_2<-r1_2[order(date,artist_id)]
# 
# colnames(r1_2)[3]<-"V2"
# r2<-r1_1[r1_2,on=c("date","artist_id")]

