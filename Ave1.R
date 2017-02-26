library(data.table)
library(forecast)
library(plyr)
library(xgboost)
x<-read.table('./mars_tianchi_songs.csv',sep=",",stringsAsFactors = F)
x<-data.table(x)
colnames(x)<-c('song_id','artist_id','publish_time','song_init_plays','Language','Gender')
y<-read.table('./mars_tianchi_user_actions.csv',sep=",",stringsAsFactors = F)
colnames(y)<-c('user_id','song_id','gmt_create','action_type','Ds')

y1<-setDT(y)[y$action_type==1]
y1$count<-1

y1<-y1[,sum(count),by=.(song_id,Ds)]
y1<-y1[order(song_id,Ds)]

y2<-y1[x, on="song_id"]


y2<-y2[,sum(V1),by=.(artist_id,Ds)]
y2<-y2[!is.na(y2$Ds)]
re<-unique(y2[,ave(V1),by=.(artist_id)])

a<-as.Date('20150901', "%Y%m%d")
a<-a+0:59
a<-rep(a,50)
r<-cbind(re,a)
r$a<-format(r$a, "%Y%m%d")
write.table(r,file='./ave1.csv',row.names = F,col.names = F,quote = F,sep = ',')
