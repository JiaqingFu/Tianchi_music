library(data.table)
library(forecast)
library(plyr)
library(xgboost)
x<-read.table('./mars_tianchi_songs.csv',sep=",",stringsAsFactors = F)
x<-data.table(x)
colnames(x)<-c('song_id','artist_id','publish_time','song_init_plays','Language','Gender')
y<-read.table('./mars_tianchi_user_actions.csv',sep=",",stringsAsFactors = F)
colnames(y)<-c('user_id','song_id','gmt_create','action_type','Ds')
y<-data.table(y)

y1<-setDT(y)[y$action_type==1]
y1$count<-1
y1$gmt_create<-NULL
y1<-y1[,count:=sum(count),by=.(song_id,Ds)]
y1$user_id<-NULL
y1<-unique(y1)


data_train<-y1[x,on='song_id']
data_train<-data_train[!is.na(Ds)]
setDT(data_train)[,year:=Ds%/%10000]
data_train[,day:=Ds%%100]
data_train[,month:=Ds%%10000%/%100]
data_train$Ds<-NULL
data_train$action_type<-NULL
data_train[,song_id:=as.factor(song_id)]
data_train[,artist_id:=as.factor(artist_id)]
song_int2chr<-unique(data.table(as.integer(data_train$song_id),as.character(data_train$song_id)))
artist_int2chr<-unique(data.table(as.integer(data_train$artist_id),as.character(data_train$artist_id)))
colnames(artist_int2chr)<-c("artist_id","char")
data_train[,song_id:=as.integer(song_id)]
data_train[,artist_id:=as.integer(artist_id)]
data_train[,count:=log1p(count)]

data_test<-data_train[month==7| month==8]
data_test[month==7]$month<-9
data_test[month==8]$month<-10
data_test$count<-NULL

labely<-as.matrix(data.frame(data_train$count))
ytrain<-data_train
ytrain$count<-NULL
bsty <- xgboost(data =as.matrix(ytrain), label = labely, max.depth = 20, eta = 0.1, nround = 100,
                nfold = 7, objective = "reg:linear")
forcy <- predict(bsty, as.matrix(data_test))
forcy<-expm1(forcy)
result<-cbind(forcy,data_test)
result<-result[,list(artist_id,year,month,day,forcy)]
result[,date:=year*10000+month*100+day]
result<-result[,list(artist_id,forcy,date)]
result<-unique(result[,forcy:=sum(forcy),by=.(artist_id,date)])
result<-result[date!=20150931]
result_j<-result[artist_int2chr,on='artist_id']
result_j<-result_j[,list(char,forcy,date)]
#result_j[,forcy:=round(forcy)]
write.table(result_j,file='./xgboost_logcount_float.csv',row.names = F,col.names = F,quote = F,sep = ',')
