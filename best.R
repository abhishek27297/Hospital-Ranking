best<-function(state,outcome){
  data<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors=FALSE)
  out<-c("heart attack","heart failure","pneumonia")
  no<-c(11,17,23)
  stat<-data[,7]
  temp<-character()
  mi<-0
  if(sum(state==stat)==0){
    stop("invalid state")
  }
  if(sum(outcome==out)==0){
    stop("invalid outcome")
  }
  for(i in 1:3){
  if(outcome==out[[i]]){
    dat<-data[,c(2,7,no[[i]])]
    da<-dat[dat$State==state,]
    mi<-min(da[,3],na.rm=TRUE)
    te<-subset(da,da[,3]==mi)
    temp<-sort(te[[1]])
  }
  }

  temp[[1]]
  }
  
  
  
    
  
  
