##Using 'plyr' package
rankall<-function(outcome,num="best"){
  data<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors=FALSE)
  out<-c("heart attack","heart failure","pneumonia")
  no<-c(11,17,23)
  states<-sort(unique(data[,7]))
  temp<-data.frame()
  c<-vector()
  wor<-numeric()
  if(sum(outcome==out)==0){
    stop("invalid outcome")
  }
 for(i in 1:3){
  if(outcome==out[[i]]){
    dat<-data[,c(2,7,no[[i]])]
    da<-arrange(dat,dat[,2],dat[,3])
   
   if(num=="worst"){
      for(i in 1:length(states)){
        tem<-da[which(da[,2]==states[i]),]
        fo<-tem[which(!is.na(tem[,3])),]
        wor[i]<-nrow(fo)
      }
    }
    
    for(i in 1:length(states)){
      tem<-da[which(da[,2]==states[i]),]
      fo<-tem[which(!is.na(tem[,3])),]
      if(sum(num=="best" || num== "worst")==0){
        te<-fo[num,1:2]
        temp[i,1]<-te[[1]]
        temp[i,2]<-te[[2]]
      }
      if(num=="worst"){
        te<-fo[wor[i],1:2]
        temp[i,1]<-te[[1]]
        temp[i,2]<-te[[2]]
      }
      
      if(num=="best"){
        te<-fo[1,1:2]
        temp[i,1]<-te[[1]]
        temp[i,2]<-te[[2]]
      }
      
      if(is.na(te[[1]])){
        temp[i,1]<-NA
        temp[i,2]<-states[[i]]
      }
      
    }
  } 
 }
  temp
}