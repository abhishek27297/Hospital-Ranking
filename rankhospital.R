rankhospital<-function(state,outcome,num="best"){
  data<-read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors=FALSE)
  out<-c("heart attack","heart failure","pneumonia")
  no<-c(11,17,23)
  stat<-data[,7]
  p<-character()
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
    d<-sort(da[,3])
    if(num=="worst"){
      d<-max(d)
      p<-sort(subset(da[,1],da[,3]==d))
    }
    if(num=="best"){
      d<-min(d)
      p<-sort(subset(da[,1],da[,3]==d))
    }
    if(sum(num=="best" || num== "worst")==0){
      if(num>length(d)){p<-NA}
      else{
        p<-sort(subset(da[,1],da[,3]==d[[num]]))
      }
    }
  }
  }

  p[[length(p)]]
}


