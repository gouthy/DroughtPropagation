rm(list=ls())
library('data.table')
library('stringi')
ik=1
idxcalcstart <- function(data){
  tmp=data
  thres = -0.5
  data[tmp<(thres)]=1;data[tmp>=(thres)]=0
  runs=rle(data==1)
  myruns = which(runs$values == TRUE & runs$lengths > 2)
  runs.lengths.cumsum = cumsum(runs$lengths)
  ends = runs.lengths.cumsum[myruns]
  newindex = ifelse(myruns>1, myruns-1, 0)
  starts = runs.lengths.cumsum[newindex] + 1
  if (0 %in% newindex) starts = c(1,starts)
    idxsted = cbind(starts,ends)
  return(idxsted) 
}
compute_chars <- function(number,strtidx,endidx){
    x = data.matrix(speicalc[,number])
    severity=cbind(sum(x[strtidx:endidx,1]),endidx-strtidx+1)
    return(severity)
}


metrics = list()
speicalc<-fread('./spei.txt')
strtdf = list()
enddf = list()
speicalc=as.matrix(speicalc)
for (i in 1:dim(speicalc)[2]){
    statsed = idxcalcstart(speicalc[,i])
    strtidx= statsed[,1]
    endidx= statsed[,2]
    if (length(strtidx)>0){
      x = list()
    for (ik in 1:length(strtidx)){
        x[[ik]]=cbind(compute_chars(i,strtidx[ik],endidx[ik]),1)
    }
    metrics[[i]]=Reduce("+", x) 
    print(i)
    flush.console()
    
} else {
        metrics[[i]] = cbind(NA,NA,NA)
    }
    }
    
metrics_spei = do.call('rbind',metrics)
write.table(metrics_spei, "./metrics_spei.txt",row.names=FALSE,quote=FALSE)
