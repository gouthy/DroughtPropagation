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

speicalc<-fread('./spei.txt')
strtdf = list()
enddf = list()
speicalc=as.matrix(speicalc)
for (i in 1:dim(speicalc)[2]){
    statsed = idxcalcstart(speicalc[,i])
    strtdf[[i]]= statsed[,1]
    enddf[[i]]= statsed[,2]
}
library('pracma')

ecoindi<-function(timeseriesdata,droughtstartidx,droughtendidx,timelag=1){
  relnumerator = 0
  Recoverytime=0
  effect = 0 
  thres = -0.5
  if ((droughtendidx+timelag)>=length(timeseriesdata)){
    droughtendidx = length(timeseriesdata)
  } else {
    droughtendidx=droughtendidx+timelag
  }
  getnegidx=which(timeseriesdata[droughtstartidx:droughtendidx]<thres)
  xs=timeseriesdata[droughtstartidx:length(timeseriesdata)]
  newrelendidx = droughtendidx-droughtstartidx+1
    if (isempty(getnegidx)){
      Recoverytime=0
      effect = 0 
    } else {
      relnumerator=relnumerator+1
      runs=rle(xs<=(thres))
      myruns = which(runs$values == TRUE)
      runs.lengths.cumsum = cumsum(runs$lengths)
      ends = runs.lengths.cumsum[myruns]
      newindex = ifelse(myruns>1, myruns-1, 0)
      starts = runs.lengths.cumsum[newindex] + 1
      if (0 %in% newindex) starts = c(1,starts)
      eventsidx=which(starts<=newrelendidx)
      Recoverytimelist =list()
      effectlist = list()
      for (i in eventsidx){
        Recoverytimelist[[i]]=ends[i]-starts[i]+1 
        effectlist[[i]]=mean(xs[starts[i]:ends[i]],na.rm=TRUE)
      }
      Recoverytime= sum(unlist(Recoverytimelist))
      effect= mean(unlist(effectlist))
        
    }
    
  return(cbind(relnumerator,Recoverytime,effect))
  }

mean.test<-function(x,y){
z <- cbind(x,y)
len = length(x)
v = diff(colMeans(z))
if(len>4){
v.rep=replicate(500, {diff(colMeans(z[sample(nrow(z),size=ceiling(len/2)),]))})
pvalue = mean(sign(v)!=sign(v.rep))
} else {
pvalue = NA
}
return(pvalue)
}

rm(speicalc)
gc()
sricalcol<-fread('./sgi_ol.txt')
sricalcda<-fread('./sgi_da.txt')
sricalcol <- as.matrix(sricalcol)
sricalcda <- as.matrix(sricalcda)

metricsol = list()
metricsda = list()

for (ik in 1:length(strtdf)){
droughtstartidx = strtdf[[ik]]
droughtendidx = enddf[[ik]]
timeseriesdataol<-sricalcol[,ik]
timeseriesdatada<-sricalcda[,ik]

  if (length(droughtstartidx)>0){
ecometricsol = list()
ecometricsda = list()
for (i in 1:length(droughtstartidx)){
ecometricsol[[i]]=ecoindi(timeseriesdata=timeseriesdataol,droughtstartidx=droughtstartidx[i],droughtendidx=droughtendidx[i])
ecometricsda[[i]]=ecoindi(timeseriesdata=timeseriesdatada,droughtstartidx=droughtstartidx[i],droughtendidx=droughtendidx[i])
    }
ecometricsol = do.call(rbind,ecometricsol)
ecometricsda = do.call(rbind,ecometricsda)
p1 = mean.test(ecometricsol[,1], ecometricsda[,1])
p2 = mean.test(ecometricsol[,2], ecometricsda[,2])
p3 = mean.test(ecometricsol[,3], ecometricsda[,3])
p4 = mean.test(ecometricsol[,2]*ecometricsol[,3], ecometricsda[,2]*ecometricsda[,3])
      
yol = cbind(t(colSums(ecometricsol,na.rm=T)),p1,p2,p3,p4)
yda = cbind(t(colSums(ecometricsda,na.rm=T)),p1,p2,p3,p4)      
metricsol[[ik]]=yol
metricsda[[ik]]=yda
  } else {
      metricsol[[ik]] = cbind(NA,NA,NA,NA,NA,NA,NA)
      metricsda[[ik]] = cbind(NA,NA,NA,NA,NA,NA,NA)

  }
}
metrics_sri_ol = do.call('rbind',metricsol)
write.table(metrics_sri_ol, "./metrics_sgi_ol.txt",row.names=FALSE,quote=FALSE)

metrics_sri_da = do.call('rbind',metricsda)
write.table(metrics_sri_da, "./metrics_sgi_da.txt",row.names=FALSE,quote=FALSE)


sricalcol<-fread('./smi.txt')
sricalcda<-fread('./smi.txt')
sricalcol <- as.matrix(sricalcol)
sricalcda <- as.matrix(sricalcda)

metricsol = list()
metricsda = list()

for (ik in 1:length(strtdf)){
droughtstartidx = strtdf[[ik]]
droughtendidx = enddf[[ik]]
timeseriesdataol<-sricalcol[,ik]
timeseriesdatada<-sricalcda[,ik]

  if (length(droughtstartidx)>0){
ecometricsol = list()
ecometricsda = list()
for (i in 1:length(droughtstartidx)){
ecometricsol[[i]]=ecoindi(timeseriesdata=timeseriesdataol,droughtstartidx=droughtstartidx[i],droughtendidx=droughtendidx[i])
ecometricsda[[i]]=ecoindi(timeseriesdata=timeseriesdatada,droughtstartidx=droughtstartidx[i],droughtendidx=droughtendidx[i])
    }
ecometricsol = do.call(rbind,ecometricsol)
ecometricsda = do.call(rbind,ecometricsda)
p1 = mean.test(ecometricsol[,1], ecometricsda[,1])
p2 = mean.test(ecometricsol[,2], ecometricsda[,2])
p3 = mean.test(ecometricsol[,3], ecometricsda[,3])
p4 = mean.test(ecometricsol[,2]*ecometricsol[,3], ecometricsda[,2]*ecometricsda[,3])
      
yol = cbind(t(colSums(ecometricsol,na.rm=T)),p1,p2,p3,p4)
yda = cbind(t(colSums(ecometricsda,na.rm=T)),p1,p2,p3,p4)      
metricsol[[ik]]=yol
metricsda[[ik]]=yda
  } else {
      metricsol[[ik]] = cbind(NA,NA,NA,NA,NA,NA,NA)
      metricsda[[ik]] = cbind(NA,NA,NA,NA,NA,NA,NA)

  }
}
metrics_sri_ol = do.call('rbind',metricsol)
write.table(metrics_sri_ol, "./metrics_smi_ol.txt",row.names=FALSE,quote=FALSE)

metrics_sri_da = do.call('rbind',metricsda)
write.table(metrics_sri_da, "./metrics_smi_da.txt",row.names=FALSE,quote=FALSE)