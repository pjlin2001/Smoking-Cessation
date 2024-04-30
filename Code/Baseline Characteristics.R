data <- read.table(file='C:\\Users\\Phili\\Downloads\\smoking.txt')
head(data)
Mean.IQR.by.trt=function(y,trt,decp=1){
  groups=sort(unique(trt))
  all=quantile(y)
  g1=quantile(y[trt==groups[1]])
  g2=quantile(y[trt==groups[2]])
  
  result=matrix(NA,1,3)
  colnames(result)=c(groups,"Overall")
  result[1,1]=paste0(round(g1[3],decp)," (",round(g1[2],decp),", ",round(g1[4],decp),")")
  result[1,2]=paste0(round(g2[3],decp)," (",round(g2[2],decp),", ",round(g2[4],decp),")")
  result[1,3]=paste0(round(all[3],decp)," (",round(all[2],decp),", ",round(all[4],decp),")")
  return(result)
}

N.prct.by.trt=function(x,trt,decp=1){
  groups=sort(unique(trt))
  x.levels=sort(unique(x))
  p=length(x.levels)
  n=length(x)
  n1=length(x[trt==groups[1]])
  n2=length(x[trt==groups[2]])
  
  result=matrix(NA,p,3)
  colnames(result)=c(groups,"Overall")
  rownames(result)=x.levels
  
  for (i in 1:p){
    n1i=sum(x[trt==groups[1]]==x.levels[i])
    n2i=sum(x[trt==groups[2]]==x.levels[i])
    ni=sum(x==x.levels[i])
    
    
    result[i,1]=paste0(n1i," (",round(n1i/n1*100,decp),"%)")
    result[i,2]=paste0(n2i," (",round(n2i/n2*100,decp),"%)")
    result[i,3]=paste0(ni," (",round(ni/n*100,decp),"%)")
  }
  
  
  return(result)
}

o <- order(data$id,data$time)
dat <- data[!duplicated(data$id),]
n <- nrow(dat)
table1 <- rbind(
  Mean.IQR.by.trt(y=dat$age,trt=dat$trt),
  N.prct.by.trt(x=dat$sex,trt=dat$trt),
  N.prct.by.trt(x=dat$race,trt=dat$trt),
  N.prct.by.trt(x=dat$empl,trt=dat$trt),
  Mean.IQR.by.trt(y=dat$years,trt=dat$trt),
  N.prct.by.trt(x=dat$level,trt=dat$trt),
  Mean.IQR.by.trt(y=dat$pa,trt=dat$trt),
  Mean.IQR.by.trt(y=dat$nosmk,trt=dat$trt)
)

noquote(table1)
# Numerator: total # of events
num.FE <- c(sum(dat$status[dat$trt=="combination"]), 
            sum(dat$status[dat$trt=="patchOnly"]), 
            sum(dat$status))

#Demoninator: total length of follow-up
denom.FE <- c(sum(dat$time[dat$trt=="combination"]), 
              sum(dat$time[dat$trt=="patchOnly"]), 
              sum(dat$time))

#death rate
round(num.FE/denom.FE,3)
      
