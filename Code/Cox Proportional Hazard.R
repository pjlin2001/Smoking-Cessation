data <- read.table(file='C:\\Users\\Phili\\Downloads\\smoking.txt')
head(data)
#subset to first event data
#Sort the data by time within each id
o <- order(data$id,data$time)
data <- data[o,]
#get the first row for each id
data.CE <- data[!duplicated(data$id),]

#set status=1 if status==2 or 1
data.CE$status <- (data.CE$status>0)+0
#fit the Cox proportional hazards model
obj <- coxph(Surv(time,status)~factor(trt)+age+factor(sex)+factor(race)
             +factor(empl)+years+factor(level)+pa+nosmk,data=data.CE)
#summarize results
summary(obj)
## Use relationship between cox-snell and martingal
## residuals
coxsnellres <- data.CE$status-resid(obj,type="martingale")
## Then use N-A method to estimate the cumulative 
## hazard function for residuals;
fit <- survfit(Surv(coxsnellres,data.CE$status)~1)
Htilde <- cumsum(fit$n.event/fit$n.risk)
plot(log(fit$time),log(Htilde),cex.axis=1.5,cex.lab=1.5,lwd=1.5,
     main="Cox-Snell residual plot",xlab="log t", cex.main=1.5,
     ylab="log cumulative hazard")
abline(0,1,lty=2,lwd=1.5)


#Schoelfeld residuals
#produce proportionality test results
sch <- cox.zph(obj) 
print(sch) 
#Plot scaled Schoelfeld residuals for each covariate
par(par(mar = c(4, 4, 2, 2)), mfrow=c(5,2))
plot(sch,xlab="Time",lwd=2,cex.lab=1.2,cex.axis=1.2)

## Martingale residuals
mart_resid <- resid(obj,type='martingale')

## Age
par(mfrow=c(2,2))
plot(data.CE$age, mart_resid,
     xlab="Age (years)", ylab="Martingale residuals",
     main='Age',cex.lab=1.2,cex.axis=1.2)
lines(lowess(data.CE$age, mart_resid),lwd=2)
abline(0,0,lty=3,lwd=2)

## Years
plot(data.CE$years, mart_resid,
     xlab="Years", ylab="Martingale residuals",
     main='Years',cex.lab=1.2,cex.axis=1.2)
lines(lowess(data.CE$years, mart_resid),lwd=2)
abline(0,0,lty=3,lwd=2)

## Prior Attempts
plot(data.CE$pa, mart_resid,
     xlab="Prior Attempts", ylab="Martingale Residuals",
     main='Prior Attempts',cex.lab=1.2,cex.axis=1.2)
lines(lowess(data.CE$pa, mart_resid),lwd=2)
abline(0,0,lty=3,lwd=2)


## Not Smoking
plot(data.CE$nosmk, mart_resid,
     xlab="Not smoking (days)", ylab="Martingale Residuals",
     main='Not smoking',cex.lab=1.2,cex.axis=1.2)
lines(lowess(data.CE$nosmk, mart_resid),lwd=2)
abline(0,0,lty=3,lwd=2)

data.CE1 <- with(data.CE, data.frame(time = time, status = status, trt = trt, age = age, sex = sex, race = race, empl = empl, years = years, level = level, pa = ifelse(pa<100,0,1), nosmk=nosmk))
obj1 <- coxph(Surv(time,status)~factor(trt)+age+factor(sex)+factor(race)
             +factor(empl)+years+factor(level)+pa+nosmk,data=data.CE1)
## Use relationship between cox-snell and martingal
## residuals
coxsnellres <- data.CE$status-resid(obj1,type="martingale")
## Then use N-A method to estimate the cumulative 
## hazard function for residuals;
fit <- survfit(Surv(coxsnellres,data.CE1$status)~1)
Htilde <- cumsum(fit$n.event/fit$n.risk)
plot(log(fit$time),log(Htilde),cex.axis=1.5,cex.lab=1.5,lwd=1.5,
     main="Cox-Snell residual plot",xlab="log t", cex.main=1.5,
     ylab="log cumulative hazard")
abline(0,1,lty=2,lwd=1.5)


#Schoelfeld residuals
#produce proportionality test results
sch <- cox.zph(obj1) 
print(sch) 
#Plot scaled Schoelfeld residuals for each covariate
par(par(mar = c(4, 4, 2, 2)), mfrow=c(5,2))
plot(sch,xlab="Time",lwd=2,cex.lab=1.2,cex.axis=1.2)

## Martingale residuals
mart_resid <- resid(obj1,type='martingale')

## Age
par(mfrow=c(2,2))
plot(data.CE1$age, mart_resid,
     xlab="Age (years)", ylab="Martingale residuals",
     main='Age',cex.lab=1.2,cex.axis=1.2)
lines(lowess(data.CE1$age, mart_resid),lwd=2)
abline(0,0,lty=3,lwd=2)

## Years
plot(data.CE1$years, mart_resid,
     xlab="Years", ylab="Martingale residuals",
     main='Years',cex.lab=1.2,cex.axis=1.2)
lines(lowess(data.CE1$years, mart_resid),lwd=2)
abline(0,0,lty=3,lwd=2)

## Prior Attempts
plot(data.CE1$pa, mart_resid,
     xlab="Prior Attempts", ylab="Martingale Residuals",
     main='Prior Attempts',cex.lab=1.2,cex.axis=1.2)
lines(lowess(data.CE1$pa, mart_resid),lwd=2)
abline(0,0,lty=3,lwd=2)


## Not Smoking
plot(data.CE1$nosmk, mart_resid,
     xlab="Not smoking (days)", ylab="Martingale Residuals",
     main='Not smoking',cex.lab=1.2,cex.axis=1.2)
lines(lowess(data.CE1$nosmk, mart_resid),lwd=2)
abline(0,0,lty=3,lwd=2)

z1 <- with(data.CE1, data.frame(trt = c("patchOnly", "combination"),
                                age = rep(median(age, na.rm = TRUE), 2),
                                sex = c("Female", "Female"),
                                race = c("black", "black"),
                                empl = c("ft", "ft"),
                                years = rep(median(years, na.rm = TRUE), 2),
                                level = c("heavy", "heavy"),
                                pa = c(0,0),
                                nosmk = rep(median(nosmk, na.rm = TRUE), 2)))
#plot the Survival Curve function
plot(survfit(obj1, newdata = z1), xlab="Time",ylab="Survival Curve With Variable Treatment Therapy", 
     col = c("red", "blue"))
