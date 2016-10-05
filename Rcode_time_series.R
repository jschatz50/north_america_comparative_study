#####################
#### 	Data prep  ####
#####################
library(zoo)
library(ggplot2)
library(reshape)
library(lattice)
library(data.table)

df1 = read.csv("!monthly_covariates.csv",header=T)
df3 = melt(df1, id=c("CITY","YEAR","MONTH"))

cities=levels(factor(df3$CITY))
vars = c("TMAX", "TMIN", "TAVG", "PRESSURE", "PSUN", "Soil.moisture", "SST", "Clearsky.GHI",
	   "GHI", "Snow.Depth", "Delta.SST.TMAX", "Delta.SST.TMIN", "delta.NDVI", "WIND.SP.DAY",
	   "WIND.DIR.DAY", "RH.DAY", "WIND.SP.NIGHT","WIND.DIR.NIGHT", "RH.NIGHT")






#####################################
######## (1) ARIMA models  ##########
#####################################
library(car)
library(lmtest)
library(nlme)
library(tseries)
library(forecast)
library(Metrics)

i=37
cities=levels(factor(df3$CITY))
city=cities[i]; city
data1=subset(df3,CITY==city)
data1=dcast(data1, CITY+MONTH+YEAR~variable, value.var="value")
data1 <- with(data1,data1[order(CITY, YEAR, MONTH),])
data1 = data1[!is.na(data1$UHImin),]

training = data1[1:floor(nrow(data1)*0.7),]
test = data1[ceiling(nrow(data1)*0.7):nrow(data1),]

## Month dummy variables
x=data.frame(data1$MONTH); colnames(x)="MONTH"
x$S1=ifelse(x$MONTH==1,1,0)
x$S2=ifelse(x$MONTH==2,1,0)
x$S3=ifelse(x$MONTH==3,1,0)
x$S4=ifelse(x$MONTH==4,1,0)
x$S5=ifelse(x$MONTH==5,1,0)
x$S6=ifelse(x$MONTH==6,1,0)
x$S7=ifelse(x$MONTH==7,1,0)
x$S8=ifelse(x$MONTH==8,1,0)
x$S9=ifelse(x$MONTH==9,1,0)
x$S10=ifelse(x$MONTH==10,1,0)
x$S11=ifelse(x$MONTH==11,1,0)
x$MONTH=NULL
x=as.matrix(x)

xreg=data1[,c("WIND","PSUN","delta.NDVI","Snow.Depth","Soil.moisture")]
y=ts(data1$UHImin,frequency=12)
y.train=ts(training$UHImin,frequency=12)
xreg.train=training[,c("WIND","PSUN","delta.NDVI","Snow.Depth","Soil.moisture")]
y.test = ts(test$UHImin,frequency=12)
xreg.test=test[,c("WIND","PSUN","delta.NDVI","Snow.Depth","Soil.moisture")]
#xreg=cbind(xreg,x)

arima1 = auto.arima(y.train,xreg=xreg.train,stepwise=FALSE,approximation=FALSE,max.D=2,ic="bic"); summary(arima1)
arima2 = arima(y.train, xreg = xreg.train, order=c(2,1,0)); summary(arima2)
tsdiag(arima1, gof.lag = 12)
Box.test(residuals(arima1),lag=20,type="Ljung-Box")
adf.test(residuals(arima1),alternative="stationary")
kpss.test(residuals(arima1))
tsdisplay(residuals(arima1))

sa=xreg; sa$WIND=0
forecast1=forecast(arima1,h=nrow(data1),xreg=xreg)
forecast2=forecast(arima1,h=nrow(data1),xreg=sa)
accuracy(as.vector(forecast1$mean),as.vector(y))
plot(as.vector(forecast1$mean),type='l',ylim=range(y,as.vector(forecast1$mean)))
lines(as.vector(y),col='red')
cor(as.vector(forecast1$mean),as.vector(y))

## plot forecast 1 (original covariates)
ylim <- range(forecast1$mean, forecast1$upper, forecast1$lower, y)
plot(as.vector(forecast1$mean), col = "red",type='l',ylim=ylim)
#lines(forecast1$upper[,2], col = "black", lty = 2,ylim=ylim)
#lines(forecast1$lower[,2], col = "black", lty = 2,ylim=ylim)
legend("topleft", legend = c("Observed", "Predicted", "95% CI"), col = c("black", "red", "red"), lty = c(1, 1, 2), bty = "n")
lines(as.vector(y))

## plot forecast 2 (wind/sun constant)
ylim <- range(forecast2$mean, forecast2$upper, forecast2$lower, y)
plot(as.vector(forecast2$mean), col = "red",type='l',ylim=ylim)
#lines(forecast2$upper[,2], col = "black", lty = 2,ylim=ylim)
#lines(forecast2$lower[,2], col = "black", lty = 2,ylim=ylim)
legend("topleft", legend = c("Observed", "Predicted", "95% CI"), col = c("black", "red", "red"), lty = c(1, 1, 2), bty = "n")
lines(as.vector(y))

tsdisplay(residuals(arima1))
tsdisplay(fitted.values(arima1))
predicted=unclass(fitted.values(arima1))
predicted=exp(unclass(fitted.values(forecast1)))
actual=unclass(y)
plot(predicted,actual)
rmse(predicted,actual)
lm1=lm(actual~predicted); summary(lm1)


######################################
######## (2) linear models  ##########
######################################
lm1 <- lm(UHImin~PSUN + WIND + Delta.NDVI + Soil.moisture + Snow.Depth, data=data1)	#OLS model
summary(lm1)
acf(residuals(lm1))	#plot autocorrelation
acf(residuals(lm1), type="partial")	#partial autocorrelation
durbinWatsonTest(lm1, max.lag=7)	#durban-watson test
dwtest(lm1, alternative="two.sided")	#alternative durban-watson


diff1  = data.frame(diff(as.matrix(data1[,4:29]),differences=1))
lm1 <- lm(UHImin~PSUN + WIND + Delta.NDVI + Soil.moisture + Delta.SST.TMIN, data=diff1)	#OLS model
summary(lm1)
acf(residuals(lm1))	#plot autocorrelation
acf(residuals(lm1), type="partial")	#partial autocorrelation
durbinWatsonTest(lm1, max.lag=7)	#durban-watson test
dwtest(lm1, alternative="two.sided")	#alternative durban-watson
vif(lm1)

###################################
######## (3) GLS models  ##########
###################################


diff1  = data.frame(diff(as.matrix(data1[,4:29]),differences=1))
gls1 <- gls(UHImin~PSUN + WIND + Soil.moisture + delta.NDVI + Snow.Depth, data=data1, correlation=corARMA(p=0,q=1), method="ML")
summary(gls1)
tsdisplay(residuals(gls1))
vif(gls1)

y2 = predict(gls1,xreg)
plot(data1$UHImin,ylim=range(data1$UHImin,y2),type='l')
lines(y2,col='red')
plot(y,y2)


gls1 <- gls(UHImin~PSUN + WIND + Soil.moisture + delta.NDVI + Snow.Depth, data=data1, corr=corCAR1(form=~1), method="ML",na.action=na.omit)
gls1 <- gls(UHImin~PSUN + WIND + Soil.moisture + delta.NDVI , data=data1, corr=corCAR1(form=~1), method="ML",na.action=na.omit)
gls1 <- gls(UHImin~PSUN + WIND + Soil.moisture + Snow.Depth, data=data1, corr=corCAR1(form=~1), method="ML",na.action=na.omit)
gls1 <- gls(UHImin~PSUN + WIND + Soil.moisture, data=data1, corr=corCAR1(form=~1), method="ML",na.action=na.omit)
gls1 <- gls(UHImin~PSUN + WIND + Soil.moisture + delta.NDVI + Snow.Depth, data=data1, corr=corCAR1(form=~1), method="ML",na.action=na.omit)
gls1 <- gls(UHImin~PSUN + WIND + Soil.moisture + delta.NDVI + Snow.Depth, data=data1, corr=corCAR1(form=~1), method="ML",na.action=na.omit)


tsdisplay(residuals(gls2))


summary(gls1)
vif(gls1)
tsdisplay(residuals(gls1))
Box.test(gls1$residuals,lag=12,type="Ljung-Box")
adf.test(gls1$residuals,alternative="stationary")
kpss.test(gls1$residuals)
gls2 <- update(gls2, correlation=NULL)
#gls2 <- update(gls1, correlation=corARMA(p=3,q=1))
anova(gls1,gls2)


##############################
######## (4) TSLMs  ##########
##############################
tslm1  <- tslm(y ~ data1$PSUN+data1$WIND+data1$Delta.NDVI+data1$Soil.moisture+data1$Delta.SST.TMIN)
tslm1  <- tslm(y ~ trend + season)

f <- forecast(tslm1, h=5,level=c(80,95),newdata=xreg)

summary(lm1)
tsdisplay(residuals(lm1))
fcast <- forecast(lm1, sa)
#plot(fcast)
ndiffs(y,test=c("kpss"))
nsdiffs(y)


############################
###### (5) lme models ######
############################
library(lme4)
summary(lm(UHImin~PSUN+WIND+delta.NDVI, data=data1))
summary(lm(UHImax~PSUN+WIND+delta.NDVI, data=data1))

m1 <- lmer(UHImin~PSUN+WIND+delta.NDVI+Snow.Depth + (1 | CITY), data=df4)	#varying intercept
m1 <- lmer(UHImin~PSUN+WIND+delta.NDVI+Snow.Depth + (PSUN+WIND+delta.NDVI+Snow.Depth+1 | CITY), data=df4)	#varying intercept and slope
coef(m1)

acf(residuals(m1))	#plot autocorrelation
acf(residuals(m1), type="partial")	#partial autocorrelation
durbinWatsonTest(m1, max.lag=7)	#durban-watson test
dwtest(m1, alternative="two.sided")	#alternative durban-watson


#############################
###### (6) INLA models ######
#############################
vars = c("WIND", "PRESSURE", "PSUN", "Soil.moisture", "delta.NDVI", "Clearsky.GHI", "GHI", "Snow.Depth", "Delta.SST.TMAX", "Delta.SST.TMIN") 

libarary(INLA)
data1$time = seq(1,nrow(data1),1)

data2=data1
data2$UHImin[(nrow(data2)-11):nrow(data2)] = NA


## linear regression equivalent
formula = UHImin~1+PSUN+WIND
model.linear = inla(formula,family="gaussian",data=data2)
round(model.linear$summary.fixed[,1:5],3)
model.linear$marginals.fixed

var1 = get(vars[3])

## autocorrelated errors
formula = UHImin ~ 1 + WIND + PSUN + Soil.moisture + Delta.SST.TMIN + f(time, model="ar1")
r = inla(formula, data=data2, family = "gaussian", control.compute=list(dic=TRUE, waic=TRUE), control.predictor = list(compute = TRUE))
summary(r)
fitted = r$summary.fitted[, "mean"]
actual = data1$UHImin
plot(fitted,type='l',ylim=range(fitted,actual))
lines(actual,col='red')
rmse(fitted[1:(length(fitted)-12)] ,actual[1:(length(fitted)-12)])
rmse(fitted[(length(fitted)-11):length(fitted)] ,actual[(length(fitted)-11):length(fitted)])
cor(fitted[(length(fitted)-11):length(fitted)] ,actual[(length(fitted)-11):length(fitted)])


formula = UHImin ~ 1 + WIND + PSUN + Soil.moisture + Delta.SST.TMIN + f(time, model="ar",order=2)
r = inla(formula, data=data2, family = "gaussian", control.compute=list(dic=TRUE, waic=TRUE), control.predictor = list(compute = TRUE))
sum1 = summary(r)
sum1$dic$dic
#h = inla.hyperpar(r)
fitted = r$summary.fitted[, "mean"]
actual = data1$UHImin
plot(fitted,type='l',ylim=range(fitted,actual))
lines(actual,col='red')
rmse(fitted[1:(length(fitted)-12)] ,actual[1:(length(fitted)-12)])
rmse(fitted[(length(fitted)-11):length(fitted)] ,actual[(length(fitted)-11):length(fitted)])
cor(fitted[(length(fitted)-11):length(fitted)] ,actual[(length(fitted)-11):length(fitted)])



For all models we use these priors: ß~N(0,s2ß=2.2·104), s2u~InvGamma(0.5,0.5) and (when needed) s2e~InvGamma(0.5,0.5). To choose the best model, we start with the full model and remove one variable at the time in a stepwise manner. In each step all nested models are examined, but only the one with lowest DIC (i.e., the best one at each step) is reported in Table 1.



fitted = r$summary.fitted[, "mean"]
actual = data1$UHImin
plot(fitted,type='l')
lines(actual,col='red')
rmse(fitted,actual)





###############################
###### (7) random forest ######
###############################
library(randomForest)
library(rpart.plot)
library(vita)

df4=dcast(df3, CITY+MONTH+YEAR~variable, value.var="value")
df4 <- with(df4,df4[order(CITY, YEAR, MONTH),])
df4 = df4[!is.na(df4$UHImin),]

df.inland = df4[is.na(df4$SST),]
df.shore = df4[!is.na(df4$SST),]
cities.inland = levels(factor(df.inland$CITY))
cities.shore = levels(factor(df.shore$CITY))

results = data.frame(matrix(ncol = length(cities.inland), nrow = 16))
results = data.frame(matrix(ncol = length(cities.inland), nrow = 8))
results2 = data.frame(matrix(ncol = length(cities.inland), nrow = 8))

for (i in 1:length(cities.shore)){
   tryCatch({
	city=cities.shore[i]; city
	data1=subset(df.shore, CITY==city)

	data1$WIND.DIR.NIGHT = with(data1, ifelse(abs(WIND.DIR.NIGHT-median(WIND.DIR.NIGHT,na.rm=T))>180, 
			ifelse(median(WIND.DIR.NIGHT,na.rm=T)>WIND.DIR.NIGHT, WIND.DIR.NIGHT+360, WIND.DIR.NIGHT-360), WIND.DIR.NIGHT))

	training = data1[1:ceiling(nrow(data1)-(nrow(data1)*.3)),]
	test = data1[(ceiling(nrow(data1)-(nrow(data1)*.3))+1):nrow(data1) ,]
	rf=randomForest(UHImin~WIND.SP.NIGHT + PSUN + Soil.moisture + RH.NIGHT + GHI + delta.NDVI + WIND.DIR.NIGHT + Delta.SST.TMIN, data=training, importance=TRUE, ntree = 1000); rf

#	gbm1 = gbm(UHImin~WIND.SP.NIGHT + PSUN + Soil.moisture + RH.NIGHT + GHI + delta.NDVI + WIND.DIR.NIGHT + Delta.SST.TMIN, 
#			   data=training, distribution="gaussian", n.trees=2000, interaction.depth=4, shrinkage = 0.001, bag.fraction=0.8, cv.folds=3)
#	best.iter <- gbm.perf(gbm1,method="cv")
#	print(best.iter)
#	summary(gbm1,n.trees=best.iter)
#	summary(gbm1)
#	plot(gbm1,c(3,5),best.iter)
#	yhat.boost=predict(gbm1, newdata=test,n.trees=2000)
#	sqrt(mean(yhat.boost-test$UHImin)^2)

	Xs = training[,c("WIND.SP.NIGHT","PSUN","Soil.moisture","RH.NIGHT","GHI","delta.NDVI","WIND.DIR.NIGHT", "Delta.SST.TMIN")]
	Ys = training[,c("UHImin")]

	pimp.varImp.reg<-PIMP(Xs,Ys,rf,S=100, ncores=4)
	pimp.t.reg = PimpTest(pimp.varImp.reg)

#	summary(pimp.t.reg,pless = 0.05)
#	importance1 = data.frame(round(importance(rf), 2)[,1]); colnames(importance1)="values"
	predicted = predict(rf, test)
	observed = test$UHImin
	plot(observed,type='l',ylim=range(predicted,observed),xaxt='n')
		axis(1, at=1:nrow(test), labels=test$MONTH)
	lines(predicted,col='red')
	rmse1 = sqrt(mean(predicted-observed)^2); rmse1
	correlation1 = cor(predicted,observed); correlation1
#	hist(treesize(rf))
#	plot(rf)
#	varImpPlot(rf)
#	partialPlot(rf, training, PRESSURE)
#	a=rfcv(training[,c("WIND","PSUN","Soil.moisture","GHI","delta.NDVI","Delta.SST.TMIN")], training$UHImin, cv.fold=10, scale="log", step=0.9)
#	cv.errors = data.frame(as.vector(a$error.cv)); colnames(cv.errors)="values"

	results[,i] = as.vector(unlist(pimp.t.reg$pval))
	results2[,i] = as.vector(unlist(pimp.t.reg$VarImp))
   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
colnames(results) = cities.shore
row.names(results) = row.names(pimp.t.reg$pval)
colnames(results2) = cities.shore
row.names(results2) = row.names(pimp.t.reg$VarImp)
write.csv(results,"shore_cities_regression_tree_results_PIMP_pval.csv")
write.csv(results2,"shore_cities_regression_tree_results_PIMP_VarImp.csv")

########################
### pruned RF models ###
########################
### get variables ###
pvals = read.csv("pvals1.csv", header=T)
cities.pvals = levels(factor(pvals$CITY))

df1 <- data.frame(matrix(ncol = 1, nrow = nrow(pvals)))
for (i in 1:nrow(pvals)){
	df1[i,] = paste(as.vector(as.character(colnames(pvals)[which(pvals[i,]<0.1)])),collapse="+")
}
df1$CITY = cities.pvals

### fit pruned models ###
list1=list()
list2=list()
for (i in 1:length(cities.pvals)){
   tryCatch({
	city=cities.pvals[i]; city
	data1=subset(df4, CITY==city)

	data1$WIND.DIR.NIGHT = with(data1, ifelse(abs(WIND.DIR.NIGHT-median(WIND.DIR.NIGHT,na.rm=T))>180, 
			ifelse(median(WIND.DIR.NIGHT,na.rm=T)>WIND.DIR.NIGHT, WIND.DIR.NIGHT+360, WIND.DIR.NIGHT-360), WIND.DIR.NIGHT))

	training = data1[1:ceiling(nrow(data1)-(nrow(data1)*.3)),]
	test = data1[(ceiling(nrow(data1)-(nrow(data1)*.3))+1):nrow(data1) ,]
	formula1 = as.formula(paste("UHImin ~ ", df1[i,1], sep = ""))
	rf=randomForest(formula1, data=training, importance=TRUE, ntree = 1000); rf
	predicted = predict(rf, test)
	observed = test$UHImin

	rmse.training = sqrt(mean(rf$mse))
	corr.training = mean(rf$rsq)	
	rmse.test = sqrt(mean(predicted-observed)^2)
	corr.test = cor(predicted,observed)^2
	mean1 = mean(training$UHImin)
	sd1 = sd(training$UHImin)
	ntrain = nrow(training)
	ntest = nrow(test)
	results1 = cbind(rmse.training,corr.training,rmse.test,corr.test,mean1,sd1,ntrain,ntest)
	results1$CITY = city
	results1$variables = paste(varlist,collapse=",")
	list1[[i]]=results1

	varlist = as.vector(unlist(strsplit(df1[i,1],"+",fixed=T)))
	list3=list()
	for (j in 1:length(varlist)){
		pp=partialPlot(rf, training, varlist[j])
		lm1 = lm(pp$y~pp$x)
		list3[[j]] = ifelse(lm1$coefficients[2] > 0, "+", "-")
	}
	relationships = data.frame(cbind(varlist,list3))
	relationships$CITY = city
	relationships = data.frame(t(data.frame(matrix(unlist(relationships), nrow=length(relationships), byrow=T))))
	list2[[i]] = relationships
   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

final.results = rbindlist(list1)
	colnames(final.results) = c("rmse.training","corr.training","rmse.test","corr.test","mean","sd","ntrain","ntest","CITY","VARS")
final.relationships = rbindlist(list2)
final.relationships$X2 = as.character(final.relationships$X2)
dd=dcast(final.relationships, X3 ~ X1, value.var="X2", df=T, fill=NA, add.missing=T)

write.csv(final.results, "pvals_results_pruned_models.csv", row.names=F)
#write.csv(dd, "pvals_relationships_pruned_models.csv", row.names=F)



###################################
#### variability of covariates ####
###################################
monthly1 = read.csv("!monthly_covariates.csv",header=T)
monthly1 = melt(monthly1, id=c("CITY","YEAR","MONTH"))

cities=levels(factor(monthly1$CITY))
vars = c("UHImax","UHImin","TMAX", "TMIN", "TAVG", "PRESSURE", "PSUN", "Soil.moisture", "SST", "Clearsky.GHI",
	   "GHI", "Snow.Depth", "Delta.SST.TMAX", "Delta.SST.TMIN", "delta.NDVI", "WIND.SP.DAY",
	   "WIND.DIR.DAY", "RH.DAY", "WIND.SP.NIGHT","WIND.DIR.NIGHT", "RH.NIGHT")

standardize <- function(x){
	stdevs <- mad(x,na.rm=T)
	scaled <- x/stdevs
	centered <- scaled - mean(scaled,na.rm=T)
	return(centered) }

list1=list()
for (i in 1:length(vars)){
	temp1=subset(monthly1, variable == vars[i])
	temp1$value = standardize(temp1$value)
	list1[[i]] = temp1
}
final1=rbindlist(list1)
x.max <- aggregate(value ~ CITY+YEAR+variable, final1, max)
x.min <- aggregate(value ~ CITY+YEAR+variable, final1, min)
x.merge = merge(x.max,x.min,by=c("CITY","YEAR","variable"))
x.merge$delta = x.merge$value.x - x.merge$value.y
x.merge = aggregate(delta ~ CITY+variable, data=x.merge, FUN='mean')

importances = read.csv(file.choose(),header=T)
importances = melt(importances, id="CITY")
rf.data = merge(importances,x.merge, all.x=T)
colnames(rf.data) = c("CITY","variable","importance","delta")
rf.data=rf.data[complete.cases(rf.data),]

vars = levels(factor(rf.data$variable))
cities = levels(factor(rf.data$CITY))
for (i in 1:length(vars)){
	i=4
	temp1 = subset(rf.data, variable==vars[i]); vars[i]
	rf=randomForest(importance~delta, data=temp1, importance=TRUE, ntree = 1000); rf
	summary(lm(importance~delta,temp1))




#####################
#### 	Data prep  ####
#####################
library(zoo)
library(ggplot2)
library(reshape)
library(lattice)
library(data.table)

df1 = read.csv("!monthly_covariates.csv",header=T)
df3 = melt(df1, id=c("CITY","YEAR","MONTH"))

cities=levels(factor(df3$CITY))
vars = c("TMAX", "TMIN", "TAVG", "PRESSURE", "PSUN", "Soil.moisture", "SST", "Clearsky.GHI",
	   "GHI", "Snow.Depth", "Delta.SST.TMAX", "Delta.SST.TMIN", "delta.NDVI", "WIND.SP.DAY",
	   "WIND.DIR.DAY", "RH.DAY", "WIND.SP.NIGHT","WIND.DIR.NIGHT", "RH.NIGHT")








########################################
#######  Sunny, clear days only  #######
########################################
#data2 <- subset(data1, !(is.na(UHImin)))
#summary(lm(diff(UHImin)~diff(WIND)+diff(PSUN),data2))

data1=subset(df2,PSUN>=.9 & WIND<=3)
cities=levels(factor(data1$CITY))

for (i in 1:length(cities)){
city=cities[i]
data2=subset(data1,CITY==city)

tiffname=paste(city,".tiff",sep="")
tiff(tiffname,height=800,width=1600,res=150)
par(mfrow=c(1,2))
par(mar=c(2,2,2,2))
boxplot(UHImin~MONTH,data2,outline=F,main="Tmin")
abline(h=mean(data2$UHImin,na.rm=T))
boxplot(UHImax~MONTH,data2,outline=F,main="Tmax")
abline(h=mean(data2$UHImax,na.rm=T))
city
dev.off()
}

a=aggregate(cbind(UHImax,UHImin)~CITY+MONTH,data=data1,FUN=mean)
b=aggregate(cbind(UHImax,UHImin)~CITY+MONTH,data=data1,FUN=length)
c=cbind(a,b)
write.csv(c,"monthly_means_counts_clear_calm.csv",row.names=F)
