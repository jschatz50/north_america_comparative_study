### monthly summaries ###
df = read.csv("!daily_UHI_time_series.csv",head=T)
df2 = read.csv("!merged_meteorology_2001-2014.csv",header=T)
df2$DATE2 = with(df2,paste(str_sub(DATE,1,4),"-",str_sub(DATE,5,6),"-",str_sub(DATE,7,8),sep=""))
df2$DATE=NULL
colnames(df2)[ncol(df2)]="DATE"

df1 = subset(df,Set=="Ten-forty")
df1 = merge(df1,df2,by=c("DATE","CITY"))
df1$Delta.SST.TMAX = with(df1, SST-TMAX)
df1$Delta.SST.TMIN = with(df1, SST-TMIN)

df1$Year=NULL
df1$Month=NULL
df1$Set=NULL
df1$RCOUNTS=NULL
df1$UCOUNTS=NULL

df2 = melt(df1, id.vars = c("DATE","CITY","YEAR","MONTH"))
res <- aggregate(value~variable+CITY+YEAR+MONTH, df2, FUN=mean, na.action=na.omit)

res = dcast(res, CITY+MONTH+YEAR~variable, value.var="value")
res <- with(res,res[order(CITY, YEAR, MONTH),])
write.csv(res,"monthly_averages.csv",row.names=F)

#####################################
#### 	    Standardize data       ####
#####################################
library(zoo)
library(ggplot2)
library(reshape)
library(lattice)
library(lubridate)
library(gridExtra)
library(reshape2)
library(stringr)
library(stlplus)

df = read.csv("!daily_UHI_time_series.csv",head=T)
df2 = read.csv("!merged_meteorology_2001-2014.csv",header=T)
df2$DATE2 = with(df2,paste(str_sub(DATE,1,4),"-",str_sub(DATE,5,6),"-",str_sub(DATE,7,8),sep=""))
df2$DATE=NULL
colnames(df2)[ncol(df2)]="DATE"

df1 = subset(df,Set=="Twenty-fifty")
cities=levels(factor(df1$CITY))
df1 = merge(df1,df2,by=c("DATE","CITY"))
df1$Delta.SST.TMAX = with(df1, SST-TMAX)
df1$Delta.SST.TMIN = with(df1, SST-TMIN)

cities=levels(factor(df$CITY))
vars = c("UHImax","UHImin","TMAX","TMIN","WIND","PSUN","Soil.moisture","Avg.60km.NDVI","GHI","Delta.NDVI","GHI","Delta.SST.TMAX","Delta.SST.TMIN")

list1=list()

### standardize data ###
standardize <- function(x){
	stdevs <- mad(x,na.rm=T)
	scaled <- x/stdevs
	centered <- scaled - mean(scaled,na.rm=T)
	return(centered) }

for (i in 1:length(vars)){
	df1[vars[i]] = standardize(as.vector(df1[vars[i]][,1]))
}
write.csv(df1,"standardized_UHI_weather_data_20-50.csv",row.names=F)


#####################################
#### 	Time series decomposition  ####
#####################################
library(zoo)
library(ggplot2)
library(reshape)
library(lattice)
library(lubridate)
library(gridExtra)
library(reshape2)
library(stringr)
library(stlplus)

df = read.csv("!daily_UHI_time_series.csv",head=T)
df2 = read.csv("!merged_meteorology_2001-2014.csv",header=T)
df2$DATE2 = with(df2,paste(str_sub(DATE,1,4),"-",str_sub(DATE,5,6),"-",str_sub(DATE,7,8),sep=""))
df2$DATE=NULL
colnames(df2)[ncol(df2)]="DATE"

df1 = subset(df,Set=="Twenty-fifty")
cities=levels(factor(df1$CITY))
df1 = merge(df1,df2,by=c("DATE","CITY"))
df1$Delta.SST.TMAX = with(df1, SST-TMAX)
df1$Delta.SST.TMIN = with(df1, SST-TMIN)

cities=levels(factor(df$CITY))
vars = c("UHImax","UHImin","TMAX","TMIN","WIND","PSUN","Soil.moisture","Avg.60km.NDVI","GHI","Delta.NDVI","GHI","Delta.SST.TMAX","Delta.SST.TMIN")

list1=list()

for (i in 1:length(cities)){
   tryCatch({
	city=cities[i]; city
	data1=subset(df1,CITY==city)
	data1$DATE=as.Date(as.character(data1$DATE), "%Y-%m-%d")
	list2=list()

	for (j in 1:length(vars)){
	   tryCatch({
		data2=data1[vars[j]]
		data2=data2[which(!is.na(data1$UHImin)),]
		x=ts(data2,frequency=365)
		fit = stlplus(x, s.window="periodic", n.p = 365, na.action = na.approx)
		b=data.frame(fit$data)
		b$weights = NULL; b$sub.labels = NULL
		b$CITY=city
		rows=which(!is.na(data1$UHImin))
		dates=data.frame(data1$DATE)
		dates=dates[rows,]
		dates=data.frame(dates); colnames(dates)="DATE"
		b$DATE=dates$DATE
		b$VAR=vars[j]
		list2[[j]]=b
     	    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
	}
	result1=rbindlist(list2)
	list1[[i]] = result1
   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

result2=rbindlist(list1)
write.csv(result2,"all_decomposed_time_series_20-50.csv",row.names=F)


#########################
#########################
#### 	Lattice plots  ####
#########################
#########################
library(gridExtra)
df = read.csv("all_decomposed_time_series_20-50.csv",head=T)

df$DATE = as.POSIXct(df$DATE,format="%Y-%m-%d")
df$doy=yday(df$DATE)
df$month = month(df$DATE)

variables = levels(df$VAR)
cities=levels(factor(df$CITY))
x=aggregate(seasonal~CITY+VAR+month,data=df,FUN=mean,na.rm=T)

for (i in 1:length(cities)){
   tryCatch({
	city.name=cities[i]; city.name
	data=subset(x,CITY==cities[i])

	selected = c("UHImin","Delta.NDVI","Delta.SST.TMIN", "PSUN", "WIND", "GHI") 
	data.min=data[data$VAR %in% selected,]
	data.min$VAR = ordered(data.min$VAR, levels = rev(c("UHImin", "PSUN", "WIND", "Delta.NDVI", "Delta.SST.TMIN", "GHI")))
	plot1=xyplot(seasonal~month | VAR,    ## see the powerful conditional formula 
	      data=data.min,
		layout=c(1,floor(nrow(data.min)/12)),
	      main=city.name, scales=list(relation="free",x=list(draw=FALSE)))

	selected = c("UHImax","Delta.NDVI","Delta.SST.TMAX", "PSUN", "WIND", "GHI") 
	data.max=data[data$VAR %in% selected,]
	data.max$VAR = ordered(data.max$VAR, levels = rev(c("UHImax", "PSUN", "WIND", "Delta.NDVI", "Delta.SST.TMAX", "GHI")))
	plot2=xyplot(seasonal~month | VAR,    ## see the powerful conditional formula 
	      data=data.max,
		layout=c(1,floor(nrow(data.min)/12)),
	      main=city.name, scales=list(relation="free",x=list(draw=FALSE)))

	tiffname=paste(city.name,"_ts",".tiff",sep="")
		tiff(tiffname,height=1600,width=1000,res=150)
		grid.arrange(plot1,plot2, ncol=2)
	dev.off()
   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}






