library(reshape2)
library(zoo)
library(ggplot2)
library(plyr)
library(data.table)

###############################################
########## (1) sensor coverage eval ###########
###############################################
elevations = read.csv("F:/Users/Jason/Desktop/Mesowest_data/!PERMANENT/Cities_elevs.csv", header=T)
traits = read.csv("F:/Users/Jason/Desktop/Mesowest_data/!PERMANENT/!Station_traits.csv", header=T)
traits = traits[c("CITY","STID","IMP500","ELEVATION")]
path1 = "F:/Users/Jason/Desktop/Mesowest_data/!QC_data/Data/Canada_AK"

filenames <- list.files(path=path1, full.names=TRUE)
filenames2 <- list.files(path=path1, full.names=FALSE)
import.list <- llply(filenames, read.csv, header=T, check.names=F)

city.data = data.frame(matrix(NA,nrow=length(filenames2),ncol=4))
list.a=list()	##empty list for sensor traits
list.b=list() 	##list of data coverage

### Alaska/Canada: >68%=urban; <15%=rural
### All others: 	 >40%=urban; <10%=rural
elev.thresh = 200
u.thresh = 68
r.thresh = 15

for (v in 1:length(import.list)){		
	tryCatch({
	df = import.list[[v]]
	city.name = filenames2[v]; city.name = gsub("filtered-","",city.name); city.name = gsub("_data.csv","",city.name)
	traits1 = subset(traits,CITY==city.name); traits1$CITY = NULL

	###### eliminate deviant elevations ######
	df = merge(df, traits1, all.x=T, by="STID")
	df = subset(df, IMP500<=r.thresh | IMP500>=u.thresh)
	mean.elev = as.numeric(subset(elevations,CITY==city.name)[4])
	goods = which(traits1$ELEVATION>=(mean.elev-elev.thresh) & traits1$ELEVATION<=(mean.elev+elev.thresh))
	goodIDs = as.character(traits1$STID[goods])
	df = df[df$STID %in% goodIDs,]

	###### separate into tmin/tmax dataframes #####
	df.min = df[,c("DATE","TMIN","IMP500")]
	df.max = df[,c("DATE","TMAX","IMP500")]

	df.max$IMP500 = round(df$IMP500,2)
	df.min$IMP500 = round(df$IMP500,2)
	df.max = dcast(df.max,DATE~IMP500,value.var="TMAX",mean)
	df.min = dcast(df.min,DATE~IMP500,value.var="TMIN",mean)

	df.max$Year = as.numeric(as.character(substr(df.max$DATE, 1, 4)))
	df.min$Year = as.numeric(as.character(substr(df.min$DATE, 1, 4)))
	df.max$Month = as.numeric(as.character(substr(df.max$DATE, 6, 7)))
	df.min$Month = as.numeric(as.character(substr(df.min$DATE, 6, 7)))

	###### create list of all consecutive combinations 2-15 years ######
	years = seq(from=2002,to=2015,by=1)
	year.list = list()

	for (j in 2:14){
	   tryCatch({
		for (i in 1:11){
			year.list[[length(year.list)+1]] <- seq(from=years[i],to=years[i+j],by=1)
		}
	}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
	}

	###### number of continuous urban and rural sensors over those years ######         
	### prep data frames ###
	a=as.numeric(colnames(df.min))
	rurals=which(a<=r.thresh); rurals=as.character(a[rurals])
	urbans=which(a>=u.thresh); urbans=as.character(a[urbans])

	rural=df.max[,colnames(df.max) %in% rurals]; colnames(rural)=rurals
	rural[!is.na(rural)]=1		#1 if present
	rural[is.na(rural)]=0		#0 if absent
	rural$Year=df.max$Year

	urban=data.frame(df.max[,colnames(df.max) %in% urbans]); colnames(urban)=urbans
	urban[!is.na(urban)]=1
	urban[is.na(urban)]=0
	urban$Year=df.max$Year

	### for each set of years, find 3-6 sensors with best data coverage ###
	pick.r = data.frame(matrix(NA,nrow=length(year.list),ncol=4)); colnames(pick.r) = c("rural.sensor.num", "r.percent_coverage", "r.SIDs","r.years")
	pick.u = data.frame(matrix(NA,nrow=length(year.list),ncol=4)); colnames(pick.u) = c("urban.sensor.num", "u.percent_coverage", "u.SIDs","u.years")
	for (i in 1:(length(year.list))){
		tmp.yrs=data.frame(year.list[[(i)]]); colnames(tmp.yrs)="Year"
		r.tmp = rural[rural$Year %in% tmp.yrs$Year,]
		r.continuous=colSums(r.tmp)/nrow(r.tmp)
		df.r=data.frame(matrix(NA,nrow=length(year.list),ncol=12))  #; colnames(df1)=c("rural","urban","number_of_years")
		for (k in 3:6){
			top5 = which(r.continuous[1:(length(r.continuous)-1)] %in% tail(sort(as.vector(r.continuous)),k) &
				r.continuous[1:(length(r.continuous)-1)]>0.5)
			df.r[i,(k-2)] = length(top5)	#number of sensors
			df.r[i,(k+2)] = sum(rowSums(r.tmp[top5]) == length(top5),na.rm=T)/nrow(r.tmp[top5])*100		#percent of rows
			df.r[i,(k+6)] = paste(as.matrix(names(top5)),collapse=",")
		}
		df.r$yrs = nrow(tmp.yrs)
		df.r$a = ifelse(df.r$X1 >=3, df.r$X1*3 + df.r$X5, 0)	#extra sensors count for 3 percent
		df.r$b = ifelse(df.r$X2 >=3, df.r$X2*3 + df.r$X6, 0)
		df.r$c = ifelse(df.r$X3 >=3, df.r$X3*3 + df.r$X7, 0)
		df.r$d = ifelse(df.r$X4 >=3, df.r$X4*3 + df.r$X8, 0)
		best.r = which(df.r[i,14:17] == max(df.r[i,14:17])) [1]
		cols = c(best.r, best.r+4, best.r+8, 13)	
		pick.r[i,] = df.r[i,cols]
		rownames(pick.r)[i]=paste(as.matrix(tmp.yrs),collapse=",")

		u.tmp= urban[urban$Year %in% tmp.yrs$Year,]
		u.continuous=colSums(u.tmp)/nrow(u.tmp)
		df.u=data.frame(matrix(NA,nrow=length(year.list),ncol=12))  #; colnames(df1)=c("rural","urban","number_of_years")
		for (k in 3:6){
			top5 = which(u.continuous[1:(length(u.continuous)-1)] %in% tail(sort(as.vector(u.continuous)),k) &
				u.continuous[1:(length(u.continuous)-1)]>0.5)
			df.u[i,(k-2)] = length(top5)	#number of sensors
			df.u[i,(k+2)] = sum(rowSums(u.tmp[top5]) == length(top5),na.rm=T)/nrow(u.tmp[top5])*100		#number of rows
			df.u[i,(k+6)] = paste(as.matrix(names(top5)),collapse=",")
		}
		df.u$yrs = nrow(tmp.yrs)
		df.u$a = ifelse(df.u$X1 >=3, df.u$X1*3 + df.u$X5, 0)	#extra sensors count for 3 percent
		df.u$b = ifelse(df.u$X2 >=3, df.u$X2*3 + df.u$X6, 0)
		df.u$c = ifelse(df.u$X3 >=3, df.u$X3*3 + df.u$X7, 0)
		df.u$d = ifelse(df.u$X4 >=3, df.u$X4*3 + df.u$X8, 0)
		best.u = which(df.u[i,14:17] == max(df.u[i,14:17])) [1]
		cols = c(best.u, best.u+4, best.u+8, 13)	
		pick.u[i,] = df.u[i,cols]
		rownames(pick.u)[i]=paste(as.matrix(tmp.yrs),collapse=",")
	}
	
	df1 = cbind(pick.r,pick.u)
	df1$years = rownames(df1)

	###### select years/sensors with at least 3/3 urban/rural sensors with at least 80% data coverage ######
	reduced=df1[which(df1$rural.sensor.num>=3 & df1$urban.sensor.num>=3 & df1$r.percent_coverage>=0.8 & df1$u.percent_coverage>=0.8),]
	reduced$numsens = reduced$rural.sensor.num + reduced$urban.sensor.num
	reduced$mean.coverage = (reduced$r.percent_coverage + reduced$u.percent_coverage)/2 
	reduced=reduced[order(-reduced$mean.coverage),]
	reduced$CITY=city.name
	list.b[[v]] = reduced
	}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

z=rbindlist(list.b)
write.csv(z,"sensor_coverage_CanAK.csv",row.names=F)




####################################################################################
########## (2) calculate daily UHI intensity for Tmax/Tmin for each city ###########
##########	   plus leave-one-out cross validation			     ###########
####################################################################################
sensor.picks = read.csv("sensor_coverage_CanAK_R.csv",header=T,check.names=F)
traits = read.csv("F:/Users/Jason/Desktop/Mesowest_data/!PERMANENT/!Station_traits.csv", header=T)
traits = traits[c("CITY","STID","IMP500","ELEVATION")]
path1 = "F:/Users/Jason/Desktop/Mesowest_data/!QC_data/Data/Canada_AK"

filenames <- list.files(path=path1, full.names=TRUE)
filenames2 <- list.files(path=path1, full.names=FALSE)
import.list <- llply(filenames, read.csv, header=T, check.names=F)

list.a = list()

for (v in 1:length(import.list)){		
	tryCatch({

	###### prep data frame ######
	df=import.list[[v]]
	city.name=filenames2[v]; city.name=gsub("filtered-","",city.name); city.name=gsub("_data.csv","",city.name)
	picks1 = subset(sensor.picks, CITY==city.name)
	urban.sensors = unlist(strsplit(as.character(picks1$u.SIDs),','))
	rural.sensors = unlist(strsplit(as.character(picks1$r.SIDs),','))
	final.years = as.numeric(unlist(strsplit(as.character(picks1$years),',')))
	traits1=subset(traits,CITY==city.name); traits1$CITY=NULL
	df = merge(df, traits1, all.x=T, by="STID")
	df.min=df[,c("DATE","TMIN","IMP500")]
	df.max=df[,c("DATE","TMAX","IMP500")]

	df.max$IMP500=round(df$IMP500,2)
	df.min$IMP500=round(df$IMP500,2)
	df.max=dcast(df.max,DATE~IMP500,value.var="TMAX",mean)
	df.min=dcast(df.min,DATE~IMP500,value.var="TMIN",mean)
	df.max$Year=as.numeric(as.character(substr(df.max$DATE, 1, 4)))
	df.min$Year=as.numeric(as.character(substr(df.min$DATE, 1, 4)))

	df.max = df.max[df.max$Year %in% as.numeric(unlist(strsplit(as.character(final.years),','))),]
	df.min = df.min[df.min$Year %in% as.numeric(unlist(strsplit(as.character(final.years),','))),]

	###### generate all combinations of urban and rural sensors (for leave-one-out) ######	
	u.combs = combn(urban.sensors, length(urban.sensors)-1)
	u.combs = as.list(data.frame(u.combs))
	u.combs[[length(u.combs)+1]] = urban.sensors

	r.combs = combn(rural.sensors, length(rural.sensors)-1)
	r.combs = as.list(data.frame(r.combs))
	r.combs[[length(r.combs)+1]] = rural.sensors

	s1 = seq(1,length(u.combs),1)
	s2 = seq(1,length(r.combs),1)
	all.combs = expand.grid(s1,s2)

	###### leave-one-out loop ######
	list.b = list()
	for (k in 1:nrow(all.combs)){
		urbans = u.combs[[all.combs[k,1]]]
		rurals = r.combs[[all.combs[k,2]]]
			'%nin%' <- Negate('%in%')	##"not in" function
		u.out = urban.sensors[urban.sensors %nin% urbans]		#identity of sensor left out
		r.out = rural.sensors[rural.sensors %nin% rurals]		#identity of sensor left out

		urban.max=data.frame(df.max[as.character(urbans)]); colnames(urban.max)=as.character(urbans); urban.max$DATE=df.max$DATE
   		   urban.max$mean.umax=rowMeans(urban.max[,1:(ncol(urban.max)-1)])
		rural.max=data.frame(df.max[as.character(rurals)]); colnames(rural.max)=as.character(rurals); rural.max$DATE=df.max$DATE
   		   rural.max$mean.rmax=rowMeans(rural.max[,1:(ncol(rural.max)-1)])
		urban.min=data.frame(df.min[as.character(urbans)]); colnames(urban.min)=as.character(urbans); urban.min$DATE=df.min$DATE
   		   urban.min$mean.umin=rowMeans(urban.min[,1:(ncol(urban.min)-1)])
		rural.min=data.frame(df.min[as.character(rurals)]); colnames(rural.min)=as.character(rurals); rural.min$DATE=df.min$DATE
   		   rural.min$mean.rmin=rowMeans(rural.min[,1:(ncol(rural.min)-1)])

		### calculate daily UHI intensity for each sensor combo ###
		UHI.max = merge(urban.max,rural.max,by="DATE")
		UHI.max$UHImax = UHI.max$mean.umax - UHI.max$mean.rmax
		UHI.min = merge(urban.min,rural.min,by="DATE")
		UHI.min$UHImin = UHI.min$mean.umin - UHI.min$mean.rmin
		final = merge(UHI.max,UHI.min,by.x="DATE", by.y="DATE")
		final = final[,c("DATE","UHImax","UHImin")]
		colnames(final)=c("DATE", paste("max",u.out,r.out), paste("min",u.out,r.out))
		list.b[[k]] = final
		}
		
		mydf <- data.frame(list.b)
		mydf$CITY = city.name
		list.a[[v]] = mydf

	}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

#ten.forty.list = list.a
#twenty.fifty.list = list.a


#############################################
#############################################
######   (3) tolerance testing         ######
#############################################
#############################################
library(zoo)

###### create functions ######
### standardize data ###
standardize <- function(x){
	stdevs <- mad(x,na.rm=T)
	scaled <- x/stdevs
	centered <- scaled - mean(scaled,na.rm=T)
	return(centered) }

### replace NAs with nearest value for filling small gaps (one gap per call) ###
my.func = function(dat) {	
    nas=is.na(dat)
    if (!any(!nas)) return (dat)
    t=rle(nas)
    f=sapply(t$lengths[t$values],seq)
    a=unlist(f)
    b=unlist(lapply(f,rev))
    x=which(nas)
    l=length(dat)
    dat[nas]=ifelse(a>b,dat[ ifelse((x+b)>l,x-a,x+b) ],dat[ifelse((x-a)<1,x+b,x-a)])
    dat }

### find peaks/troughs ###
find_peaks <- function (x, m ){
    shape <- diff(sign(diff(x, na.pad = FALSE)))
    pks <- sapply(which(shape < 0), FUN = function(i){
       z <- i - m + 1
       z <- ifelse(z > 0, z, 1)
       w <- i + m + 1
       w <- ifelse(w < length(x), w, length(x))
       if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
    })
     pks <- unlist(pks)
     pks
}

find_troughs <- function (x, m ){
    shape <- diff(sign(diff(x, na.pad = FALSE)))
    pks <- sapply(which(shape > 0), FUN = function(i){
       z <- i - m + 1
       z <- ifelse(z > 0, z, 1)
       w <- i + m + 1
       w <- ifelse(w < length(x), w, length(x))
       if(all(x[c(z : i, (i + 2) : w)] >= x[i + 1])) return(i + 1) else return(numeric(0))
    })
     pks <- unlist(pks)
     pks
}


###### detrend data, fit loess curve through seasonal trends, extract peaks/troughs/amplitudes for each leave-one-out and full dataset ######
list2=list()
for (j in 1:length(list.a)){
	tryCatch({

	### prep data frame for each city ###
	df2 = list.a[[j]]
	city.name = df2$CITY[1]
	dates1 = df2[,1]
	df2 = df2[,-seq(1, ncol(df2), by = 3)]
	maxes = df2[,seq(1, ncol(df2), by = 2)]
		maxes$DATE = dates1
		maxes$Year=as.numeric(as.character(substr(maxes$DATE, 1, 4)))
		maxes$Month=as.numeric(as.character(substr(maxes$DATE, 6, 7)))
	mins = df2[,seq(2, ncol(df2), by = 2)]; mins$DATE = dates1
		mins$Year=as.numeric(as.character(substr(mins$DATE, 1, 4)))
		mins$Month=as.numeric(as.character(substr(mins$DATE, 6, 7)))
	
	### for each leave-one-out/full, calculate troughs, peaks, amplitudes ###
	results = data.frame(matrix(NA,(nrow=ncol(mins)-3),ncol=4)); colnames(results) = c("peak","trough","amplitude","sensors")
	for (i in 1:(ncol(mins)-3)){		############# 1:(ncol(mins)-3)
		tryCatch({
		tmp1 = mins[,c("DATE","Year","Month")]
		tmp1$UHI = as.numeric(as.character((mins[,i])))
		tmp1$UHI = standardize(tmp1$UHI)
		baseyear = tmp1$Year[1]
		endyear = tmp1$Year[nrow(tmp1)]
		y = as.vector(tmp1$UHI)
		#y=na.approx(y,na.rm="FALSE")
		x = as.vector(seq(1, length(y), by=1))

		### detrend ###
		house.ts = ts(y, frequency=365, start=c(baseyear,1))
		fit = stlplus(house.ts, s.window="periodic", n.p = 361, na.action = na.approx)
		plot(fit)
		y=seasonal(fit)

		
		### fit loess curve to detrended data ###
		tiffname=paste(city.name,"-tmins.tiff",sep="")	#tmins/maxes
		tiff(tiffname,height=800,width=1600,res=150)
	
		smooth_vals = predict(loess(y~x,fullrange=T,span=1/length(levels(factor(tmp1$Year)))), se=TRUE, seq(length(y)), na.action=na.pass)
		plot(y ~ x, ylab="UHI Tmin (stdized)", xlab="Day", pch=16,cex=0.4,ylim=c(-2,2))
		lines(smooth_vals$fit, lty="solid", col="darkred", lwd=3)
		lines(smooth_vals$fit-1.96*smooth_vals$se.fit, lty="dashed", col="blue", lwd=1)
		lines(smooth_vals$fit+1.96*smooth_vals$se.fit, lty="dashed", col="blue", lwd=1)
		abline(h=0)
		abline(v=c(seq(from=0, to=length(y), by=365)))
		dev.off()

		### date of peak, date of trough, difference ###
		peaks=find_peaks(my.func(my.func(my.func(my.func(smooth_vals$fit)))), m = 210)
		troughs=find_troughs(my.func(my.func(my.func(my.func(smooth_vals$fit)))), m = 210)
		peak.vals=smooth_vals$fit[peaks]
		peak.se=smooth_vals$se.fit[peaks]
		trough.vals=smooth_vals$fit[troughs]
		trough.se=smooth_vals$se.fit[troughs]

		peaks=data.frame(peaks,peak.vals,peak.se); colnames(peaks)=c("day","val","se")
		   peaks$doy = (peaks$day/365 - floor(peaks$day/365)) * 365
		   peaks = peaks[!(peaks$day<30 | peaks$day>(length(y)-30)),]		###excludes the first & last peaks and troughs if tangents are linear
		peaks$doy2 = with(peaks, ifelse(abs(doy-median(doy))>182, 
			ifelse(median(doy)>doy, doy+365, doy-365), doy))			###accounts for cyclic nature of doy data (i.e. day 1 and day 365 are 1 day apart, not 364 days apart

		troughs=data.frame(troughs,trough.vals,trough.se); colnames(troughs)=c("day","val","se")
		   troughs$doy = (troughs$day/365 - floor(troughs$day/365)) * 365
		   troughs = troughs[!(troughs$day<30 | troughs$day>(length(y)-30)),]
		troughs$doy2 = with(troughs, ifelse(abs(doy-median(doy))>182, 
					ifelse(median(doy)>doy, doy+365, doy-365), doy))

		m=rbind(peaks,troughs)
		m <- m[order(m$day),]
		m$diff=c(NA,abs(diff(m$val)))
		first = m$day[1]
		last = m$day[nrow(m)]
		
		results[i,] = data.frame(median(peaks$doy[2:(nrow(peaks))]),median(troughs$doy[2:(nrow(troughs))]),median(m$diff, na.rm=T))
		results[i,4] = colnames(mins)[i]; results
		}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
		}
	results$CITY = city.name
	list2[[j]]=results
	}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

z=rbindlist(list2)
write.csv(z,"tmins_leave_one_out_results_CanAK.csv",row.names=F)

filenames2[which(sapply(list2, length)==0)]


#################################
#######(4) error rates ##########
#################################
library(stringr)

path1 = "F:/Users/Jason/Desktop/Mesowest_data/!Leave_one_out_results"
filenames <- list.files(path=path1, full.names=TRUE)
filenames2 <- list.files(path=path1, full.names=FALSE)
import.list <- llply(filenames, read.csv, header=T, check.names=F)

data=read.csv("tmins_leave_one_out_results_CanAK.csv",header=T)
write.csv(rmse,"Canada_AK_tmins_rmses.csv",row.names=F)

for (l in 1:length(import.list)){
	data = import.list[[l]]
	fulls = which(substr(data$sensors, 6, 6)=="")
	singles1 = which(substr(data$sensors, 4,5)=="..")
	singles2 = which(str_sub(data$sensors, -1,-1)==".")
	singles = unique(c(singles1,singles2))
	singles = singles [! singles %in% fulls]

	df1 = data[singles,]
		df1 = df1[order(df1$CITY),]
	df2 = data[fulls,]
	df3 = merge(df1,df2, by="CITY")

	df3$peak.final = with(df3, ifelse(abs(peak.y-peak.x)>182, 
			ifelse(peak.y>peak.x, peak.x+365, peak.x-365),
			peak.x))
	df3$trough.final = with(df3, ifelse(abs(trough.y-trough.x)>182, 
			ifelse(trough.y>trough.x, trough.x+365, trough.x-365),
			trough.x))

	df3$se.peak = with(df3, (peak.final - peak.y)^2)
	df3$se.trough = with(df3, (trough.final - trough.y)^2)
	df3$se.amplitude = with(df3, (amplitude.x - amplitude.y)^2)

	rmse = aggregate(x=list(df3$se.peak, df3$se.trough, df3$se.amplitude), by=list(df3$CITY), FUN=mean)
		colnames(rmse) = c("CITY","peak.rmse","trough.rmse","amplitude.rmse")
	temp=sqrt(rmse[,2:4])
	rmse = cbind(rmse$CITY,temp); colnames(rmse)[1]="CITY"
	rmse = merge(rmse,df2,by="CITY"); rmse$sensors=NULL
	rmse$percent.amplitude = (rmse$amplitude.rmse / rmse$amplitude) * 100

	outfile = paste("processed",filenames2[[l]],sep="-")
	length(which(rmse$peak.rmse<45 & rmse$trough.rmse<45))
	write.csv(rmse,outfile,row.names=F)
}

