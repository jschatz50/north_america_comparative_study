library(rgdal)
library(spdep)
library(sp)
library(fields)
library(MBA)
library(data.table)

#########################################
### (1) download station list/history ###
#########################################
file <- "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv"
a=download.file(file, destfile = "F:/Users/Jason/Desktop/hourly_NWS/isd_history.csv")

repeat {
	try(download.file(file, destfile = "F:/Users/Jason/Desktop/hourly_NWS/ish_inventory.csv",	quiet = TRUE))
 	if 	(file.info("F:/Users/Jason/Desktop/hourly_NWS/ish_inventory.csv")$size > 0) {
 		break
 	}
 }

####################################
### (2) list of urls to download ###
####################################
station_list = read.csv("F:/Users/Jason/Desktop/hourly_NWS/station_list.csv",header=T)
st <- read.csv("F:/Users/Jason/Desktop/hourly_NWS/isd_history.csv")
dim(st)
head(st)
names(st)[c(3, 9)] <- c("NAME", "ELEV")
st <- st[st$CTRY == "US", ]
st$BEGIN <- as.numeric(substr(st$BEGIN, 1, 4))
st$END <- as.numeric(substr(st$END, 1, 4))

mi.list = st[st$USAF %in% station_list$USAF,]
mi.list = mi.list[mi.list$WBAN %in% station_list$WBAN,]

list1 = list()
for (i in 1:nrow(mi.list)){
	mi.list1 = mi.list[i,]
	list2=list()

	for (s in 1:15) {
		year = 2000+s
		outputs[s, 1] = paste(sprintf("%06d", 
			mi.list1[1,1]), "-", sprintf("%05d", 
			mi.list1[1,2]), "-", year, ".gz", sep = "")
		list2[[s]] = data.frame(paste("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/", year, "/", outputs[s, 1], sep = ""))
	}
	urls1 = rbindlist(list2)
	list1[[i]] = urls1
}
output = rbindlist(list1)
colnames(output) = "URL"
output$USAF = as.numeric(substr(output$URL,44,49))
output$WBAN = as.numeric(substr(output$URL,51,55))

URLs = merge(output, mi.list, by=c("USAF","WBAN"), all.x=T)
URLs$NAME = gsub("/","-",URLs$NAME)

##########################
### (3) download files ###
##########################
for (i in 1:nrow(output)){
   tryCatch({
	row1 = URLs[i,]
	url = as.character(row1$URL)
	outname = paste(row1$NAME, substr(row1$URL,57,60))
	outname.path = paste("F:/Users/Jason/Desktop/hourly_NWS/raw_gzs/", outname, ".gz", sep="")
	download.file(url, outname.path)
   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


#######################
### (4) unzip files ###
#######################
files <- list.files("F:/Users/Jason/Desktop/hourly_NWS/raw_gzs")

for (i in 1:length(files)){
	file1 = paste("F:/Users/Jason/Desktop/hourly_NWS/raw_gzs/", files[i], sep="")
	outname = paste("F:/Users/Jason/Desktop/hourly_NWS/raw_csvs/", 
			     gsub(".gz",".csv",files[i]), sep="")
	a = R.utils::gunzip(file1, destname = outname)
}


#########################
### (5) process files ###
#########################
files <- list.files("F:/Users/Jason/Desktop/hourly_NWS/raw_csvs/new")
column.widths <- c(4, 6, 5, 4, 2, 2, 2, 2, 1, 6, 7, 5, 5, 5, 4, 3, 1, 1, 4, 1, 5, 1, 1, 1, 6, 1, 1, 1, 5, 1, 5, 1, 5, 1)
stations <- as.data.frame(matrix(NA, length(files), 6))
names(stations) <- c("USAFID", "WBAN", "YR", "LAT", "LONG", "ELEV")
for (i in 121:length(files)) {
   tryCatch({
	data <- read.fwf(paste("F:/Users/Jason/Desktop/hourly_NWS/raw_csvs/new/", files[i], sep = ""), column.widths)
	data <- data[, c(2:8, 10:11, 13, 16, 19, 29, 31, 33)]
	names(data) <- c("USAFID", "WBAN", "YR", "M", "D", "HR", "MIN", "LAT", "LONG", "ELEV", "WIND.DIR", "WIND.SPD", "TEMP", "DEW.POINT", "ATM.PRES")
	data$LAT <- data$LAT/1000
	data$LONG <- data$LONG/1000
	data$WIND.SPD <- data$WIND.SPD/10
	data$TEMP <- data$TEMP/10
	data$DEW.POINT <- data$DEW.POINT/10
	data$ATM.PRES <- data$ATM.PRES/10
	write.csv(data, file = paste("F:/Users/Jason/Desktop/hourly_NWS/processed_csvs/", files[i],".csv", sep = ""), row.names = FALSE)
	stations[i, 1:3] <- data[1, 1:3]
	stations[i, 4:6] <- data[1, 8:10]
   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


########################
### (6) aggregations ###
########################
library(RAtmosphere)

#### functions ####
compass2polar = function(deg) {ifelse(deg>=0 & deg<=90, 90-deg,
					 ifelse(deg>90 & deg<180, 360+(90-deg),
					 ifelse(deg>=180 & deg<270, 360-(deg-90),
					 ifelse(deg>=270 & deg<=360, 360-(deg-90), NA))))}

deg2rad <- function(deg) {(deg * pi) / (180)}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]}

mi.list$NAME = gsub("/","-",mi.list$NAME)

files <- list.files("F:/Users/Jason/Desktop/hourly_NWS/processed_csvs")
for (i in 1:length(files)) {
   tryCatch({
	data = read.csv(paste("F:/Users/Jason/Desktop/hourly_NWS/processed_csvs/", files[i], sep = ""), header=T)
	lat = Mode(data$LAT)
	lon = Mode(data$LONG)
	data$DATE = paste(sprintf("%02d", data$M),"/", sprintf("%02d", data$D), "/",data$YR, sep="")
	data$DATE = as.Date(data$DATE, "%m/%d/%Y")
	data$doy = yday(data$DATE)
	data$sunrise=(sapply(data$doy, function(x) suncalc(x, lat, lon, UTC = FALSE)[[1]])/24)
	data$sunset=(sapply(data$doy, function(x) suncalc(x, lat, lon, UTC = FALSE)[[2]])/24)
	data$tod = (data$HR + data$MIN/60)/24
	filtered = data[(data$TEMP < 200),]
	filtered = subset(filtered, MIN == Mode(filtered$MIN))
	filtered[filtered == 999.9] <- NA
	filtered[filtered == 999] <- NA
	filtered$ATM.PRES[filtered$ATM.PRES > 5000] <- NA
	filtered$daynight = ifelse(filtered$tod <= filtered$sunrise | filtered$tod >= filtered$sunset, "night", "day")

	means1 = aggregate(cbind(WIND.DIR, WIND.SPD, TEMP, DEW.POINT, ATM.PRES) ~ DATE + daynight, filtered, FUN=mean, na.rm=T)

	list1=list()
	for (j in min(filtered$doy):max(filtered$doy)){
		df1 = subset(filtered, doy==j)
		df1$WIND.DIR2 = with(df1, ifelse(abs(WIND.DIR-median(WIND.DIR,na.rm=T))>180, 
			ifelse(median(WIND.DIR,na.rm=T)>WIND.DIR, WIND.DIR+360, WIND.DIR-360), WIND.DIR))	
		list1[[j]] = df1
	}
	winds = rbindlist(list1)
	winds$x = with(winds, WIND.SPD * cos(deg2rad(compass2polar(WIND.DIR))))
	winds$y = with(winds, WIND.SPD * sin(deg2rad(compass2polar(WIND.DIR))))
	wind_dir = aggregate(WIND.DIR2 ~ DATE+daynight, winds, FUN = mean, na.rm=T)
	wind_vectors = aggregate(cbind(x,y) ~ DATE+daynight, winds, FUN = sum, na.rm=T)

	df_list = list(means1, wind_dir, wind_vectors)
 	merged = Reduce(function(...) merge(..., all=T), df_list)
	merged$WIND.DIR = NULL
	merged$NAME = substr(files[i],1,nchar(files[i])-13)
	merged = merge(merged, mi.list, by="NAME", all.x=T)
	merged$BEGIN = NULL; merged$END = NULL

	outpath = paste("F:/Users/Jason/Desktop/hourly_NWS/aggregated_csvs/", substr(files[i],1,nchar(files[i])-4), sep="")
	write.csv(merged, outpath, row.names=F)
   }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


#######################
### (7) merge files ###
#######################
library(plyr)
filenames <- list.files("F:/Users/Jason/Desktop/hourly_NWS/aggregated_csvs", full.names=T)
filenames2 <- list.files("F:/Users/Jason/Desktop/hourly_NWS/aggregated_csvs", full.names=FALSE)
import.list <- llply(filenames, read.csv, header=T, check.names=F)

merged1 = rbindlist(import.list)
write.csv(merged1,"daynight_weather_covariates.csv",row.names=F)




