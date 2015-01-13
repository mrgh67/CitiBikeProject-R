getwd()
setwd("/Users/mrgholt/Desktop/CITIBIKE_DATA/")
system("ls -l")

install.packages("plyr")
library("plyr")
install.packages("RCurl")
library("RCurl")
install.packages("RJSONIO")
library("RJSONIO")


prepareData = function(x) {
	df = read.csv(x, header=TRUE)
	df = na.omit(df)
	df$starttime = as.POSIXct(df$starttime)
	df$stoptime = as.POSIXct(df$stoptime)
	
	cat("No of rows in df is ", nrow(df), "\n")
	
	dfM = df[df$gender==1,]
	dfMS = dfM[dfM$usertype=="Subscriber",]
	dfMSD=cbind(weekdays(as.Date(dfMS$starttime)), dfMS)
	
	aWeekday=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
	colnames(dfMSD)[1]="day"
	dfMSMF = dfMSD[dfMSD$day %in% aWeekday,]
	
	cat("No of rows in df is ", nrow(dfMSMF), "\n")

	
	#New Years day
	dfMSMF = dfMSMF[format(dfMSMF$starttime,"%Y:%m:%d")!="2014:01:01", ]
	#Martin Luther King day
	dfMSMF = dfMSMF[format(dfMSMF$starttime,"%Y:%m:%d")!="2014:01:20", ]
	#President's day
	dfMSMF = dfMSMF[format(dfMSMF$starttime,"%Y:%m:%d")!="2014:02:17", ]
	#Memorial day
	dfMSMF = dfMSMF[format(dfMSMF$starttime,"%Y:%m:%d")!="2014:05:26", ]
	#Independence day
	dfMSMF = dfMSMF[format(dfMSMF$starttime,"%Y:%m:%d")!="2014:07:04", ]
	#Labor day
	dfMSMF = dfMSMF[format(dfMSMF$starttime,"%Y:%m:%d")!="2013:09:02", ]
	#Columbus day
	dfMSMF = dfMSMF[format(dfMSMF$starttime,"%Y:%m:%d")!="2013:10:14", ]
	#Veterans day
	dfMSMF = dfMSMF[format(dfMSMF$starttime,"%Y:%m:%d")!="2013:11:11", ]
	#Thanksgiving
	dfMSMF = dfMSMF[format(dfMSMF$starttime,"%Y:%m:%d")!="2013:11:28", ]
	#Christmas day
	dfMSMF = dfMSMF[format(dfMSMF$starttime,"%Y:%m:%d")!="2013:12:25", ]
	
	cat("No of rows in df is ", nrow(dfMSMF), "\n")
	
	dfMSMF = dfMSMF[format(dfMSMF$starttime, "%H:%M")>="09:00",]
	dfMSMF = dfMSMF[format(dfMSMF$starttime, "%H:%M")<="17:00",]

	cat("No of rows in df is ", nrow(dfMSMF), "\n")

	return(dfMSMF)
}



dfJan=prepareData("2014-01 - Citi Bike trip data.csv")
nrow(dfJan)
dfFeb=prepareData("2014-02 - Citi Bike trip data.csv")
nrow(dfFeb)
dfMar=prepareData("2014-03 - Citi Bike trip data.csv")
nrow(dfMar)
dfApr=prepareData("2014-04 - Citi Bike trip data.csv")
nrow(dfApr)
dfMay=prepareData("2014-05 - Citi Bike trip data.csv")
nrow(dfMay)
dfJun=prepareData("2014-06 - Citi Bike trip data.csv")
nrow(dfJun)
dfJul=prepareData("2014-07 - Citi Bike trip data.csv")
nrow(dfJul)
dfAug=prepareData("2014-08 - Citi Bike trip data.csv")
nrow(dfAug)
dfSep=prepareData("2013-09 - Citi Bike trip data.csv")
nrow(dfSep)
dfOct=prepareData("2013-10 - Citi Bike trip data.csv")
nrow(dfOct)
dfNov=prepareData("2013-11 - Citi Bike trip data.csv")
nrow(dfNov)
dfDec=prepareData("2013-12 - Citi Bike trip data.csv")
nrow(dfDec)


doMore = function(dfAnon) {
	dfSummary = ddply(.data=dfAnon, .variables=c("start.station.id", "end.station.id"), .fun=function(x) return(c(length(x$tripduration),min(x$tripduration),max(x$tripduration), median(x$tripduration))))
	dfSummary = dfSummary[dfSummary$start.station.id != dfSummary$end.station.id, ]
	colnames(dfSummary)[3] = "no_of_trips"
	colnames(dfSummary)[4] = "min_duration"
	colnames(dfSummary)[5] = "max_duration"
	colnames(dfSummary)[6] = "median_duration"
	tripOrder = order(dfSummary$no_of_trips, decreasing=TRUE)
	dfSummary = dfSummary[tripOrder,]
	return(dfSummary)
}
#---------------------------------S1 Get the numbers of trips between any 2 stations.

dfS1Jan = doMore(dfJan)
dfS1Jan[900:915,]

dfS1Feb = doMore(dfFeb)

dfS1Mar = doMore(dfMar)
dfS1Mar[345:355,]

dfS1Apr = doMore(dfApr)
dfS1Apr[0:15,]

dfS1May = doMore(dfMay)
dfS1May[0:15,]

dfS1Jun = doMore(dfJun)
dfS1Jun[0:15,]

dfS1Jul = doMore(dfJul)
dfS1Jul[0:15,]

dfS1Sep = doMore(dfSep)
str(dfS1Sep)
dfS1Sep[1:10,]

dfS1Aug = doMore(dfAug)
str(dfS1Aug)
dfS1Aug[1:10,]

dfS1Oct = doMore(dfOct)
str(dfS1Oct)
dfS1Oct[1:25,]

dfS1Nov = doMore(dfNov)
str(dfS1Nov)
dfS1Nov[1:25,]

dfS1Dec = doMore(dfDec)

#---------------------------------Create a Template

dfJanF = dfJan[1:1000,]
dfFebF = dfFeb[1:1000,]
dfMarF = dfMar[1:1000,]
dfAprF = dfApr[1:1000,]
dfMayF = dfMay[1:1000,]
dfJunF = dfJun[1:1000,]
dfJulF = dfJul[1:1000,]
dfAugF = dfAug[1:1000,]
dfSepF = dfSep[1:1000,]
dfOctF = dfOct[1:1000,]
dfNovF = dfNov[1:1000,]
dfDecF = dfDec[1:1000,]

#-------------------------------------------

processTopFiveH = function(dfJan, dfS1, dfJanF) {
	count = 1
	for(i in 1:1000) {
		b=dfJan[dfJan$start.station.id == dfS1$start.station.id[i] & dfJan$end.station.id == dfS1$end.station.id[i] & dfJan$tripduration == dfS1$min_duration[i],]
		if(nrow(b) > 1) {
			cat("found ", nrow(b), " on row ", i, "\n")
			for(j in 1:nrow(b)) {
				dfJanF[count,] = b[j,]
				count = count + 1
			}
		} else {
			dfJanF[count, ] = b
			count = count + 1
		}
		if(count>1000)
			break
	}	
	return(dfJanF)
}

#-------------------------------------------Fill the Template. From the top travelled journeys pull those that had the minimum trip 
#-----------------------------------------------------duration
dfJanF = processTopFiveH(dfJan, dfS1Jan, dfJanF)

dfFebF = processTopFiveH(dfFeb, dfS1Feb, dfFebF)
str(dfFebF)

dfMarF = processTopFiveH(dfMar, dfS1Mar, dfMarF)

dfAprF = processTopFiveH(dfApr, dfS1Apr, dfAprF)

dfMayF = processTopFiveH(dfMay, dfS1May, dfMayF)

dfJunF = processTopFiveH(dfJun, dfS1Jun, dfJunF)

dfJulF = processTopFiveH(dfJul, dfS1Jul, dfJulF)

dfAugF = processTopFiveH(dfAug, dfS1Aug, dfAugF)

dfSepF = processTopFiveH(dfSep, dfS1Sep, dfSepF)
str(dfSepF)

dfOctF = processTopFiveH(dfOct, dfS1Oct, dfOctF)

dfNovF = processTopFiveH(dfNov, dfS1Nov, dfNovF)

dfDecF = processTopFiveH(dfDec, dfS1Dec, dfDecF)
str(dfDecF)
#--------------------------------------------Reorder the filled template

save(dfJanF, file="dfJanF.Rda")
#load("dfJanF.Rda")

save(dfFebF, file="dfFebF.Rda")

save(dfMarF, file="dfMarF.Rda")

save(dfAprF, file="dfAprF.Rda")

save(dfMayF, file="dfMayF.Rda")

save(dfJunF, file="dfJunF.Rda")

save(dfJulF, file="dfJulF.Rda")

save(dfAugF, file="dfAugF.Rda")

save(dfSepF, file="dfSepF.Rda")

save(dfOctF, file="dfOctF.Rda")

save(dfNovF, file="dfNovF.Rda")

save(dfDecF, file="dfDec.Rda")
#-----------------------------------------------------




getOtherParams = function(df, index) {
	myMapQuestKey = "Fmjtd%7Cluurnua2nq%2Ca2%3Do5-9w859r"
	
	fromLat = df$start.station.latitude[index]
	fromLong = df$start.station.longitude[index]
	toLat = df$end.station.latitude[index]
	toLong = df$end.station.longitude[index]
	
	urlA = paste0("http://open.mapquestapi.com/directions/v2/route?key=",myMapQuestKey,"&from=",fromLat,",",fromLong,"&to=",toLat,",",toLong,"&routeType=bicycle&doReverseGeocode=True&narrativeType=none&cyclinggRoadFactor=1.0")

	webA = getURL(urlA)
	rawA = fromJSON(webA)
	distance = rawA$route$distance
	startstationzip = rawA$route$locations[[1]]$postalCode
	startstationcity = rawA$route$locations[[1]]$adminArea5
	if(startstationcity == "") {
		cat("found a blank city parameters, index is ", index, " - trying route locations 2\n")
		startstationcity = rawA$route$locations[[2]]$adminArea5
	}

	startstationcity = gsub(" ","_",startstationcity)
	
	#cat(startstationcity, "\n")
	startstationstate ="NY"

	urlElev = paste0("http://open.mapquestapi.com/elevation/v1/profile?key=",myMapQuestKey,"&latLngCollection=",fromLat,",",fromLong,",",toLat,",",toLong)
	webB = getURL(urlElev)
	rawB = fromJSON(webB)
	heightdifferential = rawB$elevationProfile[[2]][["height"]]-rawB$elevationProfile[[1]][["height"]]
	
	
	theDate = strftime(df$starttime[index], "%Y%m%d")
	#cat("the date is ", theDate, "\n")
	myWunderGroundAPI= "34fd63423ce8c852"
	urlWeatherWG = paste0("http://api.wunderground.com/api/", myWunderGroundAPI, "/history_",theDate,"/q/", startstationstate,"/",startstationcity,".json")
	webWG = getURL(urlWeatherWG)
	rawWG = fromJSON(webWG)
	
	startHour = as.numeric(strftime(df$starttime[index], "%H"))
	startMin = as.numeric(strftime(df$starttime[index], "%M"))
	sMins = (startHour * 60) + startHour
	
	#cat("start hour is ", startHour, "\n")
	#cat("start min is ", startMin, "\n")
	
	bMin = 9999999999999
	i=-1
	for (i in 1:length(rawWG$history[[3]])) {
		skk = sprintf("%s", rawWG$history[[3]][[i]]$date)
		#cat(skk,"\n")
		hour = as.numeric(rawWG$history[[3]][[i]]$date["hour"])
		minute = as.numeric(rawWG$history[[3]][[i]]$date["min"])
		allMins = (hour * 60) + minute
		diffMins = abs(allMins - sMins)		
		#skk = cat("hour is ", hour, "min is ", minute, "diff is ",  diffMins, "\n")

		if(diffMins < bMin) {
			bMin = diffMins
			bIndex = i
		}
	}
	hour = as.numeric(rawWG$history[[3]][[bIndex]]$date["hour"])
	minute = as.numeric(rawWG$history[[3]][[bIndex]]$date["min"])
	#cat("min index is ", bIndex, "which is hour ", hour, " and min ", minute,"\n")
	

	dewPoint = as.numeric(rawWG$history[[3]][[bIndex]]["dewpti"])
	#Dew Point in Fahrenheit

	precip = as.numeric(rawWG$history[[3]][[bIndex]]["precipi"])
	#precipitation in inches
		
	humidity = as.numeric(rawWG$history[[3]][[bIndex]]["hum"])
	#humidity as a percentage
	
	snow = as.numeric(rawWG$history[[3]][[bIndex]]["snow"])
	
	temp = as.numeric(rawWG$history[[3]][[bIndex]]["tempi"])
	#temperature in Fahrenheit
	
	windSpd = as.numeric(rawWG$history[[3]][[bIndex]][["wspdi"]])
	#windspeed in miles per hour
	
	visibility = as.numeric(rawWG$history[[3]][[bIndex]][["visi"]])
	#visibility in miles
	
	windchill = as.numeric(rawWG$history[[3]][[bIndex]][["windchilli"]])
	#Wind chill in Fahrenheit
	
	heatindex = as.numeric(rawWG$history[[3]][[bIndex]][["heatindexi"]])
	#Heat index in Fahrenheit
	
	
	fog = as.numeric(rawWG$history[[3]][[bIndex]]["fog"])
	rain = as.numeric(rawWG$history[[3]][[bIndex]]["rain"])
	
	hail = as.numeric(rawWG$history[[3]][[bIndex]]["hail"])
	thunder = as.numeric(rawWG$history[[3]][[bIndex]]["thunder"])
	tornado = as.numeric(rawWG$history[[3]][[bIndex]]["tornado"])

	
	theStr = sprintf("dew %f, precip %f, hum %f, snow %f, temp %f, windspeed, %f, vis %f, windchill %f, heatindex %f, fog %f, rain %f, hail %f, thunder %f, tornado, %f\n", dewPoint, precip, humidity, snow, temp, windSpd, visibility, windchill, heatindex, fog, rain, hail, thunder, tornado)
	#cat(theStr)
	
	
	results = c("dist" = distance, "zip" = startstationzip, "elev" = heightdifferential, "dew" = dewPoint, "precip" = precip, "hum" = humidity,  "snow" = snow, "temp" = temp, "windspeed" = windSpd, "visibility" = visibility, "windchill" = windchill, "heatindex" = heatindex, "fog" = fog, "rain" = rain, "hail" = hail, "thunder" = thunder, "tornado" = tornado)
	
	return(results)
}
#-----------------------------------------Expand the Data Frame

dfJanComplete = dfJanF
dfJanComplete = cbind(dfJanComplete, "distance" = 0, "zip" = 0, "elev" = 0, "dew" = 0, "precip" = 0, "hum" = 0, "snow" = 0, "temp" = 0, "windspeed" = 0, "visibility" = 0, "windchill" = 0, "heatindex" = 0, "fog" = 0, "rain" = 0, "hail" = 0, "thunder" = 0, "tornado" = 0)
dfJanComplete[1:10,]

dfFebComplete = dfFebF
dfFebComplete = cbind(dfFebComplete, "distance" = 0, "zip" = 0, "elev" = 0, "dew" = 0, "precip" = 0, "hum" = 0, "snow" = 0, "temp" = 0, "windspeed" = 0, "visibility" = 0, "windchill" = 0, "heatindex" = 0, "fog" = 0, "rain" = 0, "hail" = 0, "thunder" = 0, "tornado" = 0)
dfFebComplete[1:10,]

dfMarComplete = dfMarF
dfMarComplete = cbind(dfMarComplete, "distance" = 0, "zip" = 0, "elev" = 0, "dew" = 0, "precip" = 0, "hum" = 0, "snow" = 0, "temp" = 0, "windspeed" = 0, "visibility" = 0, "windchill" = 0, "heatindex" = 0, "fog" = 0, "rain" = 0, "hail" = 0, "thunder" = 0, "tornado" = 0)
dfMarComplete[1:10,]

dfAprComplete = dfAprF
dfAprComplete = cbind(dfAprComplete, "distance" = 0, "zip" = 0, "elev" = 0, "dew" = 0, "precip" = 0, "hum" = 0, "snow" = 0, "temp" = 0, "windspeed" = 0, "visibility" = 0, "windchill" = 0, "heatindex" = 0, "fog" = 0, "rain" = 0, "hail" = 0, "thunder" = 0, "tornado" = 0)
dfAprComplete[1:10,]

dfMayComplete = dfMayF
dfMayComplete = cbind(dfMayComplete, "distance" = 0, "zip" = 0, "elev" = 0, "dew" = 0, "precip" = 0, "hum" = 0, "snow" = 0, "temp" = 0, "windspeed" = 0, "visibility" = 0, "windchill" = 0, "heatindex" = 0, "fog" = 0, "rain" = 0, "hail" = 0, "thunder" = 0, "tornado" = 0)
dfMayComplete[1:10,]

dfJunComplete = dfJunF
dfJunComplete = cbind(dfJunComplete, "distance" = 0, "zip" = 0, "elev" = 0, "dew" = 0, "precip" = 0, "hum" = 0, "snow" = 0, "temp" = 0, "windspeed" = 0, "visibility" = 0, "windchill" = 0, "heatindex" = 0, "fog" = 0, "rain" = 0, "hail" = 0, "thunder" = 0, "tornado" = 0)
dfJunComplete[1:10,]

dfJulComplete = dfJulF
dfJulComplete = cbind(dfJulComplete, "distance" = 0, "zip" = 0, "elev" = 0, "dew" = 0, "precip" = 0, "hum" = 0, "snow" = 0, "temp" = 0, "windspeed" = 0, "visibility" = 0, "windchill" = 0, "heatindex" = 0, "fog" = 0, "rain" = 0, "hail" = 0, "thunder" = 0, "tornado" = 0)
dfJulComplete[1:10,]

dfAugComplete = dfAugF
dfAugComplete = cbind(dfAugComplete, "distance" = 0, "zip" = 0, "elev" = 0, "dew" = 0, "precip" = 0, "hum" = 0, "snow" = 0, "temp" = 0, "windspeed" = 0, "visibility" = 0, "windchill" = 0, "heatindex" = 0, "fog" = 0, "rain" = 0, "hail" = 0, "thunder" = 0, "tornado" = 0)
dfAugComplete[1:10,]

dfSepComplete = dfSepF
dfSepComplete = cbind(dfSepComplete, "distance" = 0, "zip" = 0, "elev" = 0, "dew" = 0, "precip" = 0, "hum" = 0, "snow" = 0, "temp" = 0, "windspeed" = 0, "visibility" = 0, "windchill" = 0, "heatindex" = 0, "fog" = 0, "rain" = 0, "hail" = 0, "thunder" = 0, "tornado" = 0)
dfFebComplete[1:10,]

dfOctComplete = dfOctF
dfOctComplete = cbind(dfOctComplete, "distance" = 0, "zip" = 0, "elev" = 0, "dew" = 0, "precip" = 0, "hum" = 0, "snow" = 0, "temp" = 0, "windspeed" = 0, "visibility" = 0, "windchill" = 0, "heatindex" = 0, "fog" = 0, "rain" = 0, "hail" = 0, "thunder" = 0, "tornado" = 0)
dfOctComplete[1:10,]

dfNovComplete = dfNovF
dfNovComplete = cbind(dfNovComplete, "distance" = 0, "zip" = 0, "elev" = 0, "dew" = 0, "precip" = 0, "hum" = 0, "snow" = 0, "temp" = 0, "windspeed" = 0, "visibility" = 0, "windchill" = 0, "heatindex" = 0, "fog" = 0, "rain" = 0, "hail" = 0, "thunder" = 0, "tornado" = 0)
dfNovComplete[1:10,]

dfDecComplete = dfDecF
dfDecComplete = cbind(dfDecComplete, "distance" = 0, "zip" = 0, "elev" = 0, "dew" = 0, "precip" = 0, "hum" = 0, "snow" = 0, "temp" = 0, "windspeed" = 0, "visibility" = 0, "windchill" = 0, "heatindex" = 0, "fog" = 0, "rain" = 0, "hail" = 0, "thunder" = 0, "tornado" = 0)
dfDecComplete[1:10,]

#---------------------------------------------------------------

repeatR = function(df, lineStart, lineEnd) {
	
	for(lineCounter in lineStart:lineEnd) {
		if(lineCounter <= 1000) {
			#if(lineCounter %% 25 == 0)
				cat(lineCounter, "\n")
			rets = getOtherParams(df, lineCounter)
			df$distance[lineCounter] = rets[["dist"]]
			df$zip[lineCounter] = rets[["zip"]]
			df$elev[lineCounter] = rets[["elev"]]
			df$dew[lineCounter] = rets[["dew"]]
			df$precip[lineCounter] = rets[["precip"]]
			df$hum[lineCounter] = rets[["hum"]]
			df$snow[lineCounter] = rets[["snow"]]
			df$temp[lineCounter] = rets[["temp"]]
			df$windspeed[lineCounter] = rets[["windspeed"]]
			df$visibility[lineCounter] = rets[["visibility"]]
			df$windchill[lineCounter] = rets[["windchill"]]
			df$heatindex[lineCounter] = rets[["heatindex"]]
			df$fog[lineCounter] = rets[["fog"]]
			df$rain[lineCounter] = rets[["rain"]]
			df$hail[lineCounter] = rets[["hail"]]
			df$thunder[lineCounter] = rets[["thunder"]]
			df$tornado[lineCounter] = rets[["tornado"]]
			Sys.sleep(3)
		}
	}
	return(df)
}

#-----------------------------------------------Get the Map and Weather information
dfJanComplete = repeatR(dfJanComplete, 501, 1000)
dfJanComplete[1:10,]
save(dfJanComplete, file = "dfJanComplete.Rda")
load("dfJanComplete.Rda")

dfFebComplete = repeatR(dfFebComplete, 501, 1000)
dfFebComplete[990:1000,]
save(dfFebComplete, file = "dfFebComplete.Rda")
load("dfFebComplete.Rda")

#old March format old weather url
#dfMarComplete = dfMarFC
dfMarComplete = repeatR(dfMarComplete, 501, 1000)
dfMarComplete[95:1000,]
save(dfMarComplete, file="dfMarComplete.Rda")
load("dfMarComplete.Rda")

dfAprComplete = repeatR(dfAprComplete, 501, 1000)
save(dfAprComplete, file="dfAprComplete.Rda")
dfAprComplete[499:510,]
load("dfAprComplete.Rda")

dfMayComplete = repeatR(dfMayComplete, 501, 1000)
save(dfMayComplete, file="dfMayComplete.Rda")
dfMayComplete[499:510,]
load("dfMayComplete.Rda")

dfJunComplete = repeatR(dfJunComplete, 501, 1000)
save(dfJunComplete, file="dfJunComplete.Rda")
dfJunComplete[495:505,]
load("dfJunComplete.Rda")

dfJulComplete = repeatR(dfJulComplete, 501, 1000)
save(dfJulComplete, file="dfJulComplete.Rda")
dfJulComplete[198:202,]
load("dfJulComplete.Rda")

dfAugComplete = repeatR(dfAugComplete, 651, 1000)
save(dfAugComplete, file="dfAugComplete.Rda")
dfAugComplete[495:510,]
load("dfAugComplete.Rda")

dfSepComplete = repeatR(dfSepComplete, 601, 1000)
dfSepComplete[995:1000,]
save(dfSepComplete, file = "dfSepComplete.Rda")
load("dfSepComplete.Rda")

dfOctComplete = repeatR(dfOctComplete, 601, 1000)
save(dfOctComplete, file="dfOctComplete.Rda")
load("dfOctComplete.Rda")
dfOctComplete[480:500,]

dfNovComplete = repeatR(dfNovComplete, 901, 1000)
save(dfNovComplete, file="dfNovComplete.Rda")
load("dfNovComplete.Rda")
dfNovComplete[990:1000,]

dfDecComplete = repeatR(dfDecComplete, 751, 1000)
dfDecComplete[995:1000,]
save(dfDecComplete, file = "dfDecComplete.Rda")
load("dfDecComplete.Rda")

#---------------------------------------------Now, finally, calculate the speed

calcDistance = function(df) {
	df$distance = as.numeric(df$distance)
	df$elev = as.numeric(df$elev)
	df$dew = as.numeric(df$dew)
	df$precip = as.numeric(df$precip)
	df$hum = as.numeric(df$hum)
	df$snow = as.numeric(df$snow)
	df$temp = as.numeric(df$temp)
	df$windspeed = as.numeric(df$windspeed)
	df$visibility = as.numeric(df$visibility)
	df$windchill = as.numeric(df$windchill)
	df$heatindex = as.numeric(df$heatindex)
	df$fog = as.numeric(df$fog)
	df$rain = as.numeric(df$rain)
	df$hail = as.numeric(df$hail)
	df$thunder = as.numeric(df$thunder)
	df$tornado = as.numeric(df$tornado)

	
	dfFC = df
	dfFC = cbind(dfFC, "speed" = 0)
	for (i in 1:1000) {
		temp = dfFC$tripduration[i]/3600.0
		dfFC$speed[i] = dfFC$distance[i]/temp
	}
	return(dfFC)
}
#-----------------------------------------------------

dfJanFC = calcDistance(dfJanComplete)
dfJanFCC = calcDistance(dfJanComplete)
str(dfJanFCC)
dfJanFCC[990:1000,]
dfJanFC[485:505,]
save(dfJanFCC, file="dfJanFCC.Rda")
load("dfJanFC.Rda")

dfFebFC = calcDistance(dfFebComplete)
dfFebFCC = calcDistance(dfFebComplete)
save(dfFebFCC, file="dfFebFCC.Rda")
load("dfFebFC.Rda")
dfFebFCC[1:10,]
dfFebFCC[990:1000,]
str(dfFebFC)

#In March's case forgot to duplicate
dfMarFC = calcDistance(dfMarComplete)
save(dfMarFC, file="dfMarFC.Rda")
load("dfMarFC.Rda")
dfMarFC[1:10,]
dfMarFC[990:1000,]
cl = colnames(dfMarFC)
cl = cl[1:34]
cl
dfMarFC = dfMarFC[,cl]
median(dfMarFC$speed)
dfMarFCC=dfMarFC

dfAprFC = calcDistance(dfAprComplete)
dfAprFCC = calcDistance(dfAprComplete)
save(dfAprFC, file="dfAprFC.Rda")
save(dfAprFCC, file="dfAprFCC.Rda")
load("dfAprFC.Rda")
dfAprFC[1:50,]
dfAprFC[450:500,]

dfMayFC = calcDistance(dfMayComplete)
dfMayFCC = calcDistance(dfMayComplete)
save(dfMayFC, file="dfMayFC.Rda")
save(dfMayFCC, file="dfMayFCC.Rda")
load("dfMayFC.Rda")
dfMayFC[1:50,]
dfMayFC[450:500,]

dfJunFC = calcDistance(dfJunComplete)
dfJunFCC = calcDistance(dfJunComplete)
save(dfJunFC, file="dfJunFC.Rda")
save(dfJunFCC, file="dfJunFCC.Rda")
dfJunFC[495:505,]

dfJulFC = calcDistance(dfJulComplete)
dfJulFCC = calcDistance(dfJulComplete)
save(dfJulFC, file="dfJulFC.Rda")
save(dfJulFCC, file="dfJulFCC.Rda")

dfAugFC = calcDistance(dfAugComplete)
dfAugFCC = calcDistance(dfAugComplete)
save(dfAugFC, file="dfAugFC.Rda")
save(dfAugFCC, file="dfAugFCC.Rda")
dfAugFC[1:10,]

dfSepFC = calcDistance(dfSepComplete)
dfSepFCC = calcDistance(dfSepComplete)
save(dfSepFC, file="dfSepFC.Rda")
save(dfSepFCC, file="dfSepFCC.Rda")
load("dfAprFC.Rda")
dfSepFC[1:50,]

dfOctFC = calcDistance(dfOctComplete)
dfOctFCC = calcDistance(dfOctComplete)
save(dfOctFC, file="dfOctFC.Rda")
save(dfOctFCC, file="dfOctFCC.Rda")
load("dfOctFC.Rda")
dfOctFCC[995:1000,]

dfNovFC = calcDistance(dfNovComplete)
dfNovFCC = calcDistance(dfNovComplete)
save(dfNovFC, file="dfNovFC.Rda")
save(dfNovFCC, file = "dfNovFCC.Rda")
load("dfNovFC.Rda")
dfNovFCC[990:1000,]

dfDecFC = calcDistance(dfDecComplete)
dfDecFCC = calcDistance(dfDecComplete)
save(dfDecFC, file="dfDecFC.Rda")
save(dfDecFCC, file="dfDecFCC.Rda")
load("dfDecFC.Rda")
dfDecFC[1:50,]
dfDecFC[450:500,]
dfDecFCC[995:1000,]


median(dfJanFC$speed[1:500])
median(dfFebFC$speed[1:500])
median(dfMarFC$speed[1:500])
median(dfAprFC$speed[1:500])
median(dfMayFC$speed[1:500])
median(dfJunFC$speed[1:500])
median(dfJulFC$speed[1:500])
median(dfAugFC$speed[1:500])
median(dfSepFC$speed[1:500])
median(dfOctFC$speed[1:500])
median(dfNovFC$speed[1:500])
median(dfDecFC$speed[1:500])


str(dfDecFCC)
dfDecFCC = dfDecFCC[1:1000,]

aFCC = rbind(dfJanFCC, dfFebFCC, dfMarFCC, dfAprFCC, dfMayFCC, dfJunFCC, dfJulFCC, dfAugFCC, dfSepFCC, dfOctFCC, dfNovFCC, dfDecFCC)
str(aFCC)
save(aFCC, file="aFCC.Rda")
load("aFCC.Rda")

aFC = rbind(dfJanFC[1:500,], dfFebFC[1:500,], dfMarFC[1:500,], dfAprFC[1:500,], dfMayFC[1:500,], dfJunFC[1:500,], dfJulFC[1:500,], dfAugFC[1:500,], dfSepFC[1:500,], dfOctFC[1:500,], dfNovFC[1:500,], dfDecFC[1:500,])
aFC[1:10,]
str(aFC)
save(aFC, file="aFC.Rda")

median(aFCC$speed)
sd(aFCC$speed)
median(aFCC$temp)
sd(aFCC$temp)

plot(aFCC$speed)
plot(aFCC$speed, aFCC$temp)


remove_outliers = function(x) {
	thresholdh = quantile(x, 0.75) + 1.5*IQR(x)
	thresholdl = quantile(x, 0.25) - 1.5*IQR(x)
	filtered_x = ifelse(x > thresholdh, NA, x)
	filtered_x = ifelse(filtered_x < thresholdl, NA, filtered_x)
	return(filtered_x)
}

wfc = aFCC
wfc$temp =  remove_outliers(wfc$temp)
plot(wfc$temp)
wfc$speed = remove_outliers(wfc$speed)
plot(wfc$speed)
plot(wfc$temp, wfc$speed)
str(wfc)
theTemp = as.numeric(wfc$temp)
theSpeed = as.numeric(wfc$speed)


#There's an obvious huge outlier in one of these
feb, record no 27804
ppp=dfFebFC$speed<50
plot(dfAprFC$speed)
plot(dfFebFC$speed)
plot(dfJanFC$speed)
mean(dfJanFC$speed)
mean(dfFebFC$speed[ppp])
mean(dfAprFC$speed)


getOtherParams(dfJulFC, 101)


