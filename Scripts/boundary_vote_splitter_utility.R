####Edit your working directory as appropriate for you workstation ###
homeDir = "~/Desktop/gerry_mandarin"  #dont end this with a slash
setwd(homeDir)

startingCronTaskDir = getwd()
list.of.packages = c("rjson","rgdal","rgeos","foreign","rgeos","RColorBrewer","maptools","geosphere")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages,repos="http://cran.rstudio.com/")
library(rjson)

source("Scripts/vote_analysis_cleaner.R")
source("Scripts/prepare_shapes_for_analysis.R")

#and then run the next function

allJobs = list.files("Jobs/Pending")
for(uniqueJobID in allJobs){
	
	uniqueJobID = gsub(".json$","",uniqueJobID)
	eachAnalysis = tryCatch(run_analysis(uniqueJobID,homeDir),error = function(e) e)
	setwd(homeDir)
	if(inherits(eachAnalysis,"error")){
		#send me an email
		jobManifest = fromJSON(file=paste("Jobs/Pending/",uniqueJobID,".json",sep=""))
		jobManifest[["Error"]] = as.character(eachAnalysis)
		writeLines(toJSON(jobManifest),paste("Jobs/Error/",uniqueJobID,".json",sep=""))
		unlink(paste("Jobs/Pending/",uniqueJobID,".json",sep=""))

		errorText = paste("cron task main call script had an error, blocking future tasks until resolved: ",Sys.time(),"\n",eachAnalysis,sep="")
	    errorSubject = "GerryMander Cron Task Error"
		#sendEmailError( errorText,errorSubject) 
		
	}else{
		jobManifest =  fromJSON(file=paste("Jobs/Pending/",uniqueJobID,".json",sep=""))
		for(intermediateData in c("dataJSON","geoJSON","topoJSON")){
			unlink(paste("Output/",intermediateData,"/",jobManifest$Data$fileName,".json",sep=""))
			unlink(paste("Output/",intermediateData,"/",jobManifest$Boundary$fileName,".json",sep=""))
		}
		file.rename(paste("Jobs/Pending/",uniqueJobID,".json",sep=""),paste("Jobs/Complete/",uniqueJobID,".json",sep=""))
		#send them an email (if provided)	
	}
}


#ignore for demonstration	
if(F){	
	allOutput = list.files("Jobs/Complete")
	database = c()
	states = readOGR(dsn="stateBoundary10",layer="tl_rd13_us_state10")
	for(eachOutput in allOutput){
		temp = fromJSON(file=paste("Jobs/Complete/",eachOutput,sep=""))
		checkThis = readOGR(dsn="Output/toGraph/",layer=paste(temp$Boundary$fileName,"_x_",temp$Data$fileName,sep=""))
		stateSelect = which(gIntersects(checkThis[1,],states,byid=T)==T)[1]
		if(length(stateSelect)<1){
			stateName = "Unavailable"
		}else{
			stateName = as.character(states@data[which(gIntersects(checkThis[1,],states,byid=T)==T)[1],"NAME10"])
		}
			
		tempOut = toupper(c(gsub(".json$","",eachOutput),stateName,temp$Boundary$displayName,temp$Data$displayName,paste(temp$Boundary$fileName,"_x_",temp$Data$fileName,"/mainAnalysis.png",sep="")))
		database = rbind(database,tempOut)
	}
		
	colnames(database) = c("id","State","Boundary","Data","Img")
	write.csv(database,"database.csv",row.names=F)
}
