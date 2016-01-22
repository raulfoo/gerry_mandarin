run_analysis = function(uniqueJobID,homeDir){
	setwd(homeDir)
	library(rgdal)
	library(rgeos)
	library(foreign)
	library(rjson)

	
	masterCSV = c()
	
	#uniqueJobID = "c86b331663de4bae96808146b225ab1d"
	#jobManifest = commandArgs(trailingOnly=T)[1]
	jobManifest = fromJSON(file=paste(homeDir,"/Jobs/Pending/",uniqueJobID,".json",sep=""))
	
	allBoundaryLayers = c(jobManifest$Boundary$fileName,"111th_Congress_Ohio")#,"113th_Congress_Ohio")
	elections = rbind(c( jobManifest$Data$democraticVotes,jobManifest$Data$republicanVotes))
	rownames(elections) = jobManifest$Data$displayName
	# a bit of a hack for now for bulk process
	allOutputNames = c()
	for(boundaryLayerName in allBoundaryLayers){
		#boundaryLayerName = "ProgrammerWunderkind"
		voteLayerName = jobManifest$Data$fileName
		
		outputName = paste(boundaryLayerName,voteLayerName,sep="_x_")
		
		boundary = readOGR(dsn="Input",layer=boundaryLayerName)
		precincts = readOGR(dsn="Input",layer=voteLayerName)
		
		if(proj4string(boundary)!="+proj=longlat +datum=WGS84") boundary = spTransform(boundary,CRS("+proj=longlat +datum=WGS84"))
		if(proj4string(precincts)!="+proj=longlat +datum=WGS84") precincts = spTransform(precincts,CRS("+proj=longlat +datum=WGS84"))
		
		if(boundaryLayerName == "113th_Congress_Ohio" | boundaryLayerName == "111th_Congress_Ohio" ){  #reduce data if passing a TIGER file complete US distrciting shapefile
			stateID = boundary@data[which(gIntersects(precincts[1,],boundary,byid=T)==T),"STATEFP"]
			boundary = boundary[which(boundary@data[,"STATEFP"]==stateID),]
			boundaryNameAlias = boundaryLayerName 
		}else{
			boundaryNameAlias = jobManifest$Boundary$displayName
		}
		
		precincts = gBuffer(precincts,width=0,byid=T)
		boundary= gBuffer(boundary,width=0,byid=T)
		proj4string(precincts) = proj4string(boundary)  #should just ensure these are always correct for the precincts (that will be my data)
		
		dummys = cbind(precincts@data,id=0)
					
		missedBoundaries = c()
		for(k in 1:length(precincts)){
			matches = gIntersects(gBuffer(precincts[k,],width=0),boundary,byid=T)
			matches = which(matches==T)
			if(length(matches)>1){
				compareAreas = c()
				for(testIndex in matches){
					test = gArea(gIntersection(gBuffer(precincts[k,],width=0),gBuffer(boundary[testIndex,],width=0),byid=T))
					compareAreas = c(compareAreas,test)
				}
				matches = matches[which.max(compareAreas)[1]]
				dummys[k,"id"] = matches
				
			}else if(length(matches)<1){
				missedBoundaries = c(missedBoundaries,k)
			}else{
				dummys[k,"id"] = matches
	
			}
			if(k %% 100 ==0) print(k) 
		}
		
		allCenters = gCentroid(precincts,byid=T)
		print(paste("Missed: ",length(missedBoundaries)))
		if(length(missedBoundaries)){
			for(k in 1:length(missedBoundaries)){
				temp = cbind(1:length(precincts),gDistance(precincts[missedBoundaries[k],],allCenters,byid=T))
				temp = temp[which(dummys[,"id"]!=0),]
				if(nrow(temp)<1) stop("Error")
				idValue = dummys[temp[which.min(temp[,2])[1],1],"id"]
				dummys[missedBoundaries[k],"id"] = idValue
				#find distance to all other precints
				if(k %% 25 ==0) print(k) 
				
			}
		}
		
		newSplit = precincts
		newSplit@data = dummys
		#print(paste(1:ncol(newSplit@data),colnames(newSplit@data),sep=":"))
		#print("Enter index for column representing Democratic Vote: ")  #and a check to make their number makes sense
		#demColIndex = readLines(con="stdin",n = 1)
		#print("Enter index for column representing Republican Vote: ")
		#repColIndex = readLines(con="stdin",n = 1)
		
		newSplitRaw = newSplit
		for(k in 1:nrow(elections)){
			demColIndex = elections[k,1]
			repColIndex = elections[k,2]
			#outputNameFull = paste(outputName,"_",rownames(elections)[k],sep="")
			outputNameFull = outputName
			
			allOutputNames = c(allOutputNames,outputNameFull)
			newSplit@data = newSplitRaw@data[,c(demColIndex,repColIndex,colnames(newSplitRaw@data)[ncol(newSplitRaw@data)])]
			colnames(newSplit@data) = c("DemVote","RepVote","DistID")
			
			writeOGR(newSplit,dsn="Output/toGraph/",layer=outputNameFull,driver="ESRI Shapefile",overwrite=T)
			masterCSV = rbind(masterCSV,c(outputNameFull,boundaryNameAlias,rownames(elections)[k],"DemVote","RepVote","DistID"))
		}
		#need to make output data same format as nc_vote_splitline_intersect
	}
	colnames(masterCSV) = c("Shapefile Name","Boundary Type","Election Type","Democrat Vote Column","Republican Vote Column","District ID Column")
	
	nextStepID = paste("Jobs/graphManifests/masterOut_",as.numeric(Sys.time()),".csv",sep="")
	write.csv(masterCSV,paste(homeDir,"/",nextStepID,sep=""),row.names=F)
	
	#buildGraphs = tryCatch(vote_analysis(nextStepID),error = function(e) e)
	vote_analysis(nextStepID,homeDir)
	#system(paste("Rscript public/R/Scripts/vote_analysis_cleaner.R '",nextStepID,"'",sep=""))
	
	
	allInputs = list.files("Input")
	removeBoundary  = grep(paste("^",jobManifest$Boundary$fileName,sep=""),allInputs)
	removeData  = grep(paste("^",jobManifest$Data$fileName,sep=""),allInputs)
	
	#unlink(paste("Input/",allInputs[removeBoundary],sep=""))
	#unlink(paste("Input/",allInputs[removeData],sep=""))
	
	allGraphs = list.files("Output/toGraph")
	for(eachOutputName in allOutputNames){
		removeData  = grep(paste("^",eachOutputName,sep=""),allGraphs)
		unlink(paste("Output/toGraph/",allGraphs[removeData],sep=""))
	}
	
	#need to allow for errors, probably just use a function for vote_analysis_cleaner
	#if no error

	setwd(homeDir)
	#upon succefull completion would be good to delete the dataSJON/geoJSON/topoJSON associated with this job
}