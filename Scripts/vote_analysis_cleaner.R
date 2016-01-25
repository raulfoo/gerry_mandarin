#for gerrymander
vote_analysis = function(nextStepID,homeDir){
	library(foreign)
	library(rgdal)
	library(rgeos)
	library(maptools)
	library(geosphere)
	library(RColorBrewer)
	
	setwd(homeDir)
	jobFile= nextStepID
	#jobFile = commandArgs(trailingOnly=T)[1]
	inputFile = read.csv(jobFile,stringsAsFactors=F)
	
	#have subsetting/ordering options from the master list here, for example probably want to make the true boundaries always appear first
	
	voteTypes = list()  #keep this stuff for now to support brining in the random assignment trials
	#voteTypes[["2008 President"]] = rbind(c("Republican","EL08G_SP_R","EL08G_PR_R"),c("Democrat","EL08G_SP_D","EL08G_PR_D"))
	voteTypes[[inputFile[1,"Election.Type"]]] = rbind(c("Republican","RepVote"),c("Democrat","DemVote"))
	
	#voteTypes[["2010 Senate"]]  = rbind(c("Republican","RepVote"),c("Democrat","DemVote"))
	#voteTypes[["2012 SenateAgain"]]  =  rbind(c("Republican","RepVote"),c("Democrat","DemVote"))
	
	outputList = list()
	fullOut = list()
	
	for(voteTypeIndex in 1:nrow(inputFile)){
	
		#voteTypeSelect = names(voteTypes)[voteTypeIndex]
		voteTypeSelect = inputFile[voteTypeIndex,"Election.Type"]
		output = c()
		#outputList = list()
		districtIDCol = inputFile[voteTypeIndex,"District.ID.Column"]
		demCol = inputFile[voteTypeIndex,"Democrat.Vote.Column"]
		repCol = inputFile[voteTypeIndex,"Republican.Vote.Column"]
		electionType = inputFile[voteTypeIndex,"Election.Type"]
		
		voteFileName = paste("Output/toGraph/",inputFile[voteTypeIndex,1],".dbf",sep="")
		voteDat = read.dbf(voteFileName,as.is=T)
	
		splitRound = rep("L",times=nrow(voteDat))
	
		for(splitLineID in unique(voteDat[,districtIDCol])){
			tempDat = voteDat[which(voteDat[,districtIDCol]==splitLineID),]
			republican = sum(as.numeric(tempDat[,repCol]))
			democrat = sum(as.numeric(tempDat[,demCol]))
	
			demPercent = democrat/(republican+democrat)
			if(demPercent>.5){
				winner="D"
			}else{
				winner="R"
			}
			splitRound[which(voteDat [,districtIDCol]==splitLineID)] = winner
			output = rbind(output,c(splitLineID,republican,democrat,demPercent,winner))
		}
		if(is.null(fullOut[[electionType ]])){
		fullOut[[electionType ]] = list()
		
		}
		fullOut[[electionType]][[length(fullOut[[electionType]])+1]] = output
		
	}
	
	#loop over fullOut and populate this peice
	for(i in 1:length(fullOut)){
		addNextArray = do.call("rbind",lapply(fullOut[[i]],function(x) c(length(which(x[,5]=="R")),length(which(x[,5]=="D")),(length(which(x[,5]=="D"))/(nrow(x))))))
		#inputFile[which(inputFile[,2]==names(fullOut)[i]),1]
		
		rownames(addNextArray) = inputFile[which(inputFile[,"Election.Type"]==names(fullOut)[i]),"Boundary.Type"]
		#rownames(addNextArray) = unlist(lapply(strsplit(inputFile[which(inputFile[,2]==names(fullOut)[i]),1],"_x_"),function(x) x[1]))
		
		fullOut[[i]][[length(fullOut[[i]])+1]] = addNextArray
		
		totalDem08= sum(as.numeric(fullOut[[i]][[1]][,3]))
		totalRep08 = sum(as.numeric(fullOut[[i]][[1]][,2]))
		
		fullOut[[i]][[length(fullOut[[i]])+1]] = totalDem08/(totalDem08+totalRep08)
		
	}
	
	#check this!!
	#also this logic only works if there are equal numbers of districting option in the input files for each race...not necessarily ideal flexbility
	graphing = apply(sapply(fullOut,function(x) x[[length(x)-1]][,3]),2,rev)
	
	outputDirectory = paste("Output/outputPNG/",inputFile[1,1],sep="")
	dir.create(outputDirectory)
	
	write.csv(inputFile,paste(outputDirectory,"/manifest.csv",sep=""),row.names=F)
	write.csv(graphing,paste(outputDirectory,"/graphData.csv",sep=""),row.names=F)
	
	#add randomize trial graphing
	#ordering options here?
	
	cols = brewer.pal(nrow(graphing),"PuOr")
	png(filename=paste(outputDirectory,"/mainAnalysis.png",sep=""),width=800,height=800)
	par(mar=c(3,5,3,5))
	df.bar = barplot(graphing[,1:ncol(graphing)],names.arg=colnames(graphing),beside=T,col=cols ,ylim=c(0,1),ylab="% votes won by Democrats Statewide")
	
	legend("topright",legend=rownames(graphing),col=cols ,pch=19)
	lines(c(0,(dim(graphing)[1]+1)*dim(graphing)[2]),c(.5,.5),lty=3)
	for(selectBar in 1:length(fullOut)){
		xRange = c(min(df.bar[,selectBar])-.5,max(df.bar[,selectBar])+.5)
		lines(x=xRange,y=rep(fullOut[[selectBar]][[length(fullOut[[selectBar]])]],times=2),col="blue",lwd=4,lend="square")
		print(seq(min(xRange)+.5,max(xRange),by=1)+.5)
		seatsWon = rev(unlist(lapply(fullOut[[selectBar]][1:nrow(df.bar)],function(x) length(which(x[,5]=="D")))))
		text(seq(min(xRange)+.5,max(xRange),by=1)+.75,graphing[,selectBar]+.02,paste(seatsWon," seats",sep=""),2)
		#text(df.bar[2,selectBar],.95,paste("Statewide Democratic Vote %:",round(fullOut[[selectBar]][[5]],2),sep=""),cex=.6)
	}
	axis(side=4,at=seq(0,1,length.out=1+nrow(fullOut[[1]][[1]])),labels = c(0:nrow(fullOut[[1]][[1]])))
	mtext("Simulated House Seats won by Democrat",side=4,line=3)
	dev.off()
	
	
	#ordering options here?
	
	
	rawInput = inputFile
	inputFile = inputFile[duplicated(inputFile[,"Boundary.Type"])==F,]
	#support load from inputFile here
	
	geometricScoreTrack = c()
	for(i in 1:nrow(inputFile)){
		#if(i > 1) next  #skip defaults by none, assumes only one coming from user
		shape = readOGR(dsn="Output/toGraph/",layer=inputFile[i,1])
		shape = gBuffer(shape,width=0,byid=T)
		shapeMerged = unionSpatialPolygons(shape,shape @data[,inputFile[i,"District.ID.Column"]])
		
		colorRamp = c(brewer.pal(12,"Set3"),"black") #ensure this actual matches number of districts in state, hardcoded to 13 for NC now
		colorRamp = rep(colorRamp,ceiling(length(shapeMerged)/length(colorRamp)))[1:length(shapeMerged)]
	
	
		png(filename=paste(outputDirectory,"/boundary_map-",inputFile[i,"Boundary.Type"],".png",sep=""))#,width=800,height=800)
		plot(shapeMerged,main=inputFile[i,2],col=colorRamp )
		dev.off()
		
		
			
		locationSelect = gCentroid(shapeMerged)@coords
		UTMZone_number = ceiling((180+locationSelect[1])/6)
		if(locationSelect[2]>0) UTMHemi = "north"
		if(locationSelect[2]<0) UTMHemi = "south"
		
		shapeMergedUTM= spTransform(shapeMerged,CRS(paste("+proj=utm +zone=",UTMZone_number," +",UTMHemi," +ellps=WGS84 +datum=WGS84 +units=m +no_defs",sep="")))
		
		geometricScore =gArea(shapeMergedUTM,byid=T)/perimeter(shapeMerged)^2	
		geometricScoreTrack = cbind(geometricScoreTrack,geometricScore)
		colnames(geometricScoreTrack) = inputFile[1:i,1]
		
	}
	
	
	plotDat = geometricScoreTrack
	totalSum = apply(plotDat,2,sum)
	png(paste(outputDirectory,"/geometricScores.png",sep=""),width=800,height=400)
	par(mar=c(4,10,2,2))
	plot(plotDat,type="n",xlim=c(0,max(totalSum)*1.4),ylim=c(.25,ncol(plotDat)+.5),ylab="",xlab="",axes=F)
	#names=c("Split Line Districts","Random Cont. Assignment","111th Congress Districts","113th Congress Districts"),horiz=T,
	for(j in 1:ncol(plotDat)){
		
		thisColors = colorRamp[order(plotDat[,j])]
		tempPlot = cumsum(sort(plotDat[,j]))
		for(k in 1:length(tempPlot)){
			if(k == 1){
				startX = 0
			}else{
				startX = tempPlot[k-1]
			}
			stopX = tempPlot[k]
			topY = j+.25
			botY = j-.25
			rect(startX,botY,stopX,topY,col=thisColors[k])
			
		}
	}
	
	text(totalSum,1:length(totalSum),round(totalSum,6),pos=4,outer=T)
	box()
	axis(2,labels=c(inputFile[,"Boundary.Type"]),at=c(1:nrow(inputFile)),las=2)
	axis(1,round(seq(0,max(totalSum)*1.4,length.out=5),5))
	#may need to draw these as shapes
	dev.off()
}



#for traditional measurement


#for mapping, need to create an nc boundingboxplot of proper color for a merge 
