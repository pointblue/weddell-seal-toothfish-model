# TODO: Add comment
# 
# Author: lsalas
###############################################################################


################################
#This function compiles the results of final simulations into discrete files by type of result and dive limit
# @param type A string telling the function which data.frame to aggregate. 
#  Options are: dive (total and average dive time per scenario, year and boot)
#        		result (total daily dive time, costs, tf/sf consumed, masses consumed, energy gains per scenario, year, day and boot)
#				wtgain (total weight and energy gained per scenario, year and boot)
compileSimulationData<-function(type="result"){
	pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Results/diving/final/"
	tbl.name<-paste(type,"data",sep=".")
	dirs<-c("50p","62.5p","80p")
	for(ddd in dirs){
		data.df<-data.frame()
		for(sss in 1:20){
			print(paste(ddd,sss))
			resdir<-paste(pth,ddd,"/set",sss,"/",sep="")
			files<-list.files(path=resdir,full.names=TRUE)
			for(fff in files){
				load(fff)
				df<-get(tbl.name)
				df$boot<-df$boot+(10*(sss-1))
				data.df<-rbind(data.df,df)
			}
		}
		save(data.df,file=paste(pth,ddd,"_",type,".RData",sep=""))
	}
	print("Done!")
}

#Function to further aggregate result data
subCompileResultData<-function(){
	pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Results/diving/final/"
	files<-list.files(path=pth,pattern="_result.RData",full.names=FALSE)
	fmls.df<-data.frame(fmls=c("tf.cons~pSucc+startTF+rateTF+cval+boot+year",
					"tf.mass~pSucc+startTF+rateTF+cval+boot+year",
					"sf.cons~pSucc+startTF+rateTF+cval+boot+year",
					"sf.weights~pSucc+startTF+rateTF+cval+boot+year",
					"e.dailyGained~pSucc+startTF+rateTF+cval+boot+year"),
			nams=c("TFcons","TFmass","SFcons","SFmass","enerGain"))
	funs<-c("sum","mean")
	
	for(fff in files){
		load(paste(pth,fff,sep=""))
		#here need to multiply boot by a counter!!!
		for(rrr in 1:nrow(fmls.df)){
			fml<-as.formula(as.character(fmls.df[rrr,"fmls"]))
			nam<-as.character(fmls.df[rrr,"nams"])
			for(sss in funs){
				print(paste(fff,nam,sss))
				agg.df<-aggregate(formula=fml,data=data.df,FUN=sss)
				filen<-paste(pth,"results/",(substr(fff,1,regexpr("_",fff,fixed=T)-1)),"_",sss,"_",nam,".RData",sep="")
				save(agg.df,file=filen)
			}
		}
	}
}

################################
#This function compiles the results of final simulations
# @param type A one-element string telling the function which data.frame to aggregate. 
#  Options are: dive (total and average dive time per scenario, year and boot)
#        		result (total daily dive time, costs, tf/sf consumed, masses consumed, energy gains per scenario, year, day and boot)
#				wtgain (total weight and energy gained per scenario, year and boot)
# @param diveLim A string of one or more (up to three) elements specifying the dive limit data to compile
#  Options are 50p, 62.5p and 80p
getSimulationData<-function(type="result",diveLim=c("50p","62.5p","80p")){
	pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Results/diving/final/"
	files<-character()
	for(ddd in diveLim){
		tmpfiles<-list.files(path=pth,full.names=TRUE,pattern=ddd)
		files<-c(files,tmpfiles)
	}
	files<-subset(files,grepl(type,files))
	
	df<-data.frame()
	for(fff in files){
		load(fff)
		df<-rbind(df,data.df)
	}
	return(df)
}

################################
#This function compiles the summarized results by dive limit, summarization function, and aggregated variable
# @param funct A one-element string telling the function which was used to aggregate the result data. 
#  Options are: sum (total per scenario, year and boot)
#        		mean (mean per scenario, year and boot)
# @param aggvar A one-element string naming the variable that was aggregated.
#  Options are: enerGain (the energy gained)
#				SFmass or SFcons (the mass or number of Silverfish consumed)
#				TFmass or TFcons (the mass or number of Toothfish consumed)
# @param diveLim A string of one or more (up to three) elements specifying the dive limit data to compile
#  Options are 50p, 62.5p and 80p
getSummarizedData<-function(funct="sum",aggvar,diveLim=c("50p","62.5p","80p")){
	pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Results/diving/final/results/"
	fls<-list.files(path=pth,pattern=funct,full.names=FALSE)
	fls<-subset(fls,grepl(aggvar,fls))
	df<-data.frame()
	for(ddd in diveLim){
		tfl<-subset(fls,grepl(ddd,fls))
		load(paste(pth,tfl,sep=""))
		agg.df$diveLim<-ddd
		df<-rbind(df,agg.df)
	}
	return(df)
}


getF6_elasticitiesData<-function(){
	source("c:/users/lsalas/workspace/PRBOGeneralUpdate/WESE/Single_TFconsumpt_BC_rel/ModelComponentsNew/DemogObjects/updateTrajectories.R")
	source("c:/users/lsalas/workspace/PRBOGeneralUpdate/WESE/Single_TFconsumpt_BC_rel/ModelComponentsNew/DemogObjects/Leslie.R")
	source("c:/users/lsalas/workspace/PRBOGeneralUpdate/WESE/Single_TFconsumpt_BC_rel/ModelComponentsNew/DemogObjects/BodyConditionHistory.R")
	source("c:/users/lsalas/workspace/PRBOGeneralUpdate/WESE/Single_TFconsumpt_BC_rel/ModelComponentsNew/DemogModels/fitDemogModelsNew.R")
	
	## Visualize lambdas and pop numbers
#v3_80p is higher dive limits (=80% time diving); alternatively...
#v3_90d represents the 90 dives/day limit
#v3_50p is the lowest dive limits, 50% time diving
	
	pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Results/popDy_trajectories/v3_80p_bc_w/" 	#v3_80p
	files<-list.files(path = pth, pattern = ".RData", full.names = FALSE)
	
	#### Hi diving limit
	lh.df<-data.frame()
	for(file in files){
		filepth<-paste(pth,file,sep="")
		load(filepth)
		depletion<-substr(file,1,regexpr("_",file,fixed=T)[1]-1); depletion<-ifelse(depletion=="low","Lo",ifelse(depletion=="med","Md","Hi"))
		startAbund<-substr(file,regexpr("start",file,fixed=T)[1]+5,regexpr("start",file,fixed=T)[1]+7)
		pRecov<-substr(file,regexpr("recov",file,fixed=T)[1]+5,regexpr("recov",file,fixed=T)[1]+6);pRecov<-paste(pRecov,"%",sep="")
		pSucc<-substr(file,regexpr("pS",file,fixed=T)[1]+2,regexpr("pS",file,fixed=T)[1]+5);pSucc<-ifelse(pSucc=="0.25","25%","35%")
		trajectories<-res[["trajectories"]]
		dd<-getAbundanceTrendData(traject=trajectories)
		maxTime<-max(dd$time)
		g<-subset(dd,time==maxTime)
		lambdas<-numeric()
		for(tt in 1:max(g$Traject)){
			tenddf<-subset(g, Traject==tt)
			tendN<-as.numeric(tenddf$totals)
			tlambda<-exp((log(tendN/1000))/maxTime)
			lambdas<-c(lambdas,tlambda)
		}
		lambdas<-subset(lambdas,lambdas>0)
		mlambda<-mean(lambdas);sdlambda<-sd(lambdas)
		endN<-mean(g$totals)
		sdEndN<-sd(g$totals)
		lambda<-exp((log(endN/1000))/maxTime)
		bcho<-res[["bodyconditionhistory"]]
		bc<-t(as.data.frame(HistoryBCH(bcho))); row.names(bc)<-NULL
		stBC<-bc[1,1];edBC<-bc[(nrow(bc)),1]
		tmp<-data.frame(depletion=depletion,startAbund=startAbund,pRecov=pRecov,pSucc=pSucc,lambda=lambda,mlambda,sdlambda,endN=endN,sdEndN=sdEndN,stBC=stBC,edBC=edBC,maxTime=maxTime)
		lh.df<-rbind(lh.df,tmp)
	}
	lh.df$diveLimit<-"80% of day"
	
	#### Medium diving limit
	pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Results/popDy_trajectories/v3_625p_bc_w/" 	#v3_90d
	files<-list.files(path = pth, pattern = ".RData", full.names = FALSE)
	lm.df<-data.frame()
	for(file in files){
		filepth<-paste(pth,file,sep="")
		load(filepth)
		depletion<-substr(file,1,regexpr("_",file,fixed=T)[1]-1); depletion<-ifelse(depletion=="low","Lo",ifelse(depletion=="med","Md","Hi"))
		startAbund<-substr(file,regexpr("start",file,fixed=T)[1]+5,regexpr("start",file,fixed=T)[1]+7)
		pRecov<-substr(file,regexpr("recov",file,fixed=T)[1]+5,regexpr("recov",file,fixed=T)[1]+6);pRecov<-paste(pRecov,"%",sep="")
		pSucc<-substr(file,regexpr("pS",file,fixed=T)[1]+2,regexpr("pS",file,fixed=T)[1]+5);pSucc<-ifelse(pSucc=="0.25","25%","35%")
		trajectories<-res[["trajectories"]]
		dd<-getAbundanceTrendData(traject=trajectories)
		maxTime<-max(dd$time)
		g<-subset(dd,time==maxTime)
		lambdas<-numeric()
		for(tt in 1:max(g$Traject)){
			tenddf<-subset(g, Traject==tt)
			tendN<-as.numeric(tenddf$totals)
			tlambda<-exp((log(tendN/1000))/maxTime)
			lambdas<-c(lambdas,tlambda)
		}
		lambdas<-subset(lambdas,lambdas>0)
		mlambda<-mean(lambdas);sdlambda<-sd(lambdas)
		endN<-mean(g$totals)
		sdEndN<-sd(g$totals)
		lambda<-exp((log(endN/1000))/maxTime)
		bcho<-res[["bodyconditionhistory"]]
		bc<-t(as.data.frame(HistoryBCH(bcho))); row.names(bc)<-NULL
		stBC<-bc[1,1];edBC<-bc[(nrow(bc)),1]
		tmp<-data.frame(depletion=depletion,startAbund=startAbund,pRecov=pRecov,pSucc=pSucc,lambda=lambda,mlambda,sdlambda,endN=endN,sdEndN=sdEndN,stBC=stBC,edBC=edBC,maxTime=maxTime)
		lm.df<-rbind(lm.df,tmp)
	}
	lm.df$diveLimit<-"62.5% of day"
	
	#### Low diving limit
	pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Results/popDy_trajectories/v3_50p_bc_w/" 	#v3_50p
	files<-list.files(path = pth, pattern = ".RData", full.names = FALSE)
	ll.df<-data.frame()
	for(file in files){
		filepth<-paste(pth,file,sep="")
		load(filepth)
		depletion<-substr(file,1,regexpr("_",file,fixed=T)[1]-1); depletion<-ifelse(depletion=="low","Lo",ifelse(depletion=="med","Md","Hi"))
		startAbund<-substr(file,regexpr("start",file,fixed=T)[1]+5,regexpr("start",file,fixed=T)[1]+7)
		pRecov<-substr(file,regexpr("recov",file,fixed=T)[1]+5,regexpr("recov",file,fixed=T)[1]+6);pRecov<-paste(pRecov,"%",sep="")
		pSucc<-substr(file,regexpr("pS",file,fixed=T)[1]+2,regexpr("pS",file,fixed=T)[1]+5);pSucc<-ifelse(pSucc=="0.25","25%","35%")
		trajectories<-res[["trajectories"]]
		dd<-getAbundanceTrendData(traject=trajectories)
		maxTime<-max(dd$time)
		g<-subset(dd,time==maxTime)
		lambdas<-numeric()
		for(tt in 1:max(g$Traject)){
			tenddf<-subset(g, Traject==tt)
			tendN<-as.numeric(tenddf$totals)
			tlambda<-exp((log(tendN/1000))/maxTime)
			lambdas<-c(lambdas,tlambda)
		}
		lambdas<-subset(lambdas,lambdas>0)
		mlambda<-mean(lambdas);sdlambda<-sd(lambdas)
		endN<-mean(g$totals)
		sdEndN<-sd(g$totals)
		lambda<-exp((log(endN/1000))/maxTime)
		bcho<-res[["bodyconditionhistory"]]
		bc<-t(as.data.frame(HistoryBCH(bcho))); row.names(bc)<-NULL
		stBC<-bc[1,1];edBC<-bc[(nrow(bc)),1]
		tmp<-data.frame(depletion=depletion,startAbund=startAbund,pRecov=pRecov,pSucc=pSucc,lambda=lambda,mlambda,sdlambda,endN=endN,sdEndN=sdEndN,stBC=stBC,edBC=edBC,maxTime=maxTime)
		ll.df<-rbind(ll.df,tmp)
	}
	ll.df$diveLimit<-"50% of day"
	
	lambda.df<-rbind(lh.df,lm.df,ll.df)
	lambda.df$lclambda<-lambda.df$mlambda-(1.96*lambda.df$sdlambda)
	lambda.df$uclambda<-lambda.df$mlambda+(1.96*lambda.df$sdlambda)
	lambda.df$deplRate<-ifelse(lambda.df$depletion=="Hi","75%",ifelse(lambda.df$depletion=="Lo","25%","50%"))
	
	return(lambda.df)
}