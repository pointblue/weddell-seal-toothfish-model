# TODO: Add comment
# 
# Author: lsalas
###############################################################################

################# Instantiate the FishTrend object #########################
## This function instantiates the FishTrend object 
#' @param startTF	A numeric value specifying the starting toothfish abundance for which a fish trend object will be initialized (0.4 or 0.3)
#' @param rateTF	A numeric value specifying the rate of toothfish depletion for which a fish trend object will be initialized (-0.0082, -00198, -0.0396)
#' @param time.int	The length of the simulation (years)
#' @param pth		A string with the path to the result files (summarized)
#' @param filen		A vector of file names with different summaries
#' @param meta.df	A data frame with metadata on toothfish abundance based on starting abundance and rate of depletion, and time interval
#' @param cval		A numeric value specifying the predation release level for which a fish trend object will be initialized
#' @param pSucc		A numeric value specifying the probability of diving success for which a fish trend object will be initialized
#' @param pthWeights	The path to the file with the appropriate summary of weight gains from the dive simulations
#' @param ... 		Other parameter specifications to be passed to functions called by this function
getFishTrendObject<-function(startTF=0.4,rateTF=-0.0396,time.int=35,pth,filen,meta.df,cval,pSucc,pthWeights,...){
	fto<-new("FishTrend")
	tfTrendModel<-getTFtrendModel(startTF=startTF,rateTF=rateTF,time.int=time.int)	
	tfmodel<-getAbundTFmodel(what="masses",pth=pth,filen=filen,meta.df=meta.df,rtTF=rateTF,stTF=startTF,cv=cval,pS=pSucc)
	sfmodel<-getAbundSFmodel(what="masses",pth=pth,filen=filen,meta.df=meta.df,rtTF=rateTF,stTF=startTF,cv=cval,pS=pSucc)
	tftowtmodel<-getWeightGainModel(pth=pthWeights,meta.df=meta.df,rtTF=rateTF,stTF=startTF,cv=cval,pS=pSucc)
	
	tftv<-predict(tfTrendModel,newdata=data.frame(year=1));tftv<-as.numeric(tftv)[1]
	ctf<-predict(tfmodel,newdata=data.frame(TFabund=tftv))
	csf<-predict(sfmodel,newdata=data.frame(TFabund=tftv)) #note how abundance of sf is dependent on abundance of tf
	cwt<-predict(tftowtmodel,newdata=data.frame(TFabund=tftv))
	
	CurrentTF(fto)<-as.numeric(ctf)[1]
	CurrentSF(fto)<-as.numeric(csf)[1]
	CurrentWeight(fto)<-as.numeric(cwt)[1]
	Timestep(fto)<-as.integer(1)
	
	TFabundance(fto)<-list(tfTrendModel=tfTrendModel)
	Toothfish(fto)<-list(tfmodel=tfmodel)
	Silverfish(fto)<-list(sfmodel=sfmodel)
	TFtoWeight(fto)<-list(tftowtmodel=tftowtmodel)
	
	return(fto)
}
	

################# Instantiate the BodyConditionhistory object #########################
## This function instantiates the BodyConditionhistory object
#' @param cwt			A numeric value of the current mean seal weight
#' @param wtStdev		A numeric value with the standard deviation of current seal weight expressed as a percent of the weight
#' @param bc_w_scale	A numeric value specifying the value of the scale parameter of the Weibull function of weight to body-condition. Use 0.62 for 60% target mass recovery, 0.75 (default) for 70% target mass recovery, and 0.83 for 80% target mass recovery
#' @param ...			Other parameter specifications to be passed to functions called by this function
getBodyConditionHistoryObject<-function(cwt,wtStdev,bc_w_scale,...){
	bcho<-new("BodyConditionHistory")
	bcwtmodel<-getWeightToBCModel(scale=bc_w_scale, ...)	#defaults: maxVal=1.1,scale=0.75,shape=3
	bcpred<-predict(bcwtmodel,newdata=data.frame(weightGain=c(cwt,(cwt*wtStdev))))
	CurrentBCH(bcho)<-c(bcpred[1],abs(bcpred[2]-bcpred[1]))
	WeightToBC(bcho)<-list("bcwtmodel"=bcwtmodel)
	histv<-list();histv[[1]]<-c(bcpred[1],bcpred[2]-bcpred[1])
	HistoryBCH(bcho)<-histv
	return(bcho)
}

#####################################################################################################################
## This function generates the initial values for the low-level demographic objects. It instantiates the needed objects and populates with initial values and models
#' @param sizeLeslie	A numeric vector specifying the number of rows (first component) and number of columns of the Leslie
#' @param start.abund	A numeric value with the initial number of seals in each population (defaults to 1,000 seals)
#' @param bcmean		A numeric value of the mean of the distribution of body conditions
#' @param bcse			A numeric value of the standard deviation of the distribution of body conditions
#' @param n.traject		A numeric value specifying the number of desired population trajkectories to simulate
getInitialWESEdemogValues<-function(sizeLeslie,start.abund=1000,bcmean, bcse,n.traject){
	traject<-list()		# A list of a list - each list in the list is a list of demographic objects
	for(ii in 1:n.traject){
		bco<-new("BodyCondition")
		newBodyCond<-rnorm(1,mean=bcmean,sd=bcse)
		HistoryBC(bco)<-newBodyCond
		CurrentBC(bco)<-newBodyCond
		#need the scale model
		scmdl<-getScaleParamModel()
		ScaleModel(bco)<-list(model=scmdl)
		csc<-predict(scmdl,newdata=data.frame(bodyCond=newBodyCond))
		CurrentSc(bco)<-csc[1]
		HistorySc(bco)<-csc[1]
				
		prp<-new("Propensity")
		mdls<-makePropenParamModelList()
		nr<-length(mdls);nc<-length(mdls[[1]])
		if(nr!=sizeLeslie[1] | nc!=sizeLeslie[2])stop("The length of the propensity models list does not result in a matrix of the same size as the specified Leslie")
		tmpPredict<-numeric()
		for(rr in 1:nr){
			for(cc in 1:nc){
				mdl<-mdls[[rr]][[cc]]
				if(class(mdl)[1]=="numeric"){
					res<-mdl
				}else{
					res<-predict(mdl,newdata=data.frame(bodyCond=newBodyCond),se.fit=FALSE)	#careful here to ensure we are getting a vector
				}
				tmpPredict<-c(tmpPredict,res)
			}
		}
		newPropensity<-try(matrix(tmpPredict,nrow=nr,ncol=nc,byrow=TRUE),silent=TRUE)
		ModelsProp(prp)<-mdls
		CurrentProp(prp)<-newPropensity
		histlst<-list();histlst[[1]]<-newPropensity
		HistoryProp(prp)<-histlst
		
		prd<-new("Productivity")
		mdls<-makeProductivityParamModelList(nc)
		nr<-length(mdls);nc<-length(mdls[[1]])
		if(nr!=sizeLeslie[1] | nc!=sizeLeslie[2])stop("The length of the productivity models list does not result in a matrix of the same size as the specified Leslie")
		tmpPredict<-numeric()
		for(rr in 1:nr){
			for(cc in 1:nc){
				mdl<-mdls[[rr]][[cc]]
				if(class(mdl)[1]=="numeric"){
					res<-mdl
				}else{
					res<-predict(mdl,newdata=data.frame(bodyCond=newBodyCond),se.fit=FALSE)	#careful here to ensure we are getting a vector
				}
				tmpPredict<-c(tmpPredict,res)
			}
		}
		newProductivity<-try(matrix(tmpPredict,nrow=nr,ncol=nc,byrow=TRUE),silent=TRUE)
		ModelsProd(prd)<-mdls
		CurrentProd(prd)<-newProductivity
		histlst<-list();histlst[[1]]<-newProductivity
		HistoryProd(prd)<-histlst
		
		sbr<-new("SurvivalBreeding")
		mdls<-makeSurvBreedParamModelList()
		nr<-length(mdls);nc<-length(mdls[[1]])
		if(nr!=sizeLeslie[1] | nc!=sizeLeslie[2])stop("The length of the survival-breeding models list does not result in a matrix of the same size as the specified Leslie")
		tmpPredict<-numeric()
		for(rr in 1:nr){
			for(cc in 1:nc){
				mdl<-mdls[[rr]][[cc]]
				if(class(mdl)[1]=="numeric"){
					res<-mdl
				}else{
					res<-predict(mdl,newdata=data.frame(bodyCond=newBodyCond),se.fit=FALSE)	#careful here to ensure we are getting a vector
				}
				tmpPredict<-c(tmpPredict,res)
			}
		}
		newSurvBreeding<-try(matrix(tmpPredict,nrow=nr,ncol=nc,byrow=TRUE),silent=TRUE)
		ModelsSB(sbr)<-mdls
		CurrentSB(sbr)<-newSurvBreeding
		histlst<-list();histlst[[1]]<-newSurvBreeding
		HistorySB(sbr)<-histlst
		
		snb<-new("SurvivalNonBreeding")
		mdls<-makeSurvNonBreedParamModelList()
		nr<-length(mdls);nc<-length(mdls[[1]])
		if(nr!=sizeLeslie[1] | nc!=sizeLeslie[2])stop("The length of the survival-nonbreeding models list does not result in a matrix of the same size as the specified Leslie")
		tmpPredict<-numeric()
		for(rr in 1:nr){
			for(cc in 1:nc){
				mdl<-mdls[[rr]][[cc]]
				if(class(mdl)[1]=="numeric"){
					res<-mdl
				}else{
					res<-predict(mdl,newdata=data.frame(bodyCond=newBodyCond),se.fit=FALSE)	#careful here to ensure we are getting a vector
				}
				tmpPredict<-c(tmpPredict,res)
			}
		}
		newSurvNonBreeding<-try(matrix(tmpPredict,nrow=nr,ncol=nc,byrow=TRUE),silent=TRUE)
		ModelsSNB(snb)<-mdls
		CurrentSNB(snb)<-newSurvNonBreeding
		histlst<-list();histlst[[1]]<-newSurvNonBreeding
		HistorySNB(snb)<-histlst
		
		les<-new("Leslie")
		newLeslie<-((newProductivity %*% newPropensity) + (newSurvBreeding %*% newSurvNonBreeding))
		CurrentLeslie(les)<-newLeslie
		Size(les)<-as.integer(sizeLeslie)
		les<-GetStableAgeDistribution(les,startAbund=start.abund)
	
		traject[[ii]]<-list(BodyCondition=bco,Propensity=prp,Productivity=prd,SurvivalBreeding=sbr,SurvivalNonBreeding=snb,Leslie=les)
	}
	return(traject)
}

