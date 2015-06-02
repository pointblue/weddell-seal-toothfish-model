# TODO: Add comment
# 
# Author: lsalas
###############################################################################

########### EVERY FUNCTION BELOW IS ENTIRELY HYPOTHETICAL - BASED ON PUBLISHED DATA AND SIMPLE ASSUMPTIONS #############################

### This file contains a collection of functions needed to prepare the WESE demographics simulations
### These are used by the file simulateDemographics.R


########################################################################################################################################################
## Function to generate the trend in toothfish abundance
#' @param startTF 	The starting toothfish mean encounter rate: 0.3 or 0.4 fish/d (default)
#' @param rateTF 	The rate of toothfish depletion: -0.0082, -0.0198, -0.0396 (default)
#' @param time.int	The length of the simulation in years (35 years default)
makeTFtrenddf<-function(startTF=0.4,rateTF=-0.0396,time.int=35){
	dd<-data.frame()
	for(tt in 0:(time.int-1)){
		mv<-startTF*exp(rateTF*tt)		#startTF.abund*
		tdf<-data.frame(year=(tt+1),startTF=startTF,rateTF=rateTF,TFabund=mv)
		dd<-rbind(dd,tdf)
	}
	return(dd)
}

########################################################################################################################################################
## Function to encapsulate trends in toothfish abundance with metadata about the trend and starting toothfish abundance
#' @param time.int	The length of the simulation in years (35 years default)
getMeta<-function(time.int=35){
	meta<-data.frame(startTF=c(rep(0.3,3),rep(0.4,3)),rateTF=rep(c(-0.0396,-0.0198, -0.0082),times=2))
	meta.df<-data.frame()
	for(rr in 1:nrow(meta)){
		sttf<-meta[rr,"startTF"]; rttf<-meta[rr,"rateTF"]
		tmp<-makeTFtrenddf(startTF=sttf,rateTF=rttf,time.int)
		meta.df<-rbind(meta.df,tmp)
	}
	return(meta.df)
}

########################################################################################################################################################
## Toothfish trend model - this is the trend in Toothfish consumed vs. Toothfish trend, from diving simulation results - To put in FishTrend object, slot TFabundance
#' @param startTF 	The starting toothfish mean encounter rate: 0.3 or 0.4 fish/d (default)
#' @param rateTF 	The rate of toothfish depletion: -0.0082, -0.0198, -0.0396 (default)
#' @param time.int	The length of the simulation in years (35 years default)
getTFtrendModel<-function(startTF=0.4,rateTF=-0.0396,time.int=35){
	data<-makeTFtrenddf(startTF,rateTF,time.int=35)
	tfdep.mdl<-lm("TFabund~ns(year,2)",data=data)
	return(tfdep.mdl)
}


#####################################################################################################################################################################
## The Body condition to weight model, to put in slot of body condition history object, slot WeightToBC
#' @param maxVal 	A numeric value indicating the maximum value that the weight-gained-to-body-condition function can take; i.e., the maximum value of body condition (default 1.1)
#' @param scale		A numeric value for the scale parameter of the Weibull function. Use 0.62 for 60% target mass recovery, 0.75 (default) for 70% target mass recovery, and 0.83 for 80% target mass recovery
#' @param shape		A numeric value for the shape parameter of the Weibull function. Defaults to 3 - should remain unchanged.
#' @param reverse	Boolean: if true the function returns the target mass for a particular body condion value (defaults to FALSE)
getWeightToBCModel<-function(maxVal=1.1,scale=0.75,shape=3, reverse=FALSE){
	sc<-scale;shp<-shape
	x<-seq(from=0,to=2.5,by=0.05)
	dd<-data.frame()
	for(i in x){
		y<-pweibull(i,shape=shape,scale=scale)*maxVal
		y<-ifelse(y<min(x),min(x),y)
		tmp<-data.frame(weightGain=i,bodyCond=y)
		dd<-rbind(dd,tmp)
	}
	dd$weightGain<-dd$weightGain*100
	wtbc.mdl<-gam(as.formula("bodyCond~s(weightGain,spar=0.5)"),data=dd)
	if(reverse==TRUE){
		wtbc.mdl<-gam(as.formula("weightGain~s(bodyCond,spar=0.5)"),data=dd)
	}
	return(wtbc.mdl)
}


#####################################################################################################################################################################
## The scale parameter to Body condition model, to put in slot of BodyCondition objects, slot ScaleModel
#' @param maxVal 	A numeric value indicating the maximum value that the body-condition-to-scale-parameter function can take; i.e., the maximum value of the scale parameter (default 1.05)
#' @param scale		A numeric value for the scale parameter of the Weibull function. Defaults to 0.55 - should remain unchanged
#' @param shape		A numeric value for the shape parameter of the Weibull function. Defaults to 3 - should remain unchanged.
getScaleParamModel<-function(maxVal=1.05,scale=0.55,shape=3){
	sc<-scale;shp<-shape
	x<-seq(from=0,to=1.1,by=0.05); y<-pweibull(q=x,scale=sc,shape=shp)
	#the weibull is S-shaped for the appropriate scale and shape paramters, and bound to 0-1, so we can directly link
	#BC to a propensity value by multiplying the weibull prob times the maxPropensity.
	scaledPr<-maxVal*y
	df<-data.frame(bodyCond=x,scaledPr=scaledPr)	
	bcscale.mdl<-gam(as.formula("scaledPr~s(bodyCond,spar=0.5)"),data=df)
	
	return(bcscale.mdl)
}


#####################################################################################################################################################################
## Setting of demographic parameter models
## Foremost, note the dimensions and structure of this nested list of models at the bottom
## The lengths of the lists must match the dimensions of the Leslie, so the lenghts are an input parameter (dimv)

###But first we need to fit the models for all demographic parameters needed. 

### We will need this function to backtransform logit data
#' @param x		A value in logit space
btLogit<-function(x){
	res<-exp(x)/(1+exp(x))
	return(res)
}

#####################################################################################################################################################################
### Function to create a base matrix
#' @param nr	Number of rows in the matrix
#' @param nc	Number of columns in the matrix
setBaseModelsMatrix<-function(nr,nc){
	paramModelsMatrix<-list()
	mdls<-list()
	for(rr in 1:nr){
		paramModelsMatrix[[rr]]<-list()
		for(cc in 1:nc){
			paramModelsMatrix[[rr]][[cc]]<-0
		}
	}
	return(paramModelsMatrix)
}

#####################################################################################################################################################################
### Create a body-condition-to-reproductive-propensity function; returns an invariant breeding propensity value
#' @param prpval	A numeric value for the propensity
getPropenModel<-function(prpval){
	dd<-data.frame(bodyCond=seq(0,1.5, by=0.05),propenVal=rep(prpval,times=31))
	brProplm<-lm("propenVal~bodyCond",data=dd)
	return(brProplm)
}

#####################################################################################################################################################################
### Create a body-condition-to-survival-probability-during-breeding function; returns an invariante survival probabiolity value
#' @param nb	A numeric value with the number of bootstrap samples to obtain froma normal distribution of the survival probability for each value of body condition (in logit space)
#' @param mn	A numeric value for the mean of the distribution of survival probability
#' @param vr	A numeric value for the standard deviation for the distribution of survival probability
getSurvivalModel<-function(nb,mn,vr){
	dd<-data.frame(bodyCond=seq(0,1.5, by=0.05),sbPr=btLogit(rnorm(nb,mn,vr)))
	svPrYlg<-lm("sbPr~bodyCond",data=dd)
	return(svPrYlg)
}

##################################################### Productivity ####################
###Breeding productivity, assumed to be 1, no twins, so... - this list of models goes in slot ModelsProd of Productivity object

### Function to generate the list of functions for Productivity object
### Note that here we must be specific about the size of the matrix and the stages
#' @param nc 	The number of columns in the leslie
#' @param nr	The number of rows. 
makeProductivityParamModelList<-function(nc=22,nr=22){
	productParamModels<-setBaseModelsMatrix(nr=nr,nc=nc)
	###Br5<-1;Br6<-1;Br7<-1;Br8<-1;Br9<-1;Br10<-1;BrO<-1
	###Which we put as a linear model like so... assume if breeding always successful and no twins - so 0.5 because this is a females-only model
	dd<-data.frame(bodyCond=seq(0,1.5, by=0.05),brPr=rep(0.5,times=31))
	brPrlm<-lm("brPr~bodyCond",data=dd)
	
	###Now add the specific parameter model, as fit previously, and these go in the first row...
	### Note last age class is not breeding, as well as the first 5 (pup, yearling and ages 2-4)
	for(cc in 6:nc-1){
		productParamModels[[1]][[cc]]<-brPrlm
	}
	return(productParamModels)
}


##################################################### Breeding Propensity ####################
###Breeding propensity - values come from Rotella et al. 2012, table 1 (Psi values) - this list of models goes in slot ModelsProp of Propensity object

### Function to generate the list of functions for Breeding Propensity object
### All propensity values, though simulated separately here as liner models, are invariant
### All variation in propensity is through the scale parameter obtained from the body condition functions in the BC and BCH objects
### Note that here must be specific about the size of the matrix and the stages
#' @param nc 	The number of columns in the leslie
#' @param nr	The number of rows.
#' @param BrPrpVector	Either 0 (default) or a vector of values for breeding propensity for each of the age classes in the Leslie matrix
makePropenParamModelList<-function(BrPrpVector=0,nr=22,nc=22){

	if(BrPrpVector==0){BrPrpVector<-c(rep(0,times=5),0.04,0.2084,0.423,0.704,0.912,rep(0.96,times=11),0)}	
	propenParamModels<-setBaseModelsMatrix(nr=nr,nc=nc)
	
	mdls<-list()
	for(bpp in 1:NROW(BrPrpVector)){
		if(BrPrpVector[bpp]>0){
			mdls[[bpp]]<-getPropenModel(prpval=BrPrpVector[bpp])
		}else{
			mdls[[bpp]]<-0
		}
	}
	
	###Now add the specific parameter model, as fit previously, and these go in the diagonal...
	for(cc in 1:length(mdls)){
		propenParamModels[[cc]][[cc]]<-mdls[[cc]]
	}
	return(propenParamModels)
}


##################################################### Survival Breeding ####################
###See Excel worksheet for details of calculations (Propensity.survival.calc.xlsx) - this list of models goes in slot ModelsProp of SurvivalBreeding object

### Function to generate the list of models for Survival Breeding object
### Note that here must be specific about the size of the matrix and the stages
#' @param nc 	The number of columns in the leslie
#' @param nr	The number of rows.
makeSurvBreedParamModelList<-function(nr=22,nc=22){
	survBrParamModels<-setBaseModelsMatrix(nr=nr,nc=nc)
	
	meanvar.df<-data.frame(
			mn=c(1.386,rep(5.806,3),rep(3.892,6),rep(6.213,10),-0.847),
			vr=c(0.173,rep(0.175,3),rep(0.139,6),rep(0.218,10),0.099)
	)
	
	survBrParamModels[[2]][[1]]<-1
	for(jj in 1:20){
		survBrParamModels[[jj+2]][[jj+1]]<-getSurvivalModel(nb=31,mn=meanvar.df[jj,"mn"],vr=meanvar.df[jj,"vr"])
	}
	survBrParamModels[[22]][[22]]<-getSurvivalModel(nb=31,mn=meanvar.df[21,"mn"],vr=meanvar.df[21,"vr"])
	
	return(survBrParamModels)
}


##################################################### Survival NonBreeding ####################
###See Excel worksheet for details of calculations (Propensity.survival.calc.xlsx) - this list of models goes in slot ModelsProp of SurvivalNonBreeding object

### Function to generate the list of models for Survival NonBreeding object
### Note that here must be specific about the size of the matrix and the stages
#' @param nc 	The number of columns in the leslie
#' @param nr	The number of rows.
makeSurvNonBreedParamModelList<-function(nr=22,nc=22){
	survNonBrParamModels<-setBaseModelsMatrix(nr=nr,nc=nc)
	
	meanvar.df<-data.frame(
			mn=c(0.405,0.565,rep(5.110,3),rep(2.730,6),rep(2.468,10),-0.847),
			vr=c(0.173,0.173,rep(0.154,3),rep(0.139,6),rep(0.154,10),0.099)
	)
	
	for(jj in 1:nrow(meanvar.df)){
		survNonBrParamModels[[jj]][[jj]]<-getSurvivalModel(nb=31,mn=meanvar.df[jj,"mn"],vr=meanvar.df[jj,"vr"])
	}
	
	return(survNonBrParamModels)
}

#################################################Functions fitting models based on the energy simulation results #######################################
## The following three functions are used to initialize objects - see getInitialValues.R (function getFishTrendObject)
########################################################################################################################################################
## Toothfish abundance model - this is the trend in Toothfish consumed vs. Toothfish trend, from diving simulation results - To put in FishTrend object, slot Toothfish
#' @param what 	A string indicating what to report on: trned of masses (default) or counts
#' @param pth	A string pointing to the location of energy simulation result files (summarized, and pointing to the appropriate folder)
#' @param filen	A vector of names of files to look into to retrieve sum of Toothfish consumed (if what="numbers") or mean of toothfish mass consumed (if what="masses")
#' @param meta.df A data frame with the metadata information about the trends in toothfish and silverfish simulated
#' @param rtTF 	The starting toothfish mean encounter rate for which results are being sought: 0.3 or 0.4 fish/d
#' @param stTF	The rate of toothfish depletion for which results are being sought: -0.0082, -0.0198, -0.0396
#' @param cv	An integer of values 0, 5 or 6 indicating no, medium (15%) or high (30%) predation release effects on silverfish abundance, for which results are being sought
#' @param pS	A numeric value providing the probability of diving success for which results are being sought
getAbundTFmodel<-function(what="masses",pth,filen,meta.df,rtTF,stTF,cv,pS){
	
	#if what=="masses", we want 50p, or 625p or 80p_mean_TFmass.RData - the diving limit is decided with the parameter diveLim in simulateDemographics.R
	#if what=="numbers, we need 80p_sum_TFcons.RData, etc.
	#these files contain agg.df
	grepvar<-ifelse(what=="masses","mean_TFmass.RData","sum_TFcons.RData")
	file<-subset(filen,grepl(grepvar,filen,fixed=TRUE))
	filepth<-paste(pth,file,sep="")
	load(filepth)
	data.df<-merge(agg.df,meta.df,by=c("year","startTF","rateTF"),all.x=TRUE)
	data.df<-subset(data.df, rateTF==rtTF & startTF==stTF & pSucc==pS & cval==cv)
	if(what=="masses"){
		mdlTF<-lm(as.formula("tf.mass~ns(TFabund)"),data=data.df)
	}else if(what=="numbers"){
		mdlTF<-lm(as.formula("tf.cons~ns(TFabund)"),data=data.df)
	}else{stop("Could not fit the Toothfish model")}
	
	return(mdlTF)
}

#####################################################################################################################################################################
## Silverfish model - same as with Toothfish model above: trend in Silverfish consumed vs. Toothfish trend - To put in FishTrend object, slot Silverfish
#' @param what 	A string indicating what to report on: trned of masses (default) or counts
#' @param pth	A string pointing to the location of energy simulation result files (summarized, and pointing to the appropriate folder)
#' @param filen	A vector of names of files to look into to retrieve energy simulation results
#' @param meta.df A data frame with the metadata information about the trends in toothfish and silverfish simulated
#' @param rtTF 	The starting toothfish mean encounter rate for which results are being sought: 0.3 or 0.4 fish/d
#' @param stTF	The rate of toothfish depletion for which results are being sought: -0.0082, -0.0198, -0.0396
#' @param cv	An integer of values 0, 5 or 6 indicating no, medium (15%) or high (30%) predation release effects on silverfish abundance, for which results are being sought
#' @param pS	A numeric value providing the probability of diving success for which results are being sought
getAbundSFmodel<-function(what="masses", pth, filen, meta.df,rtTF,stTF,cv,pS){
	
	#if what=="masses", we want 50p, or 625p or 80p_mean_SFmass.RData - the diving limit is decided with the parameter diveLim in simulateDemographics.R
	#if what=="numbers, we need 80p_sum_SFcons.RData, etc.
	#these files contain agg.df
	grepvar<-ifelse(what=="masses","mean_SFmass.RData","sum_SFcons.RData")
	file<-subset(filen,grepl(grepvar,filen,fixed=TRUE))
	filepth<-paste(pth,file,sep="")
	load(filepth)
	data.df<-merge(agg.df,meta.df,by=c("year","startTF","rateTF"),all.x=TRUE)
	data.df<-subset(data.df, rateTF==rtTF & startTF==stTF & pSucc==pS & cval==cv)
	if(what=="masses"){
		mdlSF<-lm(as.formula("sf.weights~ns(TFabund)"),data=data.df)
	}else if(what=="numbers"){
		mdlSF<-lm(as.formula("sf.cons~ns(TFabund)"),data=data.df)
	}else{stop("Could not fit the Silverfish model")}
	
	return(mdlSF)
}

#####################################################################################################################################################################
##  The Weight gain model - this is a model of weight vs. Toothfish consumed, to put in FishTrend object, slot TFtoWeight
#' @param pth	A string pointing to the location of energy simulation result file with weight gains for the appropirate time limit
#' @param meta.df A data frame with the metadata information about the trends in toothfish and silverfish simulated
#' @param rtTF 	The starting toothfish mean encounter rate for which results are being sought: 0.3 or 0.4 fish/d
#' @param stTF	The rate of toothfish depletion for which results are being sought: -0.0082, -0.0198, -0.0396
#' @param cv	An integer of values 0, 5 or 6 indicating no, medium (15%) or high (30%) predation release effects on silverfish abundance, for which results are being sought
#' @param pS	A numeric value providing the probability of diving success for which results are being sought
getWeightGainModel<-function(pth,meta.df,rtTF,stTF,cv,pS){
	
	load(pth)	#the file has a data.df object with the weight gain data 
	
#add attribution: df with year, startTF, rate, TFabund
	data.df<-merge(data.df,meta.df,by=c("year","startTF","rateTF"),all.x=TRUE)
	data.df<-subset(data.df, rateTF==rtTF & startTF==stTF & pSucc==pS & cval==cv)
	
#Now fit a model that predicts based on TFabund.
	mdlTF<-lm(as.formula("weightGainedKg~ns(TFabund)"),data=data.df)
	
}


################################################# MODELING TRAJECTORIES ######################################################################
##############################################################################################################################################
## This function models the population trajectories based initial parameters, related to diving parametrizations
## It uses the functions above, and the parameters specified in simulateDemographics.R 

#' @param time.int		The length of the dive simulation in years (defaults to 35)
#' @param ratetf		The rate of toothfish depletion	(defaults to -0.0396
#' @param starttf		The starting toothfish abundance (or encounter rate - defaults to 0.4 fish/d)
#' @param cv			The scale parameter of the Weibull function for predation release effects (defaults to 6, for a 30% increase in silverfish abundance from predation release effects)
#' @param pS			The probability of diving success (the chance that a dive will result in silverfish consumed, defaulst to 0.35
#' @param ntraject		The number of population trajectories to simulate (defaults to 100)
#' @param startAbund	The starting number of seals in the populations being simulated (defaults to 1,000)
#' @param pth			The path to the folder with the (summarized) results from the diving simulations, for sum of Toothfish and Silverfish consumed, and mean of toothfish and silverfish mass consumed (see file compile_finalData.R)
#' @param filen			The name of the appropriate summary file or files (based on the diveLim parameter, set in simulateDemographics.R)
#' @param meta.df		The data.frame object with the metadata about toothfish abundance each year under the appropriate depletion rate and starting abundance
#' @param bc_w_scale	The scale parameter for the Weibull function defining the relationship between weight gain and body condition index (defaults to 0.75, for a 70% target mass recovery)
#' @param pthWeights	The path to the appropriate summary file of weight gains from the dive simulation (based on the diveLim parameter, set in simulateDemographics.R)
modelTrajectories<-function(time.int=35,ratetf=-0.0396,starttf=0.4,cv=6,pS=0.35,ntraject=100,startAbund=1000,pth,filen,meta.df,bc_w_scale=0.75,pthWeights){
	
	#################################### Set initial parameters ###########################
	## These are entirely dictated by the dive simulations parameters passed to the function
	
	fto<-getFishTrendObject(startTF=starttf,rat=ratetf,time.int=time.int,pth=pth,filen=filen,meta.df=meta.df,cval=cv,pSucc=pS,pthWeights=pthWeights) 	#trend=0, maxSF.cons=300	# startTF.abund, trend, sd.TFtrend, time.int, bootsize, initSF.cons, maxSF.cons, sftfrate, numTF.indiet, rate.damp, sd.SFtrend
	cwt<-CurrentWeight(fto)
	
	## The below function call generates the model that relates seal weight to body condition
	wtStdev<-1+0.05				#0.15 = Value in Garrot et al. 2013, table 1, the standard deviation of the normal dist of random effects on demographic parameters for non-iceberg years, here expressed as a rate
	bcho<-getBodyConditionHistoryObject(cwt=cwt,wtStdev=wtStdev,bc_w_scale=bc_w_scale)
	
	pcbc<-CurrentBCH(bcho)
	bcmean<-pcbc[1];bcse<-pcbc[2]
	
	################################### Set trajectory arrays and load starting values into objects ###########################
	## For the first time we set the size of the Leslie
	sizeLeslie<-c(22,22)
	n.traject<-ntraject
	startWESE.abund<-startAbund
	
	trajectories<-getInitialWESEdemogValues(sizeLeslie=sizeLeslie,start.abund=startWESE.abund,bcmean=bcmean,bcse=bcse,n.traject=n.traject)
	
	################################### Run loops ####################################
	## The below depends on the two top level objects, fto and bcho, instantiated and populated above
	## These objects define the trend in fish abundances and consequently, the trend in WESE body condition
	## The trend in body condition is evaluated at each time step and each trajectory is updated with it
	for(tt in 2:time.int){
		#get fish trend values
		fto<-UpdateFish(object=fto,timestep=as.integer(tt))
		wtMean<-CurrentWeight(fto)
		wtStdev<-wtMean*(0.15)
		
		#get body condition dist params
		bcho<-UpdateBCH(object=bcho,wtMean,wtStdev,timestep=as.integer(tt))
		nbc<-CurrentBCH(bcho)
		traj<-try(updateTrajectories(traject=trajectories, newBC=nbc, timestep=as.integer(tt)),silent=TRUE)
		if(!inherits(traj,"try-error")){
			trajectories<-traj
		}else{
			res<-list(trajectories=trajectories,fishobject=fto,bodyconditionhistory=bcho)
			return(res)
		}
	}
	
	res<-list(trajectories=trajectories,fishobject=fto,bodyconditionhistory=bcho)
	return(res)
}








