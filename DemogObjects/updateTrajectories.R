# TODO: Add comment
# 
# Author: lsalas
###############################################################################

### This function defines how updates happen to the trajectory of each simulated population. Here we use only one timestep per year, but can be split into two or more.
#' @param traject 	A list object containing all the trajectiroes being simulated
#' @param newBC		A numeric vector containing the mean and standard deviation of the distribution of body condition indices at the given timestep
#' @param timestep	An integer indicating the timestep (=year) at which the update is happening
updateTrajectories<-function(traject, newBC, timestep){
	#take trajectory list and loop through it
	bcmean<-newBC[1];bcsd<-newBC[2]
	for(jj in 1:length(traject)){
		#for each trajectory, sample body condition and update the matrices, and then abundances
		objs<-traject[[jj]]
		bco<-objs[["BodyCondition"]]
		nbco<-UpdateBC(bco,bodyCondMean=bcmean,bodyCondSD=bcsd,timestep=timestep)
		bodyCond<-CurrentBC(nbco)
		objs[["BodyCondition"]]<-nbco
		
		#retrieve the scale parameter and pass where needed 
		scalePar<-CurrentSc(nbco);scalePar<-ifelse(scalePar>1,1,scalePar)
		
		prd<-objs[["Productivity"]]
		nprd<-UpdateProd(prd,bodyCond=bodyCond,timestep=timestep)
		newProd<-CurrentProd(nprd)
		objs[["Productivity"]]<-nprd
		
		prp<-objs[["Propensity"]]
		nprp<-UpdateProp(prp,bodyCond=bodyCond,scalePar=scalePar,timestep=timestep)
		newProp<-CurrentProp(nprp)
		objs[["Propensity"]]<-nprp
		
		sbr<-objs[["SurvivalBreeding"]]
		nsbr<-UpdateSB(sbr,bodyCond=bodyCond,timestep=timestep)
		newSurvB<-CurrentSB(nsbr)
		objs[["SurvivalBreeding"]]<-nsbr
		
		snb<-objs[["SurvivalNonBreeding"]]
		nsnb<-UpdateSNB(snb,bodyCond=bodyCond,scalePar=scalePar,timestep=timestep)
		newSurvNB<-CurrentSNB(nsnb)
		objs[["SurvivalNonBreeding"]]<-nsnb
		
		les<-objs[["Leslie"]]
		nles<-UpdateLeslie(les,propenMx=newProp, prodMx=newProd, survBrdMx=newSurvB, survNonBrdMx=newSurvNB)
		nles<-UpdateAbunds(nles,timestep=timestep)
		objs[["Leslie"]]<-nles
		
		traject[[jj]]<-objs
	}
	return(traject)
}

################################################################################################################
## This function compiles and packages the abundance data from the trajectories
#' @param traject 	A list object containing all the trajectiroes being simulated
getAbundanceTrendData<-function(traject){
	results<-data.frame()
	for(tt in 1:length(traject)){
		objs<-traject[[tt]]
		les<-objs[["Leslie"]]
		traj.dat<-SummarizeLeslie(les)
		traj.dat$Traject<-tt
		results<-rbind(results,traj.dat)
	}
	return(results)
}

## This function compiles and packages the body condition data from the trajectories
#' @param traject 	A list object containing all the trajectiroes being simulated
getBCtrendData<-function(traject){
	results<-data.frame()
	for(tt in 1:length(traject)){
		objs<-traject[[tt]]
		bco<-objs[["BodyCondition"]]
		traj.dat<-SummarizeLeslie(les)
		traj.dat$Traject<-tt
		results<-rbind(results,traj.dat)
	}
}
