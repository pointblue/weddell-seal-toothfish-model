# TODO: Add comment
# 
# Author: lsalas
###############################################################################

###################################################################################################################
############# Simulation of demography from results of energy gain simulations by Weddell seals ###################
###################################################################################################################

#################################### Dependencies ##########################
library(gam)
library(MASS)
library(splines)

## Source from github:
pth.obj<-"c:/users/lsalas/workspace/PRBOGeneralUpdate/WESE/DemogObjects/"
pth.mdl<-"c:/users/lsalas/workspace/PRBOGeneralUpdate/WESE/"

################################### Specify simulation dive limit ####################
diveLim="80p"	#options: "50p", "62.5p" and "80p"

## Need path to general results, and to weight gain results summarized (see summarizeDivingResults.R)
pth.res<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Results/diving/final/results/"
pthWeights<-paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Results/diving/final/",diveLim,"_wtgain.RData",sep="")

## Need a path to save demographic model results
pthsave<-paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Results/popDy_trajectories/",diveLim,"/",sep="")

#################################### Paths to sources #################################
#################################### Get these from Github ############################
if(Sys.info()[[1]]=="Windows"){
	pth.obj<-"c:/users/lsalas/workspace/PRBOGeneralUpdate/WESE/DemogObjects/"
	pth.mdl<-"c:/users/lsalas/workspace/PRBOGeneralUpdate/WESE/"
	pth.res<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Results/diving/final/results/"
	pthWeights<-paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Results/diving/final/",diveLim,"_wtgain.RData",sep="")
	pthsave<-paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Results/popDy_trajectories/",diveLim,"/",sep="")
}else{
	pth.obj<-"/home/lsalas/Desktop/WESE/DemogObjects/"
	pth.mdl<-"/home/lsalas/Desktop/WESE/"
	pth.res<-"/home/lsalas/Desktop/WESE/diving/final/results/"
	pthWeights<-paste("/home/lsalas/Desktop/WESE/diving/final/",diveLim,"_wtgain.RData",sep="")
	pthsave<-paste("/home/lsalas/Desktop/WESE/diving/popDy_trajectories/",diveLim,"/",sep="")
} 

#################################### Load object source codeset ##############################
demogObj<-c("FishTrend","BodyConditionHistory","BodyCondition","SurvivalNonBreeding","SurvivalBreeding","Propensity","Productivity","Leslie","getInitialValues","updateTrajectories")
for(ooo in demogObj){
	src<-paste(pth.obj,ooo,".R",sep="")
	source(src)
}
#################################### Load parameter model functions ###################
source(paste(pth.mdl,"fitDemogModelsFunctions.R",sep=""))

#################################### Other necessities ################################
meta.df<-getMeta(time.int=35)
filen<-list.files(path = pth.res, pattern = diveLim, full.names = FALSE) 

####################################################################################################################################################################
## test...
aa<-modelTrajectories(time.int=35,ratetf=-0.0082,starttf=0.4,cv=6,pS=0.35,ntraject=10,startAbund=1000,pth=pth.res,filen=filen,meta.df=meta.df,pthWeights=pthWeights)
trajectories<-aa[["trajectories"]]
dd<-getAbundanceTrendData(traject=trajectories)
maxTime<-max(dd$time)
g<-subset(dd,time==maxTime)
loglambda<-(log(mean(g$totals)/1000))/maxTime
print(exp(loglambda))

## Plotting results
library(ggplot2)
p<-ggplot(data=dd,aes(x=time,y=totals, group=as.factor(Traject))) + geom_line(aes(colour=as.factor(Traject))) + theme_bw() + 
		theme(legend.position="none") + labs(x="Year",y="Seal abundance")
dev.new();print(p)

bcho<-aa[["bodyconditionhistory"]]
dd<-SummarizeBCH(bcho)
dd$lci<-dd$bc.mean-(1.96*dd$bc.se);dd$uci<-dd$bc.mean+(1.96*dd$bc.se)
p<-ggplot(data=dd,aes(x=time,y=bc.mean)) + geom_point(size=1.2) + geom_errorbar(aes(ymax=uci,ymin=lci)) + theme_bw() + labs(x="Year",y="BCI")
dev.new();print(p)


########################################################################################################
##loop through conditions and save results
tm<-Sys.time()
time.int<-35
ntraject<-100
startAbund<-1000
ratesTF<-c(-0.0082,-0.0198,-0.0396)		#
startsTF<-c(0.3,0.4)
cvals<-c(6)	#0,5,
pSuccs<-c(0.25,0.35)
bcwscale<-c(0.62,0.75,0.83) 	#0.62 = BC=1 at 84Kg, or 60% of loss; 0.75 = BC=1 at 100Kg or 70% of loss; 0.83 is BC=1 at 80% of loss recovery
for(rrr in ratesTF){
	for(sss in startsTF){
		for(ccc in cvals){
			for(ppp in pSuccs){
				for(bbb in bcwscale){
					res<-modelTrajectories(time.int=35,ratetf=rrr,starttf=sss,cv=ccc,pS=ppp,ntraject=ntraject,startAbund=startAbund,pth=pth.res,filen=filen,meta.df=meta.df,bc_w_scale=bbb,pthWeights=pthWeights)
					#careful here!! Look at source file to set destination (line 32 above)
					ratname<-ifelse(rrr==-0.0082,"low",ifelse(rrr=="-0.0198","med","high"))
					bclevname<-ifelse(bbb==0.62,"60",ifelse(bbb==0.75,"70","80"))
					filnam<-paste(pthsave,ratname,"_start",sss,"_cv",ccc,"_pS",ppp,"_recov",bclevname,".RData",sep="")
					save(res,file=filnam)
				}
			}
		}
	}
}
Sys.time()-tm

