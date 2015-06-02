# TODO: Add comment
# 
# Author: lsalas {lsalas@pointblue.org}
###############################################################################

###################################################################################################################
################### Simulation of energy gain through diving by Weddell seals #####################################
###################################################################################################################

library(gam)

##Inputs - see Supplementary Materials and Table S1 
OEcc<-7.88													## Constant of energy conversion kJ/ml O2, for a 400Kg seal, from Williams et al 2004
MassEnergyConv<-25											## Constant of mass-energy conversion: approximate caloric content of 1 g of blubber (kJ)
MRcostminute<-23.841										## Metabolic cost per minute of resting (kJ/min), from Williams et al 2004 for a 400 Kg seal, average between in water and on-ice costs
energyBe<-1150												## Gross energy gain from a single deep (benthic) dive kJ - arbitrarily set, see Supplementary materials for justification
percDd<-0.1													## Percent daily dives that are deep - from J. Burns, unpublished data
meanMassTF<-3.428795; sdMassTF<-0.3126209					## Average mass of TF in log(KG) - see toothfishSizeDist.R, dressing is 60%, so this is corrected below - when sampling
energyTF<-9*0.90											## 9 kJ/g of toothfish, per Lenky et al. 2012: energy value of a gram of toothfish consumed, times conversion efficiency constant - here using 90% (see Fadely et al 1990)
energySF<-5*0.90											## 5 kJ/g of silverfish, per Lenky et al. 2012: energy value of a gram of silverfish consumed, times conversion efficiency constant - here using 90% (see Fadely et al 1990)
meanMassSF<-3.555348;sdMassSF<-0.1777674					## Log of mean (= 35g) silverfish mass, and CV=5% - results in values 20-61 approx.
n.sims<-10													## Number of simulations to run - CAEREFUL: very time- and resource-consuming. Best to run in segments of 10, summarize each and then aggregate.
time.d<-24													## Average duration of deep dives; this from Jenn's data, average of averages, for months 1-9. Also see Davis et al 2003, table 3, type 4 dives
CostUn.d<-((1.1*time.d)+74.27)*OEcc							## Costs of benthic dives, from eq.4 in Williams et al. 2004 - cost of unsuccessful deep dive
pSuccess<-c(0.25,0.35)										## Percent successful dives
startTF.abund<-c(0.4,0.30)									## Mean of the poisson distribution of TF caught per day
rateTF<-c(-0.0396,-0.0198, -0.0082)							## Rate of TF depletion, 75%, 50% and 25% in 35 yrs respectively
cval<-c(0,5,6)												## Shape parameter for the Weibull distribution for predation release effects; now using only cval==6 (=30% predation release effects)
time.int<-35												## How many years the simulation runs
nDivesLim<-120												## Limit on number of dives. Allowable values: 120, 90, 73. Look at the limit specifications just below this statement
timeDivingLimit<-ifelse(nDivesLim==120,1150,				## Maximum time spent diving each day. Look at the limit specifications just below this statement
		ifelse(nDivesLim==90,900,720))
limName<-ifelse(nDivesLim==120,"v80p",
		ifelse(nDivesLim==90,"v625p","v50p"))
## 80% of time diving: nDivesLim=120, timeDivingLimit=1150, limName=v80p (80% of day diving)
## 90 dives max/day: nDivesLim=90, timeDivingLimit=900, limName=v625p (62.5% of day diving)
## 50% of time diving: nDivesLim=73, timeDivingLimit=720, limName=v50p (50% of day diving)
## Limit number of dives in a day calculated as: x*24 + y*8.4 = timeDivingLimit; y/x=9 (90% of dives are shallow)

## Specify a path where results will be saved.
## CAUTION: the simulation produces large files. Try with a low number of simulations (n.sim=10)
savePth<-"/home/ubuntu/boot/"

## must source grom Github:
## Utilities/utilities.functions.R


#######################################################################################################################
## Looping through scenarios, catching simulation results and saving them to files...
## NOTE: for reasons of efficient use of memory, daily diving limits are set at the top, so the simulations are within a single daily diving limit
tm<-Sys.time()
for(ss in pSuccess){
	for(aa in startTF.abund){
		for(rat in 1:3){
			rr<-rateTF[rat]
			result.data<-list()
			wtgain.data<-list()
			dive.data<-list()
			for(cc in cval){
				tfmodel<-generateTFtrendModel(startTF.abund=aa,time.int=time.int,rateTF=rr)	
				
				if(cc==0){ 					#no release
					stSF=6.8;Kval<-6.8 		
				}else if(cc==5){			#low release
					stSF=6.8;Kval=7.82		
				}else {						#high release 
					stSF=6.8;Kval=8.84		
				}
				
				sfmodel<-generateSFtrendModel(startTF.abund=aa,startSF.abund=stSF,rateTF=rr,cval=cc,Kval=Kval,time.int=time.int)	
				
				res.lst<-simulateWeightTrend(OEcc=OEcc, pSuccess=ss, MassEnergyConv=MassEnergyConv, MRcostminute=MRcostminute, energyBe=energyBe, 
						percDd=percDd, meanMassTF=meanMassTF, sdMassTF=sdMassTF, energyTF=energyTF, energySF=energySF, 
						meanMassSF=meanMassSF, sdMassSF=sdMassSF, time.d=time.d, CostUn.d=CostUn.d,tfmodel=tfmodel,sfmodel=sfmodel,
						n.sims=n.sims,time.int=time.int)
				
				raw.df<-res.lst[["rawdata"]];raw.df$pSucc<-ss;raw.df$startTF<-aa;raw.df$rateTF<-rr;raw.df$cval<-cc
				weight.df<-res.lst[["gaindata"]];weight.df$pSucc<-ss;weight.df$startTF<-aa;weight.df$rateTF<-rr;weight.df$cval<-cc
				dive.df<-res.lst[["divedata"]];dive.df$pSucc<-ss;dive.df$startTF<-aa;dive.df$rateTF<-rr;dive.df$cval<-cc
				result.data<-rbind(result.data,raw.df)
				wtgain.data<-rbind(wtgain.data,weight.df)
				dive.data<-rbind(dive.data,dive.df)
				
			}
			
			##############################################################################################################
			## SET PATH TO WHERE TO SAVE RESULTS
			filen<-paste(savePth,"wese.weightGain.s",ss,".TFA",aa,".rat",rr,"serial_",limName,".RData",sep="")
			save(result.data,wtgain.data,dive.data,file=filen)
			##############################################################################################################
		}
	}
}
Sys.time()-tm


