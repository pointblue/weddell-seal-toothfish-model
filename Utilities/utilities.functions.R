# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## First we need a function to model the decline of Toothfish in WESE daily diet - note that we have trend values
## Make sure to use a rate, not a trend. CCAMLR rate (50% drop in 35 yrs) = -0.0198; alternatively consider 75% drop in 35 years = -0.0396
## NOTE: the prob of 0 for a poisson dist with lambda=1 is 37%, the prob of 1 is 37%, the prob of 2 or more is 26%, so we start with seals catching 2 TF 26% of successful days, and at least 1 TF 63% of days
## NOTE: the prob of 0 for a poisson dist with lambda=0.5 is 61%, the prob of 1 is 30%, the prob of 2 or more is 9%
generateTFtrendModel<-function(startTF.abund=0.6,rateTF=-0.0396,cv.trendTF=0.05,time.int=20,bootsize=5,...){	#Function 1
	dd<-data.frame()
	for(tt in 0:time.int){
		mv<-startTF.abund*exp(rateTF*tt)		#startTF.abund*
		rmv<-rnorm(bootsize,mean=mv,sd=cv.trendTF*mv)
		tdf<-data.frame(time=rep(tt,times=bootsize),abund.tf=rmv)
		dd<-rbind(dd,tdf)
	}
	tfmodel<-gam(as.formula("abund.tf~s(time)"),data=dd)	#lm("abund.tf~time+I(time^2)",data=dd)
	return(tfmodel)
}

## We also need a function to model the increase in silverfish abundance, following predation release
generateSFtrendModel<-function(startTF.abund=0.4,startSF.abund=5.78,rateTF=-0.0396,cval=1,Kval=7.82,time.int=35,...){		
	#Using a direct mathematical relationship between TF abundance and SF growth rate
	#cval=0 means, thus, no release
	dd<-data.frame()
	for(tt in 1:time.int){
		mv<-startTF.abund*exp(rateTF*tt)
		mvd<-(startTF.abund-mv)/startTF.abund
		rateSF<--1*cval*rateTF*mvd
		vv<-Kval*startSF.abund*exp(rateSF*tt)/(Kval + (startSF.abund*(exp(rateSF*tt)-1)))
		tdf<-data.frame(time=tt,abund.tf=mv,abund.sf=vv)
		dd<-rbind(dd,tdf)
	}
	sfmodel<-gam(as.formula("abund.sf~s(time)"),data=dd)
	return(sfmodel)
}

## This is the main function of the simulations: daily diving simulation, or energy gain function
## BEWARE: one simulation run = 1 scenario for 35 years (= 35 yrs x 90 days = 3150 loops = LOTS of memory needed and large results vectors)
## Benchmark: 10 simulations of 1 scenario consume 1 hr of an Amazon EC2 Ubuntu server's time.
simulateWeightTrend<-function(OEcc, pSuccess, MassEnergyConv, MRcostminute, energyBe, percDd, meanMassTF, sdMassTF, 
		energyTF, energySF, meanMassSF, sdMassSF, time.d, CostUn.d,tfmodel,sfmodel,n.sims,time.int){		
	
	res.df<-data.frame(cbind(rep(NA,(n.sims*time.int*90)),NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)) #12 columns, 3150 x boot rows - dimensioning result vector to increase speed of execution
	names(res.df)<-c("boot","year","day","totalDiveTime","restCost","diveEnergyCost","tf.cons","tf.mass","sf.cons","sf.weights","e.gainBe","e.dailyGained")
	jjj<-0
	for(bb in 1:n.sims){
		#every simulation is 3150 rows of raw results, 35 rows of energy gain data, and 35 rows of dive gehavior data
		for(yy in 1:time.int){
			startTime<-Sys.time()
			for(dd in 1:90){
				massConsumed<-numeric()
				sDiveEnergyCost<-numeric()
				diveTimes<-numeric()
				catchTF<-integer()
				massTF<-numeric()
				dive.time<-numeric()
				propTF<-predict(tfmodel,newdata=data.frame(time=yy))		## estimated for evary year in the interval modeled
				catchTF<-rpois(n=1,lambda=propTF)							## did the seal catch toothfish today?
				catchTF<-ifelse(catchTF>2,2,catchTF)						## capping toothfish harvest at 2 fish/d max
				massTF<-sum(round(rlnorm(catchTF,			
										mean=meanMassTF,sd=sdMassTF),digits=3))		## mass of the toothfish caught, if any
				massTF<-massTF*1000*0.6 									## converting to grams and adjusting for dress weight as 60% of mass
				massConsumed<-massTF
				nTFdives<-ifelse(catchTF==0,0,ifelse(catchTF>1,2,1))		## how many dives to catch these many TF...
				dive.time<-rnorm(n=nTFdives,mean=8.4,sd=1.87)				## how much diving time... 8.4 minites, 1.87 minutes - values from Burns pers. comm.
				diveTimes<-c(diveTimes,dive.time)							## adding to tally of dive times
				dive.cost<-((4.74*dive.time)-8.2)*1.44*OEcc					## dive costs, from eq.3 in Williams et al. 2004
				gainTF<-(energyTF*massTF)-sum(dive.cost)					## propTF is the proportion of a TF caught that year = tfmodel prediction (thus times energy minus acquisition costs)
				sDiveEnergyCost<-c(sDiveEnergyCost,dive.cost)				## add to tally of energy costs
				
				##Next... This function is a do-while loop: each iteration is a dive of average 8.4 minutes long (sd:1.87) - per J. Burns' unpiblished data for months 1-9.
				sfConsumed<-numeric()
				sfWeights<-numeric()
				sf.mean<-predict(sfmodel,newdata=data.frame(time=yy))		## what is the mean number of silverfish encountered per successful dive?
				repeat{	## each iteration is a dive during this day
					## In each iteration, a sf mean value is taken from a poison distribution with mean varying with silverfish abundance trend
					## We use the mean silverfish within the do-while loop to generate a sample for one dive simulation, for the number of silverfish catches in the dive
					## We thus add the energy from silverfish catched, the cost from the increasing number of dives, the energy added by benthic dives
					##T he do-while loop function has the limits specified above, or stops when the stomach capacity limit has been reached
					sfcatch<-integer()
					catchWeights<-integer()
					dive.time<-numeric()
					successYN<-ifelse(runif(1)>pSuccess,0,1)						## successful dive?
					sfcatch<-rpois(n=1,lambda=sf.mean)*successYN					## seal takes a dive, catches some SF
					sfcatch<-sfcatch[1]
					sfConsumed<-c(sfConsumed,sfcatch)								## add numbers catched to tally
					catchWeights<-round(rlnorm(sfcatch,
									mean=meanMassSF,sd=sdMassSF),digits=0)			## what were the weights of the catch?
					catchWeights<-ifelse(catchWeights<20,20,catchWeights)			## bounding low at 20g	
					catchWeights<-ifelse(catchWeights>60,60,catchWeights)			## bounding high at 60g
					sfWeights<-c(sfWeights,catchWeights)							## add weights to tally of weights
					dive.time<-rnorm(n=1,mean=8.4,sd=1.87)							## how much time spent catching these?
					diveTimes<-c(diveTimes,dive.time)								## adding to tally of dive times
					dive.cost<-((4.74*dive.time)-8.2)*OEcc 							## dive costs, from eq.3 in Williams et al. 2004
					if(sfcatch[1]>0)dive.cost<-dive.cost*1.44						## adding post-prandial costs, per Williams et al. 2004
					sDiveEnergyCost<-c(sDiveEnergyCost,dive.cost)					## adding to tally of energy costs
					
					## The current number of dives/day is used to estimate the small gain from deep dives...
					## Deep dives result in a small net energy gain. It costs ~1143 kJ for a successful deep dive, so 
					## for the dive to result in a small gain, energyBe must be higher than 1150 kJ. Default is set to 1200 kJ (see energyBe above) 
					currenNdives<-NROW(sDiveEnergyCost)								## How many deep dives at this point?
					gainBe<-(energyBe-(CostUn.d*1.44))*floor(currenNdives*percDd)	## Gain from benthic dives, assuming NDd*percDd >= 1
					timeBe<-time.d*floor(currenNdives*percDd)						## time spent on benthic dives
					
					currentGainedEnergy<-((sum(sfWeights)*energySF)-
								sum(sDiveEnergyCost))+gainBe						## Where is the seal wrt energy gain?
					
					## Now checking on daily limits...(See limit specifications above)
					if((sum(diveTimes))+timeBe>timeDivingLimit){					## limit number of minutes diving in a day
						break
					}else if((NROW(sDiveEnergyCost)+ceiling(currenNdives*percDd))>nDivesLim){	## limit number of dives in a day
						break
					}else if((sum(sfWeights)+massTF)>=40000){						## stomach capacity limit for a day
						break
					}else{}
				}
				## calculate the time not diving and its costs...
				totalDiveTime<-sum(diveTimes); restTime<-1440-totalDiveTime
				restCost<-restTime*MRcostminute
				
				## log the day's accomplishments for the tally 
				tmp.df<-data.frame(boot=bb,year=yy,day=dd,totalDiveTime=totalDiveTime,restCost=restCost,
						diveEnergyCost=sum(sDiveEnergyCost),tf.cons=catchTF,tf.mass=massTF,sf.cons=sum(sfConsumed),sf.weights=mean(sfWeights),
						e.gainBe=gainBe,e.dailyGained=currentGainedEnergy+gainTF-restCost)
				jjj<-jjj+1
				res.df[jjj,]<-tmp.df[1,]	## this is the raw results tally
			}
		}
	}
	
	## Aggregate tallies to report
	## energy gain tally
	gain.df<-aggregate(as.formula("e.dailyGained~boot+year"),data=res.df,FUN="sum")
	names(gain.df)<-c("boot","year","energyGained")
	gain.df$weightGainedKg<-gain.df$energyGained/25000
	
	## diving behavior tally
	dive.df<-aggregate(as.formula("totalDiveTime~boot+year"),data=res.df,FUN="sum")
	names(dive.df)<-c("boot","year","totalDiveTime")
	dive.df$avgDiveTime<-dive.df$totalDiveTime/90
	
	## put all three tallies (raw results, energy, diving) in a list and send back.  
	## The list contains all the results for 10 simulations of a single scenario
	res.lst<-list(rawdata=res.df,gaindata=gain.df,divedata=dive.df)	
	return(res.lst)
}

