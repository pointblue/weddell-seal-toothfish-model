# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(ggplot2)


source("c:/users/lsalas/workspace/PRBOGeneralUpdate/WESE/Single_TFconsumpt_BC_rel/Results/Graphs/compile_finalData.R")
lambda.df<-getF6_elasticitiesData()

lambda.df$scaledDepletion<-ifelse(lambda.df$depletion=="Hi",-0.0396,ifelse(lambda.df$depletion=="Lo",-0.0082,-0.0198))
lambda.df$scaledStartAbund<-as.numeric(as.character(lambda.df$startAbund))
lambda.df$scaledPSucc<-ifelse(lambda.df$pSucc=="35%",0.35,0.25)		#0.167,-0.167
lambda.df$scaledDiveLimit<-ifelse(lambda.df$diveLimit=="80% of day",0.80,ifelse(lambda.df$diveLimit=="50% of day",0.50,0.625))		#0.246,ifelse(lambda.df$diveLimit=="50% of day",-0.246,-0.026)
lambda.df$scaledPctRecover<-ifelse(lambda.df$pRecov=="80%",0.80,ifelse(lambda.df$pRecov=="60%",0.60,0.70))

#we can use this, but once scaled, it's the same as using the above
###################################################################
#lambda.df$scaledDepletion<-ifelse(lambda.df$depletion=="Hi",1,ifelse(lambda.df$depletion=="Lo",-0.586,0))
#lambda.df$scaledStartAbund<-ifelse(lambda.df$startAbund==0.3,-0.25,0)
#lambda.df$scaledPSucc<-ifelse(lambda.df$pSucc=="25%",-0.286,0)		#0.167,-0.167
#lambda.df$scaledDiveLimit<-ifelse(lambda.df$diveLimit=="80% of day",0.28,ifelse(lambda.df$diveLimit=="50% of day",-0.2,0))		#0.246,ifelse(lambda.df$diveLimit=="50% of day",-0.246,-0.026)
#lambda.df$scaledPctRecover<-ifelse(lambda.df$pRecov=="80%",0.143,ifelse(lambda.df$pRecov=="60%",-0.143,0))
#####################################################################


#### Scaling...
lambda.df$scaledDepletion<-as.numeric(scale(lambda.df$scaledDepletion))
lambda.df$scaledStartAbund<-as.numeric(scale(lambda.df$scaledStartAbund))
lambda.df$scaledPSucc<-as.numeric(scale(lambda.df$scaledPSucc))
lambda.df$scaledDiveLimit<-as.numeric(scale(lambda.df$scaledDiveLimit))
lambda.df$scaledPctRecover<-as.numeric(scale(lambda.df$scaledPctRecover))

## Simple model - full model below!
sens_pct<-lm(as.formula("lambda~scaledDepletion+scaledStartAbund+scaledPSucc+scaledDiveLimit+scaledPctRecover"),data=lambda.df)

pars<-c("scaledDepletion","scaledStartAbund","scaledPSucc","scaledDiveLimit","scaledPctRecover")
getElasticities<-function(mdl,pars,par,df,change=0.01){
	opars<-pars[grepl(par,pars)==FALSE]
	uv<-unique(df[,par])
	epar<-seq.int(from=min(uv), to=max(uv), length.out=99)
	epar<-c(epar,0)
	pred.df<-data.frame(epar=epar,v1=rep(0,times=100),v2=rep(0,times=100),v3=rep(0,times=100),v4=rep(0,times=100))	#params have been standardized, so mean is 0
	names(pred.df)<-c(par,opars)
	pred.df$predicted<-predict(mdl,newdata=pred.df)	#predicting lambda
	pred.df<-pred.df[order(pred.df$predicted),]
	at0<-subset(pred.df,pred.df[,par]==0); at0<-at0[1,]	#predicted value at mean of par
	at0p<-at0$predicted
	ulim<-at0p+change;llim<-at0p-change	#1% change in lambda...
	usub<-subset(pred.df,predicted>ulim); lsub<-subset(pred.df,predicted<llim)
	ul<-round(usub[1,par],2)
	return(c(par,ul))
}

dd1<-getElasticities(mdl=sens_pct,pars=pars,par="scaledDepletion",df=lambda.df)
dd2<-getElasticities(mdl=sens_pct,pars=pars,par="scaledStartAbund",df=lambda.df)
dd3<-getElasticities(mdl=sens_pct,pars=pars,par="scaledPSucc",df=lambda.df)
dd4<-getElasticities(mdl=sens_pct,pars=pars,par="scaledDiveLimit",df=lambda.df)
dd5<-getElasticities(mdl=sens_pct,pars=pars,par="scaledPctRecover",df=lambda.df)

data<-data.frame(rbind(dd1,dd2,dd3,dd4,dd5))	
names(data)<-c("Parameter","Elasticity")
data$ParameterName<-c("Toothfish depl. rate","Starting Abundance","% dive success","Daily dive limit","% mass recovery")
data$Elasticity<-as.numeric(as.character(data$Elasticity))
data<-subset(data,ParameterName != "% mass recovery")
#removing % mass recovery because it has very large elasticities, for plotting purposes...
pelas<-ggplot(data=data,aes(x=ParameterName,y=Elasticity)) + geom_bar(stat="identity", width=0.8) +
		theme_bw() + 
		theme(axis.text.x=element_text(size=10, angle=20, hjust=1)) +
		labs(x="Parameter",y="Elasticity (units of SD)")

jpeg(filename="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/antarctica/paper/figures/Figure8_elasticitiesLambda.jpg",
		width=400, height=350,quality=100)
print(pelas)
dev.off()


## Partial r-squared
sse_full<-deviance(sens_pct)
mdl_dplt<-lm(as.formula("lambda~scaledStartAbund+scaledPSucc+scaledDiveLimit+scaledPctRecover"),data=lambda.df); sse_dplt<-deviance(mdl_dplt)
mdl_stab<-lm(as.formula("lambda~scaledDepletion+scaledPSucc+scaledDiveLimit+scaledPctRecover"),data=lambda.df); sse_stab<-deviance(mdl_stab)
mdl_psuc<-lm(as.formula("lambda~scaledDepletion+scaledStartAbund+scaledDiveLimit+scaledPctRecover"),data=lambda.df); sse_psuc<-deviance(mdl_psuc)
mdl_dvlm<-lm(as.formula("lambda~scaledDepletion+scaledStartAbund+scaledPSucc+scaledPctRecover"),data=lambda.df); sse_dvlm<-deviance(mdl_dvlm)
mdl_prcv<-lm(as.formula("lambda~scaledDepletion+scaledStartAbund+scaledPSucc+scaledDiveLimit"),data=lambda.df); sse_prcv<-deviance(mdl_prcv)

part_r2<-data.frame(Parameter=pars,Partial_r2=c(
				((sse_dplt-sse_full)/sse_dplt),
				((sse_stab-sse_full)/sse_stab),
				((sse_psuc-sse_full)/sse_psuc),
				((sse_dvlm-sse_full)/sse_dvlm),
				((sse_prcv-sse_full)/sse_prcv)))


#######################
## FULL MODEL
sens_pct_full<-lm(as.formula(paste("lambda~scaledDepletion+scaledStartAbund+scaledPSucc+scaledDiveLimit+scaledPctRecover+",
						"I(scaledDepletion*scaledStartAbund)+",
						"I(scaledDepletion*scaledPSucc)+",
						"I(scaledDepletion*scaledDiveLimit)+",
						"I(scaledDepletion*scaledPctRecover)+",
						"I(scaledStartAbund*scaledPSucc)+",
						"I(scaledStartAbund*scaledDiveLimit)+",
						"I(scaledStartAbund*scaledPctRecover)+",
						"I(scaledPSucc*scaledDiveLimit)+",
						"I(scaledPSucc*scaledPctRecover)+",
						"I(scaledDiveLimit*scaledPctRecover)+",
						"I(scaledDepletion*scaledStartAbund*scaledPSucc)+",
						"I(scaledDepletion*scaledStartAbund*scaledDiveLimit)+",
						"I(scaledDepletion*scaledStartAbund*scaledPctRecover)+",
						"I(scaledDepletion*scaledPSucc*scaledDiveLimit)+",
						"I(scaledDepletion*scaledPSucc*scaledPctRecover)+",
						"I(scaledDepletion*scaledDiveLimit*scaledPctRecover)+",
						"I(scaledStartAbund*scaledPSucc*scaledDiveLimit)+",
						"I(scaledStartAbund*scaledPSucc*scaledPctRecover)+",
						"I(scaledStartAbund*scaledDiveLimit*scaledPctRecover)+",
						"I(scaledPSucc*scaledDiveLimit*scaledPctRecover)+",
						"I(scaledDepletion*scaledStartAbund*scaledPSucc*scaledDiveLimit)+",
						"I(scaledDepletion*scaledStartAbund*scaledPSucc*scaledPctRecover)+",
						"I(scaledDepletion*scaledPSucc*scaledDiveLimit*scaledPctRecover)+",
						"I(scaledDepletion*scaledStartAbund*scaledDiveLimit*scaledPctRecover)+",
						"I(scaledDepletion*scaledStartAbund*scaledPSucc*scaledDiveLimit*scaledPctRecover)",
						sep="")),data=lambda.df)