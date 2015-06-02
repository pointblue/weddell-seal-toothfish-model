# TODO: Add comment
# 
# Author: lsalas
###############################################################################

library(ggplot2)

#########################################################################
#elasticities for mass data.
pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Results/diving/final/"

getSealMassTrend<-function(filen){
	wght.df<-data.frame()
	for(fff in filen){
		#get the params from the file name
		dL<-substr(fff,1,regexpr("_",fff,fixed=TRUE)-2)
		fil<-paste(pth,fff,sep="")
		load(fil)
		data.df$diveLim<-paste(dL,"%")
		wght.df<-rbind(wght.df,data.df)
	}
	wght.df<-subset(wght.df,cval==6)
	res2.df<-data.frame()
	for(ss in c(0.25,0.35)){
		for(tt in c(0.3,0.4)){
			for(mm in c("50 %","62.5 %","80 %")){
				for(dd in c(-0.0082,-0.0198,-0.0396)){
					tmp<-subset(wght.df,pSucc==ss & startTF==tt & diveLim==mm & rateTF==dd)
					y1<-subset(tmp,year<4,select="weightGainedKg")
					wtstart<-as.numeric(mean(y1$weightGainedKg))
					y35<-subset(tmp,year>(max(tmp$year)-4),select="weightGainedKg")
					wtend<-as.numeric(mean(y35$weightGainedKg))
					tdf<-data.frame(pSucc=ss, startTF=tt, diveLim=mm, rateTF=dd, weightLost=wtend) 
					res2.df<-rbind(res2.df,tdf)
				}
			}
		}
	}
	
	data.df<-res2.df
	data.df$pSucc<-as.factor(as.character(data.df$pSucc));data.df$diveLim<-as.factor(as.character(data.df$diveLim))
	data.df$startTF<-as.factor(as.character(data.df$startTF));data.df$rateTF<-as.factor(as.character(data.df$rateTF))
	
	return(data.df)
}

filen<-list.files(path = pth, pattern = "wtgain.RData", full.names = FALSE)
weight.df<-getSealMassTrend(filen)

weight.df$depletion<-ifelse(weight.df$rateTF==-0.0082,"Low depletion",ifelse(weight.df$rateTF==-0.0198,"Med. depletion","High depletion"))
weight.df$diveLimit<-ifelse(weight.df$diveLim=="80 %",0.80,ifelse(weight.df$diveLim=="50 %",0.50,0.625))		#0.246,ifelse(lambda.df$diveLimit=="50% of day",-0.246,-0.026)


#### Scaling...
weight.df$depletionPercent<-as.numeric(scale(as.numeric(as.character(weight.df$rateTF))))
weight.df$stAbundancePercent<-as.numeric(scale(as.numeric(as.character(weight.df$startTF))))
weight.df$pSuccPercent<-as.numeric(scale(as.numeric(as.character(weight.df$pSucc))))
weight.df$diveLimPercent<-as.numeric(scale(weight.df$diveLimit))

## simple model - full model below
sens_pct<-lm(as.formula("weightLost~depletionPercent+stAbundancePercent+pSuccPercent+diveLimPercent"),data=weight.df)

pars<-c("depletionPercent","stAbundancePercent","pSuccPercent","diveLimPercent")
getElasticities<-function(mdl,pars,par,df,change=10){
	opars<-pars[grepl(par,pars)==FALSE]
	uv<-unique(df[,par])
	epar<-seq.int(from=min(uv), to=max(uv), length.out=99)
	epar<-c(epar,0)
	pred.df<-data.frame(epar=epar,v1=rep(0,times=100),v2=rep(0,times=100),v3=rep(0,times=100))	#params have been standardized, so mean is 0
	names(pred.df)<-c(par,opars)
	pred.df$predicted<-predict(mdl,newdata=pred.df)	#predicting mass gain
	pred.df<-pred.df[order(pred.df$predicted),]
	at0<-subset(pred.df,pred.df[,par]==0); at0<-at0[1,]	#predicted value at mean of par
	at0p<-at0$predicted
	ulim<-at0p+change;llim<-at0p-change	#1% change in wt gain...
	usub<-subset(pred.df,predicted>ulim); lsub<-subset(pred.df,predicted<llim)
	ul<-round(usub[1,par],2)
	return(c(par,ul))
}

dd1<-getElasticities(mdl=sens_pct,pars=pars,par="depletionPercent",df=weight.df)
dd2<-getElasticities(mdl=sens_pct,pars=pars,par="stAbundancePercent",df=weight.df)
dd3<-getElasticities(mdl=sens_pct,pars=pars,par="pSuccPercent",df=weight.df)
dd4<-getElasticities(mdl=sens_pct,pars=pars,par="diveLimPercent",df=weight.df)

data<-data.frame(rbind(dd1,dd2,dd3,dd4))	
names(data)<-c("Parameter","Elasticity")
data$ParameterName<-c("Toothfish depl. rate","Starting Abundance","% dive success","Daily dive limit")
data$Elasticity<-as.numeric(as.character(data$Elasticity))

#subset(data,!ParameterName %in% c("% dive success","Daily dive limit"))
#must subset to remove % dive success and daily dive limit, as they have very small effect on differences in mass lost
pelas<-ggplot(data=data,aes(x=ParameterName,y=Elasticity)) + geom_bar(stat="identity", width=0.8) +
		theme_bw() + 
		theme(axis.text.x=element_text(size=10, angle=20, hjust=1)) +
		labs(x="Parameter",y="Elasticity (units of SD)")

jpeg(filename="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/antarctica/paper/figures/FigureS3_elasticitiesMass.jpg",
		width=400, height=350,quality=100)
print(pelas)
dev.off()


## Partial r-squared
sse_full<-deviance(sens_pct)
mdl_dplt<-lm(as.formula("weightLost~stAbundancePercent+pSuccPercent+diveLimPercent"),data=weight.df); sse_dplt<-deviance(mdl_dplt)
mdl_stab<-lm(as.formula("weightLost~depletionPercent+pSuccPercent+diveLimPercent"),data=weight.df); sse_stab<-deviance(mdl_stab)
mdl_psuc<-lm(as.formula("weightLost~depletionPercent+stAbundancePercent+diveLimPercent"),data=weight.df); sse_psuc<-deviance(mdl_psuc)
mdl_dvlm<-lm(as.formula("weightLost~depletionPercent+stAbundancePercent+pSuccPercent"),data=weight.df); sse_dvlm<-deviance(mdl_dvlm)

part_r2<-data.frame(Parameter=pars,Partial_r2=c(
				((sse_dplt-sse_full)/sse_dplt),
				((sse_stab-sse_full)/sse_stab),
				((sse_psuc-sse_full)/sse_psuc),
				((sse_dvlm-sse_full)/sse_dvlm)))

######################
## FULL MODEL
sens_pct<-lm(as.formula(paste("weightLost~depletionPercent+stAbundancePercent+pSuccPercent+diveLimPercent+",
						"I(depletionPercent*stAbundancePercent)+",
						"I(depletionPercent*pSuccPercent)+",
						"I(depletionPercent*diveLimPercent)+",
						"I(stAbundancePercent*pSuccPercent)+",
						"I(stAbundancePercent*diveLimPercent)+",
						"I(pSuccPercent*diveLimPercent)+",
						"I(depletionPercent*stAbundancePercent*pSuccPercent)+",
						"I(depletionPercent*stAbundancePercent*diveLimPercent)+",
						"I(depletionPercent*pSuccPercent*diveLimPercent)+",
						"I(stAbundancePercent*pSuccPercent*diveLimPercent)+",
						"I(depletionPercent*stAbundancePercent*pSuccPercent*diveLimPercent)",
						sep="")),data=weight.df)
