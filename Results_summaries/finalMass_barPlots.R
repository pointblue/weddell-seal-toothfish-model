# TODO: Add comment
# 
# Author: lsalas
###############################################################################


library(ggplot2)
library(splines)

initAbund<-c(0.3, 0.4)
pth<-"//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/Antarctica/Results/diving/final/"

makeTFtrenddf<-function(startTF,rateTF,time.int=35){
	dd<-data.frame()
	for(tt in 0:(time.int-1)){
		mv<-startTF*exp(rateTF*tt)		#startTF.abund*
		tdf<-data.frame(year=(tt+1),startTF=startTF,rateTF=rateTF,TFabund=mv)
		dd<-rbind(dd,tdf)
	}
	return(dd)
}

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
	
	#wght.df$release<-ifelse(wght.df$cval==0,"No release",ifelse(wght.df$cval==5,"Low release","High release"))
	wght.df<-subset(wght.df,cval==6)
	
	res2.df<-data.frame()
	for(ss in c(0.25,0.35)){
		for(tt in c(0.3,0.4)){
			for(cc in c("50 %","62.5 %","80 %")){	#using dive limits, not cvals
				for(dd in c(-0.0082,-0.0198,-0.0396)){
					res.tmp<-data.frame()
					for(bb in 1:200){
						tmp<-subset(wght.df,pSucc==ss & startTF==tt & diveLim==cc & rateTF==dd & boot==bb)
						ymx<-max(tmp$year)
						tmpy<-subset(tmp,year==ymx,select="weightGainedKg")
						res.tmp<-rbind(res.tmp,tmpy)
					}
					fWeight<-mean(res.tmp$weightGainedKg)
					sdfWeight<-sd(res.tmp$weightGainedKg)
					rdf<-data.frame(pSucc=ss,startTF=tt,diveLim=cc,rateTF=dd,year=35,fWeight=fWeight,sdfWeight=sdfWeight)
					res2.df<-rbind(res2.df,rdf)
				}
			}
		}
	}
	
	meta<-unique(res2.df[,c("startTF","rateTF")])
	meta.df<-data.frame()
	for(rr in 1:nrow(meta)){
		sttf<-meta[rr,"startTF"]; rttf<-meta[rr,"rateTF"]
		tmp<-makeTFtrenddf(startTF=sttf,rateTF=rttf,time.int=35)
		meta.df<-rbind(meta.df,tmp)
	}
	
	res.df<-merge(res2.df,meta.df,by=c("year","startTF","rateTF"))
	res.df$pSucc<-as.factor(as.character(res.df$pSucc));res.df$diveLim<-as.factor(as.character(res.df$diveLim))
	res.df$startTF<-as.factor(as.character(res.df$startTF));res.df$rateTF<-as.factor(as.character(res.df$rateTF))
	
	return(res.df)
}

filen<-list.files(path=pth,pattern="_wtgain.RData")
df<-getSealMassTrend(filen)
df$depletion<-ifelse(df$rateTF==-0.0082,"Low depletion",ifelse(df$rateTF==-0.0198,"Med. depletion","High depletion"))
df$depletion<-reorder(df$depletion,as.numeric(as.character(df$rateTF)))
df$ucl<-df$fWeight+df$sdfWeight; df$lcl<-df$fWeight-df$sdfWeight; df$lcl<-ifelse(df$lcl<0,0,df$lcl)

for(aaa in initAbund){
	tdf<-subset(df,startTF==aaa)
	p<-ggplot(data=tdf,aes(x=pSucc,y=fWeight)) + 
			geom_errorbar(aes(ymax=ucl,ymin=lcl)) +
			geom_bar(stat="identity") +
			theme_bw() + labs(x="Prob. successful dive",y="Mass gained (kg)") +	#,title=paste("Starting abundance:",aaa)
			facet_grid(diveLim~depletion)
	dev.new()
	print(p)
	png(filename=paste("//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/antarctica/paper/figures/figure5_",aaa,"_barPlot.png",sep=""),
			width=400, height=300)
	print(p)
	dev.off()
}

