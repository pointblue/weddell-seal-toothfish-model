# TODO: Add comment
# 
# Author: lsalas
###############################################################################

library(ggplot2)

#################################################################################################################################
## Paper figure 6 faceted


########## Original #########################
source("c:/users/lsalas/workspace/PRBOGeneralUpdate/WESE/Single_TFconsumpt_BC_rel/Results/Graphs/compile_finalData.R")
lambda.df<-getF6_elasticitiesData()

lambda.df$Scenario<-as.factor(paste(lambda.df$pSucc,"        ", lambda.df$startAbund, "       ",lambda.df$pRecov,sep=""))
lambda.df$lbdMarker<-ifelse(lambda.df$lambda<0.985511,"L","H")
lambda.df$endnMarker<-ifelse(lambda.df$endN<600,"L","H")

ord.df<-data.frame(Scenario=c("35%        0.4       60%","35%        0.4       70%","35%        0.4       80%",
		"35%        0.3       60%","35%        0.3       70%","35%        0.3       80%",
		"25%        0.4       60%","25%        0.4       70%","25%        0.4       80%",
		"25%        0.3       60%","25%        0.3       70%","25%        0.3       80%"),ordn=c(12:1))
lambda.df<-merge(lambda.df,ord.df,by="Scenario")

p1<-ggplot(data=lambda.df,aes(x=reorder(Scenario,ordn),y=lambda)) + 		
		geom_pointrange(size=0.8,aes(ymin=lclambda,ymax=uclambda,color=lbdMarker)) + 
		coord_flip() + theme_bw() + labs(x="Scenario", y="Lambda") + theme(axis.text.y=element_text(size=10, family=c("serif"),hjust=0)) +
		scale_y_continuous(breaks=c(0.82,0.86,0.9,0.94,0.98)) + scale_colour_manual(values=c("dark gray", "dark blue")) + theme(legend.position="none") +
		facet_grid(deplRate~diveLimit)

p2<-ggplot(data=lambda.df,aes(x=reorder(Scenario,ordn),y=endN)) + geom_bar(aes(fill=endnMarker),stat="identity") + coord_flip() + theme_bw() + 
		labs(y="Ending number of seals",x="Scenario") + theme(axis.text.y=element_text(size=10, family=c("serif"),hjust=0)) +
		scale_y_continuous(breaks=c(0,200,400,600,800)) + scale_fill_manual(values=c("dark gray", "dark blue")) + theme(legend.position="none") +
		facet_grid(deplRate~diveLimit)

#add shading
p1<-ggplot(data=lambda.df,aes(x=reorder(Scenario,ordn),y=lambda)) + 	
		geom_rect(xmin=0.5,xmax=6.5,ymin=0.79,ymax=1.02,colour="light gray",fill="light gray",alpha=0.05) +
		#geom_text(x=6,y=0.81,colour="dark blue",size=2.5,label="Start abund. 0.4", hjust=0,alpha=0.05) +
		geom_pointrange(size=0.8,aes(ymin=lclambda,ymax=uclambda,color=lbdMarker)) + 
		coord_flip() + theme_bw() + labs(x="Scenario", y="Lambda") + theme(axis.text.y=element_text(size=10, family=c("serif"),hjust=0)) +
		scale_y_continuous(breaks=c(0.82,0.86,0.9,0.94,0.98)) + scale_colour_manual(values=c("dark gray", "dark blue")) + theme(legend.position="none") +
		facet_grid(deplRate~diveLimit)

p2<-ggplot(data=lambda.df,aes(x=reorder(Scenario,ordn),y=endN)) + 
		geom_rect(xmin=0.5,xmax=6.5,ymin=-50,ymax=1010,colour="light gray",fill="light gray",alpha=0.05) +
		geom_bar(aes(fill=endnMarker),stat="identity") + coord_flip() + theme_bw() + 
		labs(y="Ending number of seals",x="Scenario") + theme(axis.text.y=element_text(size=10, family=c("serif"),hjust=0)) +
		scale_y_continuous(breaks=c(0,200,400,600,800)) + scale_fill_manual(values=c("dark gray", "dark blue")) + theme(legend.position="none") +
		facet_grid(deplRate~diveLimit)

jpeg(filename="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/antarctica/paper/figures/figure7A.jpg",
		width=550, height=550,quality=100)
print(p1)
dev.off()

jpeg(filename="//prbo.org/Data/Home/Petaluma/lsalas/Documents/lsalas/antarctica/paper/figures/figure7B.jpg",
		width=550, height=550,quality=100)
print(p2)
dev.off()


######### Simplified? #####################
source("c:/users/lsalas/workspace/PRBOGeneralUpdate/WESE/Single_TFconsumpt_BC_rel/Results/Graphs/compile_finalData.R")
lambda.df<-getF6_elasticitiesData()
lambda.df<-subset(lambda.df,pRecov=="70%")

lambda.df$Scenario<-paste(lambda.df$pSucc,"        ", lambda.df$startAbund,sep="")
lambda.df$lbdMarker<-ifelse(lambda.df$lambda<0.98,"L","H")
lambda.df$endnMarker<-ifelse(lambda.df$endN<800,"L","H")

lambda.df$startAbund<-as.character(lambda.df$startAbund)
stab<-c("0.3","0.4")

for(sss in stab){
	p1<-ggplot(data=subset(lambda.df,startAbund==sss),aes(x=pSucc,y=lambda)) + 
			geom_pointrange(size=0.8,aes(ymin=lclambda,ymax=uclambda,color=lbdMarker)) + 
			coord_flip() + theme_bw() + labs(x="Prob. Success", y="Lambda", title=paste("Starting toothfish abundance:",sss)) + theme(axis.text.y=element_text(size=10, family=c("serif"),hjust=0)) +
			scale_y_continuous(breaks=c(0.82,0.86,0.9,0.94,0.98), limits=c(0.82,1)) + scale_colour_manual(values=c("dark gray", "dark blue")) + theme(legend.position="none") +
			facet_grid(deplRate~diveLimit)
	
	p2<-ggplot(data=subset(lambda.df,startAbund==sss),aes(x=pSucc,y=endN)) + geom_bar(aes(fill=endnMarker),stat="identity") + coord_flip() + theme_bw() + 
			labs(y="Ending number of seals",x="Prob. Success",title=paste("Starting toothfish abundance:",sss)) + theme(axis.text.y=element_text(size=10, family=c("serif"),hjust=0)) +
			scale_y_continuous(breaks=c(0,200,400,600,800)) + scale_fill_manual(values=c("dark gray", "dark blue")) + theme(legend.position="none") +
			facet_grid(deplRate~diveLimit)
	dev.new();print(p1)
	dev.new();print(p2)
}
