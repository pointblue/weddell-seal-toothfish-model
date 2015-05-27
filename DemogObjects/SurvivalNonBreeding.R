# TODO: Add comment
# 
# Author: lsalas
###############################################################################

## This file sets the SurvivalNonBreeding object for the simple WESE simulation
#' Abstract class for SurvivalNonBreeding
#' 
#' Abstract class for SurvivalNonBreeding
#'
#' @slot CurrentSNB  A 10x10 matrix object with the current survNonBreeding matrix
#' @slot ModelsSNB A list object, each entry contains a model relating each parameter of the survNonBreeding matrix to WESE body condition
#' @slot HistorySNB A list object that holds the history of survNonBreeding matrices by time step
#' @exportClass SurvivalNonBreeding
setClass(Class="SurvivalNonBreeding", representation(
				CurrentSNB = "matrix",
				ModelsSNB = "list",
				HistorySNB = "list"
		))

############################################ SLOT METHODS #######################################
########################## Set CurrentSNB slot
#' Set generic to  method that sets the CurrentSNB slot of the SurvivalNonBreeding object.
#' 
#' @name setCurrentSNB
#' @param object A SurvivalNonBreeding object
#' @param value The current survNonBreeding matrix to put into the CurrentSNB slot
#' @nord
setGeneric("CurrentSNB<-", 
		function(object, value)	standardGeneric("CurrentSNB<-"))

#' Set the CurrentSNB slot of a SurvivalNonBreeding object.
#' 
#' @name setCurrentSNB
#' @param object A SurvivalNonBreeding object
#' @param value The current survNonBreeding matrix to put into the CurrentSNB slot

setReplaceMethod("CurrentSNB",signature(object="SurvivalNonBreeding"),
		function(object,value) {
			slot(object,"CurrentSNB")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the CurrentSNB slot value of a SurvivalNonBreeding object.
#' 
#' @name CurrentSNB
#' @param object A SurvivalNonBreeding object
#' @nord
setGeneric("CurrentSNB", 
		function(object) standardGeneric("CurrentSNB"))

#' Retrieve the CurrentSNB slot value of a SurvivalNonBreeding object.
#' 
#' @name CurrentSNB
#' @param object A SurvivalNonBreeding object

setMethod("CurrentSNB", signature(object="SurvivalNonBreeding"),
		function(object) slot(object,"CurrentSNB"))

##########################
########################## Set ModelsSNB slot
#' Set generic to  method that sets the ModelsSNB slot of the SurvivalNonBreeding object.
#' 
#' @name setModelsSNB
#' @param object A SurvivalNonBreeding object
#' @param value A list object, each entry contains a model relating each parameter of the survNonBreeding matrix to WESE body condition, to put into the ModelsSNB slot
#' @nord
setGeneric("ModelsSNB<-", 
		function(object, value)	standardGeneric("ModelsSNB<-"))

#' Set the ModelsSNB slot of a SurvivalNonBreeding object.
#' 
#' @name setModelsSNB
#' @param object A SurvivalNonBreeding object
#' @param value A list object, each entry contains a model relating each parameter of the survNonBreeding matrix to WESE body condition, to put into the ModelsSNB slot

setReplaceMethod("ModelsSNB",signature(object="SurvivalNonBreeding"),
		function(object,value) {
			slot(object,"ModelsSNB")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the ModelsSNB slot value of a SurvivalNonBreeding object.
#' 
#' @name ModelsSNB
#' @param object A SurvivalNonBreeding object
#' @nord
setGeneric("ModelsSNB", 
		function(object) standardGeneric("ModelsSNB"))

#' Retrieve the ModelsSNB slot value of a SurvivalNonBreeding object.
#' 
#' @name ModelsSNB
#' @param object A SurvivalNonBreeding object

setMethod("ModelsSNB", signature(object="SurvivalNonBreeding"),
		function(object) slot(object,"ModelsSNB"))

##########################
########################## Set HistorySNB slot
#' Set generic to  method that sets the HistorySNB slot of the SurvivalNonBreeding object.
#' 
#' @name setHistorySNB
#' @param object A SurvivalNonBreeding object
#' @param value A list object that holds the history of survNonBreeding matrices by time step
#' @nord
setGeneric("HistorySNB<-", 
		function(object, value)	standardGeneric("HistorySNB<-"))

#' Set the HistorySNB slot of a SurvivalNonBreeding object.
#' 
#' @name setHistorySNB
#' @param object A SurvivalNonBreeding object
#' @param value A list object that holds the history of survNonBreeding matrices by time step

setReplaceMethod("HistorySNB",signature(object="SurvivalNonBreeding"),
		function(object,value) {
			slot(object,"HistorySNB")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the HistorySNB slot value of a SurvivalNonBreeding object.
#' 
#' @name HistorySNB
#' @param object A SurvivalNonBreeding object
#' @nord
setGeneric("HistorySNB", 
		function(object) standardGeneric("HistorySNB"))

#' Retrieve the HistorySNB slot value of a SurvivalNonBreeding object.
#' 
#' @name HistorySNB
#' @param object A SurvivalNonBreeding object

setMethod("HistorySNB", signature(object="SurvivalNonBreeding"),
		function(object) slot(object,"HistorySNB"))

##########################

############################################ INITIALIZE ####################################################
#' Instantiate a new SurvivalNonBreeding object
#' 
#' @name initialize
#' @nord
#' @exportMethod initialize 
setMethod("initialize",
		signature(.Object = "SurvivalNonBreeding"),
		function (.Object, ...) 
		{
			.Object@CurrentSNB<-matrix()
			.Object@ModelsSNB<-list()
			.Object@HistorySNB<-list()
			.Object
		}
)

############################################ SURVIVALBREEDING METHODS #######################################
########################## Create current survNonBreeding, update history of survNonBreeding
#' Set generic to  method that creates current survNonBreeding matrix from a sampled value of WESE body condition
#' 
#' @name UpdateSNB
#' @param object A SurvivalNonBreeding object
setGeneric("UpdateSNB",
		function(object,  ...) standardGeneric("UpdateSNB"))

#' Create current survNonBreeding matrix from from a sampled value of WESE body condition
#' 
#' @param object A SurvivalNonBreeding object
#' @param bodyCond Numeric. A sampled value of body condition
#' @param scalePar Numeric. The value of the scale parameter to adjust the value of the demographic parameter.
#' @param timestep Integer, a value for the current timestep
setMethod("UpdateSNB", signature(object = "SurvivalNonBreeding"),
		function(object, bodyCond,scalePar=1, timestep) {
			ea.call<-match.call()
			if (is.null(object)) stop("A SurvivalNonBreeding object is required.")
			if (is.null(bodyCond)) stop("A value of WESE body condition is required.")
			if (is.null(timestep) | (class(timestep)!="integer")) stop("A valid timestep value is required.")
			histLst<-HistorySNB(object)
			#NOTE: 	The first survNonBreeding matrix is provided by direct assignment to the slot - all other updates go though this method
			#		Thus, HistorySNB starts with length 1, not 0. As a consequence, the length of histLst should always match the timestep-1
			if(length(histLst)!=timestep-1)stop("The timestep-1 value does not match the current lenght of the HistorySNB list.")
			
			## The survNonBreeding matrix has values for the diagonal, all else is 0's
			## The functions in the ModelsSNB slot are, however, generated by a general function and apply to all cells in the matrix
			mdls<-ModelsSNB(object)
			nr<-length(mdls);nc<-length(mdls[[1]])
			if(nr!=nc) stop("The number of row and columns in the models object are not the same.")
			tmpPredict<-numeric()
			for(rr in 1:nr){
				for(cc in 1:nc){
					mdl<-mdls[[rr]][[cc]]
					if(class(mdl)[1]=="numeric"){
						res<-mdl
					}else{
						res.pred<-predict(mdl,newdata=data.frame(bodyCond=bodyCond),se.fit=FALSE)	#careful here to ensure we are getting a vector
						res<-res.pred[1]; 
						if(rr==1 & cc==1){res<-res*scalePar}
					}
					tmpPredict<-c(tmpPredict,res)
				}
			}
			newsurvNonBreeding<-try(matrix(tmpPredict,nrow=nr,ncol=nc,byrow=TRUE),silent=TRUE)
			if(inherits(newsurvNonBreeding,"try-error")){
				stop("Failed to generate the new survNonBreeding matrix - check input body condition and models")
			}else{
				nstp<-timestep
				histLst[[nstp]]<-newsurvNonBreeding
				HistorySNB(object)<-histLst
				CurrentSNB(object)<-newsurvNonBreeding
			}
			return(object)
		}
)
########################## 
########################## Create trend table of SurvivalNonBreeding
#' Set generic to a method that creates output tables from the history of survNonBreeding (HistorySNB slot) in the object
#' 
#' @name SummarizeSNB
#' @param object A SurvivalNonBreeding object
setGeneric("SummarizeSNB",
		function(object,  ...) standardGeneric("SummarizeSNB"))

#' A method that creates output tables from tthe history of survNonBreeding (HistorySNB slot) in the object
#' 
#' @param object A SurvivalNonBreeding object
setMethod("SummarizeSNB", signature(object = "SurvivalNonBreeding"),
		function(object) {
			ea.call<-match.call()
			if (is.null(object)) stop("A SurvivalNonBreeding object is required.")
			histLst<-HistorySNB(object)
			if(length(histLst)<5)stop("The HistorySNB list has very little or no data. Check to see that a simulation has been run.")
			## NOTE: the survNonBreeding matrix has values of survNonBreeding for the diagonal. Those are compiled here.
			by.cat<-data.frame()
			for(ii in 1:length(histLst)){
				tmp.mx<-histLst[[ii]]
				dd<-diag(tmp.mx)
				nr<-NROW(dd);tm<-rep(ii,times=nr);agecats<-paste("Age",1:(nr),sep="")
				dat<-data.frame(time=tm,age=agecats,survBreed=dd)
				by.cat<-rbind(by.cat,dat)
			}
			return(by.cat)
		}
)
########################## 






