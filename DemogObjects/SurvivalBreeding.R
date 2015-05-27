# TODO: Add comment
# 
# Author: lsalas
###############################################################################

## This file sets the SurvivalBreeding object for the simple WESE simulation
#' Abstract class for SurvivalBreeding
#' 
#' Abstract class for SurvivalBreeding
#'
#' @slot CurrentSB  A 10x10 matrix object with the current survBreeding matrix
#' @slot ModelsSB A list object, each entry contains a model relating each parameter of the survBreeding matrix to WESE body condition
#' @slot HistorySB A list object that holds the history of survBreeding matrices by time step
#' @exportClass SurvivalBreeding
setClass(Class="SurvivalBreeding", representation(
				CurrentSB = "matrix",
				ModelsSB = "list",
				HistorySB = "list"
		))

############################################ SLOT METHODS #######################################
########################## Set CurrentSB slot
#' Set generic to  method that sets the CurrentSB slot of the SurvivalBreeding object.
#' 
#' @name setCurrentSB
#' @param object A SurvivalBreeding object
#' @param value The current survBreeding matrix to put into the CurrentSB slot
#' @nord
setGeneric("CurrentSB<-", 
		function(object, value)	standardGeneric("CurrentSB<-"))

#' Set the CurrentSB slot of a SurvivalBreeding object.
#' 
#' @name setCurrentSB
#' @param object A SurvivalBreeding object
#' @param value The current survBreeding matrix to put into the CurrentSB slot

setReplaceMethod("CurrentSB",signature(object="SurvivalBreeding"),
		function(object,value) {
			slot(object,"CurrentSB")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the CurrentSB slot value of a SurvivalBreeding object.
#' 
#' @name CurrentSB
#' @param object A SurvivalBreeding object
#' @nord
setGeneric("CurrentSB", 
		function(object) standardGeneric("CurrentSB"))

#' Retrieve the CurrentSB slot value of a SurvivalBreeding object.
#' 
#' @name CurrentSB
#' @param object A SurvivalBreeding object

setMethod("CurrentSB", signature(object="SurvivalBreeding"),
		function(object) slot(object,"CurrentSB"))

##########################
########################## Set ModelsSB slot
#' Set generic to  method that sets the ModelsSB slot of the SurvivalBreeding object.
#' 
#' @name setModelsSB
#' @param object A SurvivalBreeding object
#' @param value A list object, each entry contains a model relating each parameter of the survBreeding matrix to WESE body condition, to put into the ModelsSB slot
#' @nord
setGeneric("ModelsSB<-", 
		function(object, value)	standardGeneric("ModelsSB<-"))

#' Set the ModelsSB slot of a SurvivalBreeding object.
#' 
#' @name setModelsSB
#' @param object A SurvivalBreeding object
#' @param value A list object, each entry contains a model relating each parameter of the survBreeding matrix to WESE body condition, to put into the ModelsSB slot

setReplaceMethod("ModelsSB",signature(object="SurvivalBreeding"),
		function(object,value) {
			slot(object,"ModelsSB")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the ModelsSB slot value of a SurvivalBreeding object.
#' 
#' @name ModelsSB
#' @param object A SurvivalBreeding object
#' @nord
setGeneric("ModelsSB", 
		function(object) standardGeneric("ModelsSB"))

#' Retrieve the ModelsSB slot value of a SurvivalBreeding object.
#' 
#' @name ModelsSB
#' @param object A SurvivalBreeding object

setMethod("ModelsSB", signature(object="SurvivalBreeding"),
		function(object) slot(object,"ModelsSB"))

##########################
########################## Set HistorySB slot
#' Set generic to  method that sets the HistorySB slot of the SurvivalBreeding object.
#' 
#' @name setHistorySB
#' @param object A SurvivalBreeding object
#' @param value A list object that holds the history of survBreeding matrices by time step
#' @nord
setGeneric("HistorySB<-", 
		function(object, value)	standardGeneric("HistorySB<-"))

#' Set the HistorySB slot of a SurvivalBreeding object.
#' 
#' @name setHistorySB
#' @param object A SurvivalBreeding object
#' @param value A list object that holds the history of survBreeding matrices by time step

setReplaceMethod("HistorySB",signature(object="SurvivalBreeding"),
		function(object,value) {
			slot(object,"HistorySB")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the HistorySB slot value of a SurvivalBreeding object.
#' 
#' @name HistorySB
#' @param object A SurvivalBreeding object
#' @nord
setGeneric("HistorySB", 
		function(object) standardGeneric("HistorySB"))

#' Retrieve the HistorySB slot value of a SurvivalBreeding object.
#' 
#' @name HistorySB
#' @param object A SurvivalBreeding object

setMethod("HistorySB", signature(object="SurvivalBreeding"),
		function(object) slot(object,"HistorySB"))

##########################

############################################ INITIALIZE ####################################################
#' Instantiate a new SurvivalBreeding object
#' 
#' @name initialize
#' @nord
#' @exportMethod initialize 
setMethod("initialize",
		signature(.Object = "SurvivalBreeding"),
		function (.Object, ...) 
		{
			.Object@CurrentSB<-matrix()
			.Object@ModelsSB<-list()
			.Object@HistorySB<-list()
			.Object
		}
)

############################################ SURVIVALBREEDING METHODS #######################################
########################## Create current survBreeding, update history of survBreeding
#' Set generic to  method that creates current survBreeding matrix from a sampled value of WESE body condition
#' 
#' @name UpdateSB
#' @param object A SurvivalBreeding object
setGeneric("UpdateSB",
		function(object,  ...) standardGeneric("UpdateSB"))

#' Create current survBreeding matrix from from a sampled value of WESE body condition
#' 
#' @param object A SurvivalBreeding object
#' @param bodyCond Numeric. A sampled value of body condition
#' @param scalePar Numeric. The value of the scale parameter to adjust the value of the demographic parameter.
#' @param timestep Integer, a value for the current timestep
setMethod("UpdateSB", signature(object = "SurvivalBreeding"),
		function(object, bodyCond,scalePar=1, timestep) {
			ea.call<-match.call()
			if (is.null(object)) stop("A SurvivalBreeding object is required.")
			if (is.null(bodyCond)) stop("A value of WESE body condition is required.")
			if (is.null(timestep) | (class(timestep)!="integer")) stop("A valid timestep value is required.")
			histLst<-HistorySB(object)
			#NOTE: 	The first survBreeding matrix is provided by direct assignment to the slot - all other updates go though this method
			#		Thus, HistorySB starts with length 1, not 0. As a consequence, the length of histLst should always match the timestep-1
			if(length(histLst)!=timestep-1)stop("The timestep-1 value does not match the current lenght of the HistorySB list.")
			
			## The survBreeding matrix has values for the sub-diagonal and in the [10,10] cell (for SO), all else is 0's
			## The functions in the ModelsSB slot are, however, generated by a general function and apply to all cells in the matrix
			mdls<-ModelsSB(object)
			nr<-length(mdls);nc<-length(mdls[[1]])
			if(nr!=nc) stop("The number of row and columns in the models object are not the same.")
			tmpPredict<-numeric()
			for(rr in 1:nr){
				for(cc in 1:nc){
					mdl<-mdls[[rr]][[cc]]
					if(class(mdl)=="numeric"){
						res<-mdl
					}else{
						res.pred<-predict(mdl,newdata=data.frame(bodyCond=bodyCond),se.fit=FALSE)	#careful here to ensure we are getting a vector
						res<-res.pred[1]; res<-res*scalePar
					}
					tmpPredict<-c(tmpPredict,res)
				}
			}
			newsurvBreeding<-try(matrix(tmpPredict,nrow=nr,ncol=nc,byrow=TRUE),silent=TRUE)
			if(inherits(newsurvBreeding,"try-error")){
				stop("Failed to generate the new survBreeding matrix - check input body condition and models")
			}else{
				nstp<-timestep
				histLst[[nstp]]<-newsurvBreeding
				HistorySB(object)<-histLst
				CurrentSB(object)<-newsurvBreeding
			}
			return(object)
		}
)
########################## 
########################## Create trend table of SurvivalBreeding
#' Set generic to a method that creates output tables from the history of survBreeding (HistorySB slot) in the object
#' 
#' @name SummarizeSB
#' @param object A SurvivalBreeding object
setGeneric("SummarizeSB",
		function(object,  ...) standardGeneric("SummarizeSB"))

#' A method that creates output tables from the history of survBreeding (HistorySB slot) in the object
#' 
#' @param object A SurvivalBreeding object
setMethod("SummarizeSB", signature(object = "SurvivalBreeding"),
		function(object) {
			ea.call<-match.call()
			if (is.null(object)) stop("A SurvivalBreeding object is required.")
			histLst<-HistorySB(object)
			if(length(histLst)<5)stop("The HistorySB list has very little or no data. Check to see that a simulation has been run.")
			## NOTE: the survBreeding matrix has values of survBreeding for the subdiagonal and cell[10,10]. Those are compiled here.
			by.cat<-data.frame()
			for(ii in 1:length(histLst)){
				tmp.mx<-histLst[[ii]]
				nr<-dim(tmp.mx)[1];nc<-dim(tmp.mx)[2]
				dd<-numeric()
				for(rr in 2:nr){
					for(cc in 1:(nc-1)){
						if(cc==rr-1) dd<-c(dd,tmp.mx[rr,cc])
					}
				}
				dd<-c(dd,tmp.mx[nr,nc])
				nr<-NROW(dd);tm<-rep(ii,times=nr);agecats<-paste("Age",1:(nr),sep="")
				dat<-data.frame(time=tm,age=agecats,survBreed=dd)
				by.cat<-rbind(by.cat,dat)
			}
			return(by.cat)
		}
)
########################## 





