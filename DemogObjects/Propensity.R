# TODO: Add comment
# 
# Author: lsalas
###############################################################################

## This file sets the Propensity object for the simple WESE simulation
#' Abstract class for Propensity
#' 
#' Abstract class for Propensity
#'
#' @slot CurrentProp  A 10x10 matrix object with the current breeding propensity matrix
#' @slot ModelsProp A list object, each entry contains a model relating each parameter of the propensity matrix to WESE body condition
#' @slot HistoryProp A list object that holds the history of propensity matrices by time step
#' @exportClass Propensity
setClass(Class="Propensity", representation(
				CurrentProp = "matrix",
				ModelsProp = "list",
				HistoryProp = "list"
		))

############################################ SLOT METHODS #######################################
########################## Set CurrentProp slot
#' Set generic to  method that sets the CurrentProp slot of the Propensity object.
#' 
#' @name setCurrentProp
#' @param object A Propensity object
#' @param value The current Propensity matrix to put into the CurrentProp slot
#' @nord
setGeneric("CurrentProp<-", 
		function(object, value)	standardGeneric("CurrentProp<-"))

#' Set the CurrentProp slot of a Propensity object.
#' 
#' @name setCurrentProp
#' @param object A Propensity object
#' @param value The current Propensity matrix to put into the CurrentProp slot

setReplaceMethod("CurrentProp",signature(object="Propensity"),
		function(object,value) {
			slot(object,"CurrentProp")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the CurrentProp slot value of a Propensity object.
#' 
#' @name CurrentProp
#' @param object A Propensity object
#' @nord
setGeneric("CurrentProp", 
		function(object) standardGeneric("CurrentProp"))

#' Retrieve the CurrentProp slot value of a Propensity object.
#' 
#' @name CurrentProp
#' @param object A Propensity object

setMethod("CurrentProp", signature(object="Propensity"),
		function(object) slot(object,"CurrentProp"))

##########################
########################## Set ModelsProp slot
#' Set generic to  method that sets the ModelsProp slot of the Propensity object.
#' 
#' @name setModelsProp
#' @param object A Propensity object
#' @param value A list object, each entry contains a model relating each parameter of the propensity matrix to WESE body condition, to put into the ModelsProp slot
#' @nord
setGeneric("ModelsProp<-", 
		function(object, value)	standardGeneric("ModelsProp<-"))

#' Set the ModelsProp slot of a Propensity object.
#' 
#' @name setModelsProp
#' @param object A Propensity object
#' @param value A list object, each entry contains a model relating each parameter of the propensity matrix to WESE body condition, to put into the ModelsProp slot

setReplaceMethod("ModelsProp",signature(object="Propensity"),
		function(object,value) {
			slot(object,"ModelsProp")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the ModelsProp slot value of a Propensity object.
#' 
#' @name ModelsProp
#' @param object A Propensity object
#' @nord
setGeneric("ModelsProp", 
		function(object) standardGeneric("ModelsProp"))

#' Retrieve the ModelsProp slot value of a Propensity object.
#' 
#' @name ModelsProp
#' @param object A Propensity object

setMethod("ModelsProp", signature(object="Propensity"),
		function(object) slot(object,"ModelsProp"))

##########################
########################## Set HistoryProp slot
#' Set generic to  method that sets the HistoryProp slot of the Propensity object.
#' 
#' @name setHistoryProp
#' @param object A Propensity object
#' @param value A list object that holds the history of propensity matrices by time step
#' @nord
setGeneric("HistoryProp<-", 
		function(object, value)	standardGeneric("HistoryProp<-"))

#' Set the HistoryProp slot of a Propensity object.
#' 
#' @name setHistoryProp
#' @param object A Propensity object
#' @param value A list object that holds the history of propensity matrices by time step

setReplaceMethod("HistoryProp",signature(object="Propensity"),
		function(object,value) {
			slot(object,"HistoryProp")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the HistoryProp slot value of a Propensity object.
#' 
#' @name HistoryProp
#' @param object A Propensity object
#' @nord
setGeneric("HistoryProp", 
		function(object) standardGeneric("HistoryProp"))

#' Retrieve the HistoryProp slot value of a Propensity object.
#' 
#' @name HistoryProp
#' @param object A Propensity object

setMethod("HistoryProp", signature(object="Propensity"),
		function(object) slot(object,"HistoryProp"))

##########################

############################################ INITIALIZE ####################################################
#' Instantiate a new Propensity object
#' 
#' @name initialize
#' @nord
#' @exportMethod initialize 
setMethod("initialize",
		signature(.Object = "Propensity"),
		function (.Object, ...) 
		{
			.Object@CurrentProp<-matrix()
			.Object@ModelsProp<-list()
			.Object@HistoryProp<-list()
			.Object
		}
)

############################################ PROPENSITY METHODS #######################################
########################## Create current Propensity, update history of propensity
#' Set generic to  method that creates current Propensity matrix from a sampled value of WESE body condition
#' 
#' @name UpdateProp
#' @param object A Propensity object
setGeneric("UpdateProp",
		function(object,  ...) standardGeneric("UpdateProp"))

#' Create current Propensity matrix from from a sampled value of WESE body condition
#' 
#' @param object A Propensity object
#' @param bodyCond Numeric. A sampled value of body condition
#' @param scalePar Numeric. The value of the scale parameter to adjust the value of the demographic parameter.
#' @param timestep Integer, a value for the current timestep
setMethod("UpdateProp", signature(object = "Propensity"),
		function(object, bodyCond,scalePar=1, timestep) {
			ea.call<-match.call()
			if (is.null(object)) stop("A Propensity object is required.")
			if (is.null(bodyCond)) stop("A value of WESE body condition is required.")
			if (is.null(timestep) | (class(timestep)!="integer")) stop("A valid timestep value is required.")
			histLst<-HistoryProp(object)
			#NOTE: 	The first propensity matrix is provided by direct assignment to the slot - all other updates go though this method
			#		Thus, HistoryProp starts with length 1, not 0. As a consequence, the length of histLst should always match the timestep-1
			if(length(histLst)!=timestep-1)stop("The timestep-1 value does not match the current lenght of the HistoryProp list.")
			
			## The propensity matrix has values for the diagonal, all else is 0's
			## The functions in the ModelsProp slot are, however, generated by a general function and apply to all 100 cells in the matrix
			mdls<-ModelsProp(object)
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
						res<-as.numeric(res.pred[1]); res<-res*scalePar
					}
					tmpPredict<-c(tmpPredict,res)
				}
			}
			newPropensity<-try(matrix(tmpPredict,nrow=nr,ncol=nc,byrow=TRUE),silent=TRUE)
			if(inherits(newPropensity,"try-error")){
				stop("Failed to generate the new propensity matrix - check input body condition and models")
			}else{
				nstp<-timestep
				histLst[[nstp]]<-newPropensity
				HistoryProp(object)<-histLst
				CurrentProp(object)<-newPropensity
			}
			return(object)
		}
)
########################## 
########################## Create trend table of Propensity
#' Set generic to a method that creates output tables from the history of propensity (HistoryProp slot) in the object
#' 
#' @name SummarizeProp
#' @param object A Propensity object
setGeneric("SummarizeProp",
		function(object,  ...) standardGeneric("SummarizeProp"))

#' A method that creates output tables from tthe history of propensity (HistoryProp slot) in the object
#' 
#' @param object A Propensity object
setMethod("SummarizeProp", signature(object = "Propensity"),
		function(object) {
			ea.call<-match.call()
			if (is.null(object)) stop("A Propensity object is required.")
			histLst<-HistoryProp(object)
			if(length(histLst)<5)stop("The HistoryProp list has very little or no data. Check to see that a simulation has been run.")
			## NOTE: the Propensity matrix has values of propensity for the first row only. Those are compiled here.
			by.cat<-data.frame()
			for(ii in 1:length(histLst)){
				tmp.mx<-histLst[[ii]];dd<-diag(tmp.mx)
				nr<-NROW(dd);tm<-rep(ii,times=nr);agecats<-paste("Age",1:(nr),sep="")
				dat<-data.frame(time=tm,age=agecats,propen=dd)
				by.cat<-rbind(by.cat,dat)
			}
			return(by.cat)
		}
)
########################## 




