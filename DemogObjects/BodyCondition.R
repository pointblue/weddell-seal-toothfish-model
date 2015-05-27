# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## This file sets the BodyCondition object for the simple WESE simulation
#' Abstract class for BodyCondition
#' 
#' Abstract class for BodyCondition
#'
#' @slot CurrentBC  Numeric - the current bodyCondition value
#' @slot HistoryBC Numeric vector that holds the history of sampled breeding conditions by time step
#' @exportClass BodyCondition
setClass(Class="BodyCondition", representation(
				CurrentBC = "numeric",
				HistoryBC = "numeric",
				ScaleModel = "list",
				CurrentSc = "numeric",
				HistorySc = "numeric"
		))

############################################ SLOT METHODS #######################################
########################## Set CurrentBC slot
#' Set generic to  method that sets the CurrentBC slot of an object of the Weddell Seal demographics project.
#' 
#' @name setCurrentBC
#' @param object An object of the Weddell Seal demographics project
#' @param value The current value to place in the CurrentBC slot of the object
#' @nord
setGeneric("CurrentBC<-", 
		function(object, value)	standardGeneric("CurrentBC<-"))

#' Set the CurrentBC slot of a BodyCondition object.
#' 
#' @name setCurrentBC
#' @param object A BodyCondition object
#' @param value The current bodyCondition value
setReplaceMethod("CurrentBC",signature(object="BodyCondition"),
		function(object,value) {
			slot(object,"CurrentBC")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the CurrentBC slot value of an object of the Weddell Seal demographics project.
#' 
#' @name CurrentBC
#' @param object An object of the Weddell Seal demographics project
#' @nord
setGeneric("CurrentBC", 
		function(object) standardGeneric("CurrentBC"))

#' Retrieve the CurrentBC slot value of a BodyCondition object.
#' 
#' @name CurrentBC
#' @param object A BodyCondition object
setMethod("CurrentBC", signature(object="BodyCondition"),
		function(object) slot(object,"CurrentBC"))

##########################
########################## Set HistoryBC slot
#' Set generic to  method that sets the HistoryBC slot of an object of the Weddell Seal demographics project.
#' 
#' @name setHistoryBC
#' @param object An object of the Weddell Seal demographics project
#' @param value A list object
#' @nord
setGeneric("HistoryBC<-", 
		function(object, value)	standardGeneric("HistoryBC<-"))

#' Set the HistoryBC slot of a BodyCondition object.
#' 
#' @name setHistoryBC
#' @param object A BodyCondition object
#' @param value A vector that holds the history of sampled breeding conditions by time step
setReplaceMethod("HistoryBC",signature(object="BodyCondition"),
		function(object,value) {
			slot(object,"HistoryBC")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the HistoryBC slot value of an object of the Weddell Seal demographics project.
#' 
#' @name HistoryBC
#' @param object An object of the Weddell Seal demographics project
#' @nord
setGeneric("HistoryBC", 
		function(object) standardGeneric("HistoryBC"))

#' Retrieve the HistoryBC slot value of a BodyCondition object.
#' 
#' @name HistoryBC
#' @param object A BodyCondition object
setMethod("HistoryBC", signature(object="BodyCondition"),
		function(object) slot(object,"HistoryBC"))

##########################
########################## Set ScaleModel slot
#' Set generic to  method that sets the ScaleModel slot of an object of the Weddell Seal demographics project.
#' 
#' @name setScaleModel
#' @param object An object of the Weddell Seal demographics project
#' @param value The current value to place in the ScaleModel slot of the object
#' @nord
setGeneric("ScaleModel<-", 
		function(object, value)	standardGeneric("ScaleModel<-"))

#' Set the ScaleModel slot of a BodyCondition object.
#' 
#' @name setScaleModel
#' @param object A BodyCondition object
#' @param value The model to scale the current BC value for adjusting demographic parameters
setReplaceMethod("ScaleModel",signature(object="BodyCondition"),
		function(object,value) {
			slot(object,"ScaleModel")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the ScaleModel slot value of an object of the Weddell Seal demographics project.
#' 
#' @name ScaleModel
#' @param object An object of the Weddell Seal demographics project
#' @nord
setGeneric("ScaleModel", 
		function(object) standardGeneric("ScaleModel"))

#' Retrieve the ScaleModel slot value of a BodyCondition object.
#' 
#' @name ScaleModel
#' @param object A BodyCondition object
setMethod("ScaleModel", signature(object="BodyCondition"),
		function(object) slot(object,"ScaleModel"))

##########################
########################## Set CurrentSc slot
#' Set generic to  method that sets the CurrentBC slot of an object of the Weddell Seal demographics project.
#' 
#' @name setCurrentSc
#' @param object An object of the Weddell Seal demographics project
#' @param value The current value to place in the CurrentSc slot of the object
#' @nord
setGeneric("CurrentSc<-", 
		function(object, value)	standardGeneric("CurrentSc<-"))

#' Set the CurrentBC slot of a BodyCondition object.
#' 
#' @name setCurrentSc
#' @param object A BodyCondition object
#' @param value The current scaled bodyCondition value
setReplaceMethod("CurrentSc",signature(object="BodyCondition"),
		function(object,value) {
			slot(object,"CurrentSc")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the CurrentSc slot value of an object of the Weddell Seal demographics project.
#' 
#' @name CurrentSc
#' @param object An object of the Weddell Seal demographics project
#' @nord
setGeneric("CurrentSc", 
		function(object) standardGeneric("CurrentSc"))

#' Retrieve the CurrentSc slot value of a BodyCondition object.
#' 
#' @name CurrentSc
#' @param object A BodyCondition object
setMethod("CurrentSc", signature(object="BodyCondition"),
		function(object) slot(object,"CurrentSc"))

##########################
########################## Set HistorySc slot
#' Set generic to  method that sets the HistorySc slot of an object of the Weddell Seal demographics project.
#' 
#' @name setHistorySc
#' @param object An object of the Weddell Seal demographics project
#' @param value A list object
#' @nord
setGeneric("HistorySc<-", 
		function(object, value)	standardGeneric("HistorySc<-"))

#' Set the HistorySc slot of a BodyCondition object.
#' 
#' @name setHistorySc
#' @param object A BodyCondition object
#' @param value A vector that holds the history of sampled breeding conditions by time step, scaled as multipliers that adjust demographic parameters
setReplaceMethod("HistorySc",signature(object="BodyCondition"),
		function(object,value) {
			slot(object,"HistorySc")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the HistorySc slot value of an object of the Weddell Seal demographics project.
#' 
#' @name HistorySc
#' @param object An object of the Weddell Seal demographics project
#' @nord
setGeneric("HistorySc", 
		function(object) standardGeneric("HistorySc"))

#' Retrieve the HistorySc slot value of a BodyCondition object.
#' 
#' @name HistorySc
#' @param object A BodyCondition object
setMethod("HistorySc", signature(object="BodyCondition"),
		function(object) slot(object,"HistorySc"))

##########################

############################################ INITIALIZE ####################################################
#' Instantiate a new BodyCondition object
#' 
#' @name initialize
#' @nord
#' @exportMethod initialize 
setMethod("initialize",
		signature(.Object = "BodyCondition"),
		function (.Object, ...) 
		{
			.Object@CurrentBC<-numeric()
			.Object@HistoryBC<-numeric()
			.Object@ScaleModel<-list()
			.Object@CurrentSc<-numeric()
			.Object@HistorySc<-numeric()
			.Object
		}
)

############################################ BODYCONDITION METHODS #######################################
########################## Sample new bodyCondition, update history of bodyCondition
#' Set generic to  method that creates current bodyCondition value by sampling a value of WESE body condition
#' 
#' @name UpdateBC
#' @param object A BodyCondition object
setGeneric("UpdateBC",
		function(object,  ...) standardGeneric("UpdateBC"))

#' Create current bodyCondition value by sampling a value of WESE body condition
#' 
#' @param object A BodyCondition object
#' @param bodyCondMean Numeric. The current mean value of the distribution of body conditions
#' @param bodyCondSD Numeric. The current standard deviation of the distribution of body conditions
#' @param timestep Integer, a value for the current timestep
setMethod("UpdateBC", signature(object = "BodyCondition"),
		function(object, bodyCondMean,bodyCondSD,timestep) {
			ea.call<-match.call()
			if (is.null(object)) stop("A BodyCondition object is required.")
			if (is.null(bodyCondMean) | (bodyCondMean<=0)) stop("A value of WESE mean body condition >0 is required.")
			if (is.null(bodyCondSD)) stop("A value of WESE SD body condition is required.")
			if (is.null(timestep) | (class(timestep)!="integer")) stop("A valid timestep value is required.")
			histLstBC<-HistoryBC(object)
			histLstSc<-HistorySc(object)
			#NOTE: 	The first body condition value is provided by direct assignment to the slot - all other updates go though this method
			#		Thus, HistoryBC starts with length 1, not 0. As a consequence, the length of histLst should always match the timestep-1
			if(length(histLstBC)!=timestep-1)stop("The timestep-1 value does not match the current lenght of the HistoryBC list.")
			if(length(histLstSc)!=timestep-1)stop("The timestep-1 value does not match the current lenght of the HistorySc list.")
			
			newBodyCond<-rnorm(1,mean=bodyCondMean,sd=bodyCondSD)
			newBodyCond<-ifelse(newBodyCond<0,0,newBodyCond)
			scalemod<-ScaleModel(object)[[1]]
			newScalepar<-predict(scalemod,newdata=data.frame(bodyCond=newBodyCond))
			newScalepar<-ifelse(newScalepar<=0,0.01,newScalepar)
			
			histLstBC<-c(histLstBC,newBodyCond)
			HistoryBC(object)<-histLstBC
			CurrentBC(object)<-newBodyCond
			histLstSc<-c(histLstSc,newScalepar)
			HistorySc(object)<-histLstSc
			CurrentSc(object)<-newScalepar
			
			return(object)
		}
)
########################## 
 

