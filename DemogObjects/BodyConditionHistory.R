# TODO: Add comment
# 
# Author: lsalas
###############################################################################

## This file sets the BodyConditionHistory object for the simple WESE simulation
#' Abstract class for BodyConditionHistory
#' 
#' Abstract class for BodyConditionHistory
#'
#' @slot CurrentBCH  Numeric, a vector with the mean and sd (in that order) of WESE body condition
#' @slot HistoryBCH A list object that holds the history of mean and sd values by timestep (i.e., the body condition distribution's trend data)
#' @slot WeightToBC A list object containing the model that describes the link between weight and body condition.  
#' @exportClass BodyConditionHistory
setClass(Class="BodyConditionHistory", representation(
				CurrentBCH = "numeric",
				HistoryBCH = "list",
				WeightToBC = "list"
		))

############################################ SLOT METHODS #######################################
########################## Set CurrentBCH slot
#' Set generic to  method that sets the CurrentBCH slot of a BodyConditionHistory object.
#' 
#' @name setCurrentBCH
#' @param object A BodyConditionHistory object
#' @param value The current value to place in the CurrentBCH slot of the object
#' @nord
setGeneric("CurrentBCH<-", 
		function(object, value)	standardGeneric("CurrentBCH<-"))

#' Set the CurrentBCH slot of a BodyConditionHistory object.
#' 
#' @name setCurrentBCH
#' @param object A BodyConditionHistory object
#' @param value The current mean and sd (in that order) of WESE body condition
setReplaceMethod("CurrentBCH",signature(object="BodyConditionHistory"),
		function(object,value) {
			slot(object,"CurrentBCH")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the CurrentBCH slot of a BodyConditionHistory object.
#' 
#' @name CurrentBCH
#' @param object A BodyConditionHistory object
#' @nord
setGeneric("CurrentBCH", 
		function(object) standardGeneric("CurrentBCH"))

#' Retrieve the CurrentBCH slot value of a BodyConditionHistory object.
#' 
#' @name CurrentBCH
#' @param object A BodyConditionHistory object
setMethod("CurrentBCH", signature(object="BodyConditionHistory"),
		function(object) slot(object,"CurrentBCH"))

##########################
########################## Set HistoryBCH slot
#' Set generic to  method that sets the HistoryBCH slot of a BodyConditionHistory object.
#' 
#' @name setHistoryBCH
#' @param object A BodyConditionHistory object
#' @param value A list object
#' @nord
setGeneric("HistoryBCH<-", 
		function(object, value)	standardGeneric("HistoryBCH<-"))

#' Set the HistoryBCH slot of a BodyConditionHistory object.
#' 
#' @name setHistoryBCH
#' @param object A BodyConditionHistory object
#' @param value A list object that holds the history of mean and sd values by timestep (i.e., the body condition distribution's trend data)
setReplaceMethod("HistoryBCH",signature(object="BodyConditionHistory"),
		function(object,value) {
			slot(object,"HistoryBCH")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the HistoryBCH slot value of a BodyConditionHistory object.
#' 
#' @name HistoryBCH
#' @param object A BodyConditionHistory object
#' @nord
setGeneric("HistoryBCH", 
		function(object) standardGeneric("HistoryBCH"))

#' Retrieve the HistoryBCH slot value of a BodyConditionHistory object.
#' 
#' @name HistoryBCH
#' @param object A BodyConditionHistory object
setMethod("HistoryBCH", signature(object="BodyConditionHistory"),
		function(object) slot(object,"HistoryBCH"))

##########################
########################## Set WeightToBC slot
#' Set generic to  method that sets the WeightToBC slot of a BodyConditionHistory object.
#' 
#' @name setWeightToBC
#' @param object A BodyConditionHistory object
#' @param value A list object
#' @nord
setGeneric("WeightToBC<-", 
		function(object, value)	standardGeneric("WeightToBC<-"))

#' Set the WeightToBC slot of a BodyConditionHistory object.
#' 
#' @name setWeightToBC
#' @param object A BodyConditionHistory object
#' @param value A list object that holds the model that relates weight to body condition
setReplaceMethod("WeightToBC",signature(object="BodyConditionHistory"),
		function(object,value) {
			slot(object,"WeightToBC")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the WeightToBC slot value of a BodyConditionHistory object.
#' 
#' @name WeightToBC
#' @param object A BodyConditionHistory object
#' @nord
setGeneric("WeightToBC", 
		function(object) standardGeneric("WeightToBC"))

#' Retrieve the WeightToBC slot value of a BodyConditionHistory object.
#' 
#' @name WeightToBC
#' @param object A BodyConditionHistory object
setMethod("WeightToBC", signature(object="BodyConditionHistory"),
		function(object) slot(object,"WeightToBC"))

##########################
############################################ INITIALIZE ####################################################
#' Instantiate a new BodyConditionHistory object
#' 
#' @name initialize
#' @nord
#' @exportMethod initialize 
setMethod("initialize",
		signature(.Object = "BodyConditionHistory"),
		function (.Object, ...) 
		{
			.Object@CurrentBCH<-numeric()
			.Object@HistoryBCH<-list()
			.Object@WeightToBC<-list()
			.Object
		}
)

############################################ BODYCONDITIONHISTORY METHODS #######################################
########################## Fit new bodyCondition, update history of distBodyCondition
#' Set generic to  method that creates current the current value of a BodyConditionHistory object. The CurrentBCH slot is updated by requesting the mean and SD of weight gain from the 
#' WeightTrend object, then converting these to body condition values using the function in the WeightToBC slot.
#' 
#' @name UpdateBCH
#' @param object A BodyConditionHistory object
setGeneric("UpdateBCH",
		function(object,  ...) standardGeneric("UpdateBCH"))

#' Create current bodyCondition value by sampling a value of WESE body condition
#' 
#' @param object A BodyConditionHistory object
#' @param wtMean Numeric. The current mean seal weight
#' @param wtStdev Numeric. The current standard deviation of seal weight - should match the value in Garrott et al. 2013
#' @param timestep Integer, a value for the current timestep
setMethod("UpdateBCH", signature(object = "BodyConditionHistory"),
		function(object, wtMean,wtStdev,timestep) {
			ea.call<-match.call()
			if (is.null(object)) stop("A BodyConditionHistory object is required.")
			if (is.null(wtMean) ) stop("A value of mean seal weight is required.")
			if (is.null(wtStdev)) stop("A value of standard deviation of seal weight is required.")
			if (is.null(timestep) | (class(timestep)!="integer")) stop("A valid timestep value is required.")
			histLst<-HistoryBCH(object)
			#NOTE: 	The first body condition value is provided by direct assignment to the slot - all other updates go though this method
			#		Thus, HistoryBCH starts with length 1, not 0. As a consequence, the length of histLst should always match the timestep-1
			if(length(histLst)!=timestep-1)stop("The timestep value does not match the current lenght of the HistoryBCH list.")
			
			wgtbcmodel<-WeightToBC(object)[[1]]
			#need to get a value from this function for the mean and mean + sd
			wtMnSd<-wtMean+wtStdev; newdata=data.frame(weightGain=c(wtMean,wtMnSd))
			pred<-try(predict(wgtbcmodel,newdata=newdata))
			if(!inherits(pred,"try-error")){
				bcmean<-ifelse(pred[1]>0,pred[1],0)
				bcsd<-pred[2]-pred[1];if(bcsd<0){bcsd<-bcsd*-1}
			
				newBodyCond<-c(bcmean,bcsd)
				nstp<-timestep
				histLst[[nstp]]<-newBodyCond
				HistoryBCH(object)<-histLst
				CurrentBCH(object)<-newBodyCond
				return(object)
			}else{
				stop("Failed to generate parameters for the distribution of body condition")
			}
		}
)
########################## 
########################## Create trend table of Body Condition data
#' Set generic to a method that creates output tables from the HistoryBCH of a BodyConditionHistory object.
#' 
#' @name SummarizeBCH
#' @param object A BodyConditionHistory object
setGeneric("SummarizeBCH",
		function(object,  ...) standardGeneric("SummarizeBCH"))

#' A method that creates output tables from from the HistoryBCH of Body Condition (HistoryBCH slot) in the object
#' 
#' @param object A BodyConditionHistory object
setMethod("SummarizeBCH", signature(object = "BodyConditionHistory"),
		function(object) {
			ea.call<-match.call()
			if (is.null(object)) stop("A BodyConditionHistory object is required.")
			histLst<-HistoryBCH(object)
			if(length(histLst)<5)stop("The HistoryBCH list has very little or no data. Check to see that a simulation has been run.")
			body.cond<-data.frame()
			for(ii in 1:length(histLst)){
				tmp.mx<-histLst[[ii]]
				dat<-data.frame(time=ii,bc.mean=tmp.mx[1],bc.se=tmp.mx[2])
				body.cond<-rbind(body.cond,dat)
			}
			return(body.cond)
		}
)
########################## 

