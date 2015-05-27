# TODO: Add comment
# 
# Author: lsalas
###############################################################################


## This file sets the FishTrend object for the simple WESE simulation
#' Abstract class for FishTrend
#' 
#' Abstract class for FishTrend
#'
#' @slot TFabundance The model with the trend in Toothfish abundance over time, with metadata about start abundance and depletion rate
#' @slot CurrentWeight The current weight of the average seal, estimated from the toothfish-availability-to-weight model and toothfish availability
#' @slot CurrentTF  The numeric value of current number of Toothfish consumed
#' @slot CurrentSF The numeric value of current number of Silverfish consumed
#' @slot Timestep The integer value of the current timestep
#' @slot Toothfish  A list holding the trend model of toothfish consumed vs. toothfish abundance and the data used to train it, if any
#' @slot Silverfish A list holding the trend model of silverfish consumed vs. toothfish abundance and the data used to train it, if any
#' @slot TFtoWeight A model relating the seals' weight vs. toothfish abundance
#' @exportClass FishTrend
setClass(Class="FishTrend", representation(
				TFabundance = "list",
				CurrentWeight = "numeric",
				CurrentTF = "numeric",
				CurrentSF = "numeric",
				Timestep = "integer",
				Toothfish = "list",
				Silverfish = "list",
				TFtoWeight = "list"
		))

############################################ SLOT METHODS #######################################
########################## Set TFabundance slot
#' Set generic to  method that sets the TFabundance slot of the FishTrend object.
#' 
#' @name setTFabundance
#' @param object A FishTrend object
#' @param value The model with the trend in Toothfish abundance over time, with metadata about start abundance and depletion rate
#' @nord
setGeneric("TFabundance<-", 
		function(object, value)	standardGeneric("TFabundance<-"))

#' Set the TFabundance slot of a FishTrend object.
#' 
#' @name setTFabundance
#' @param object A FishTrend object
#' @param value The model with the trend in Toothfish abundance over time, with metadata about start abundance and depletion rate
setReplaceMethod("TFabundance",signature(object="FishTrend"),
		function(object,value) {
			slot(object,"TFabundance")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the TFabundance slot value of a FishTrend object.
#' 
#' @name TFabundance
#' @param object A FishTrend object
#' @nord
setGeneric("TFabundance", 
		function(object) standardGeneric("TFabundance"))

#' Retrieve the TFabundance slot value of a FishTrend object.
#' 
#' @name TFabundance
#' @param object A FishTrend object
setMethod("TFabundance", signature(object="FishTrend"),
		function(object) slot(object,"TFabundance"))

##########################
########################## Set CurrentWeight slot
#' Set generic to  method that sets the CurrentWeight slot of the FishTrend object.
#' 
#' @name setCurrentWeight
#' @param object A FishTrend object
#' @param value The current weight of the average seal, estimated from the toothfish-availability-to-weight model and toothfish availability
#' @nord
setGeneric("CurrentWeight<-", 
		function(object, value)	standardGeneric("CurrentWeight<-"))

#' Set the CurrentWeight slot of a FishTrend object.
#' 
#' @name setCurrentWeight
#' @param object A FishTrend object
#' @param value The current weight of the average seal, estimated from the toothfish-availability-to-weight model and toothfish availability
setReplaceMethod("CurrentWeight",signature(object="FishTrend"),
		function(object,value) {
			slot(object,"CurrentWeight")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the CurrentWeight slot value of a FishTrend object.
#' 
#' @name CurrentWeight
#' @param object A FishTrend object
#' @nord
setGeneric("CurrentWeight", 
		function(object) standardGeneric("CurrentWeight"))

#' Retrieve the CurrentWeight slot value of a FishTrend object.
#' 
#' @name CurrentWeight
#' @param object A FishTrend object
setMethod("CurrentWeight", signature(object="FishTrend"),
		function(object) slot(object,"CurrentWeight"))

##########################
########################## Set CurrentTF slot
#' Set generic to  method that sets the CurrentTF slot of the FishTrend object.
#' 
#' @name setCurrentTF
#' @param object A FishTrend object
#' @param value The numeric value of current Toothfish abundance
#' @nord
setGeneric("CurrentTF<-", 
		function(object, value)	standardGeneric("CurrentTF<-"))

#' Set the CurrentTF slot of a FishTrend object.
#' 
#' @name setCurrentTF
#' @param object A FishTrend object
#' @param value The numeric value of current Toothfish abundance

setReplaceMethod("CurrentTF",signature(object="FishTrend"),
		function(object,value) {
			slot(object,"CurrentTF")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the CurrentTF slot value of a FishTrend object.
#' 
#' @name CurrentTF
#' @param object A FishTrend object
#' @nord
setGeneric("CurrentTF", 
		function(object) standardGeneric("CurrentTF"))

#' Retrieve the CurrentTF slot value of a FishTrend object.
#' 
#' @name CurrentTF
#' @param object A FishTrend object

setMethod("CurrentTF", signature(object="FishTrend"),
		function(object) slot(object,"CurrentTF"))

##########################
########################## Set CurrentSF slot
#' Set generic to  method that sets the CurrentSF slot of the FishTrend object.
#' 
#' @name setCurrentSF
#' @param object A FishTrend object
#' @param value The numeric value of current Silverfish abundance
#' @nord
setGeneric("CurrentSF<-", 
		function(object, value)	standardGeneric("CurrentSF<-"))

#' Set the CurrentSF slot of a FishTrend object.
#' 
#' @name setCurrentSF
#' @param object A FishTrend object
#' @param value The numeric value of current Silverfish abundance

setReplaceMethod("CurrentSF",signature(object="FishTrend"),
		function(object,value) {
			slot(object,"CurrentSF")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the CurrentSF slot value of a FishTrend object.
#' 
#' @name CurrentSF
#' @param object A FishTrend object
#' @nord
setGeneric("CurrentSF", 
		function(object) standardGeneric("CurrentSF"))

#' Retrieve the CurrentSF slot value of a FishTrend object.
#' 
#' @name CurrentSF
#' @param object A FishTrend object

setMethod("CurrentSF", signature(object="FishTrend"),
		function(object) slot(object,"CurrentSF"))

##########################
########################## Set Timestep slot
#' Set generic to  method that sets the Timestep slot of the FishTrend object.
#' 
#' @name setTimestep
#' @param object A FishTrend object
#' @param value The integer value of the current timestep
#' @nord
setGeneric("Timestep<-", 
		function(object, value)	standardGeneric("Timestep<-"))

#' Set the Timestep slot of a FishTrend object.
#' 
#' @name setTimestep
#' @param object A FishTrend object
#' @param value The integer value of the current timestep

setReplaceMethod("Timestep",signature(object="FishTrend"),
		function(object,value) {
			slot(object,"Timestep")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the Timestep slot value of a FishTrend object.
#' 
#' @name Timestep
#' @param object A FishTrend object
#' @nord
setGeneric("Timestep", 
		function(object) standardGeneric("Timestep"))

#' Retrieve the CurrentSF slot value of a FishTrend object.
#' 
#' @name Timestep
#' @param object A FishTrend object

setMethod("Timestep", signature(object="FishTrend"),
		function(object) slot(object,"Timestep"))

##########################
########################## Set Toothfish slot
#' Set generic to  method that sets the Toothfish slot of the FishTrend object.
#' 
#' @name setToothfish
#' @param object A FishTrend object
#' @param value A list holding the trend model of toothfish consumed vs. toothfish abundance and the data used to train it, if any
#' @nord
setGeneric("Toothfish<-", 
		function(object, value)	standardGeneric("Toothfish<-"))

#' Set the Toothfish slot of a FishTrend object.
#' 
#' @name setToothfish
#' @param object A FishTrend object
#' @param value A list holding the trend model of toothfish consumed vs. toothfish abundance and the data used to train it, if any

setReplaceMethod("Toothfish",signature(object="FishTrend"),
		function(object,value) {
			slot(object,"Toothfish")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the Toothfish slot value of a FishTrend object.
#' 
#' @name Toothfish
#' @param object A FishTrend object
#' @nord
setGeneric("Toothfish", 
		function(object) standardGeneric("Toothfish"))

#' Retrieve the Toothfish slot value of a FishTrend object.
#' 
#' @name Toothfish
#' @param object A FishTrend object

setMethod("Toothfish", signature(object="FishTrend"),
		function(object) slot(object,"Toothfish"))

##########################
########################## Set Silverfish slot
#' Set generic to  method that sets the Silverfish slot of the FishTrend object.
#' 
#' @name setSilverfish
#' @param object A FishTrend object
#' @param value A list holding the trend model of silverfish consumed vs. toothfish abundance and the data used to train it, if any
#' @nord
setGeneric("Silverfish<-", 
		function(object, value)	standardGeneric("Silverfish<-"))

#' Set the Silverfish slot of a FishTrend object.
#' 
#' @name setSilverfish
#' @param object A FishTrend object
#' @param value A list holding the trend model of silverfish consumed vs. toothfish abundance and the data used to train it, if any

setReplaceMethod("Silverfish",signature(object="FishTrend"),
		function(object,value) {
			slot(object,"Silverfish")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the Silverfish slot value of a FishTrend object.
#' 
#' @name Silverfish
#' @param object A FishTrend object
#' @nord
setGeneric("Silverfish", 
		function(object) standardGeneric("Silverfish"))

#' Retrieve the Silverfish slot value of a FishTrend object.
#' 
#' @name Silverfish
#' @param object A FishTrend object

setMethod("Silverfish", signature(object="FishTrend"),
		function(object) slot(object,"Silverfish"))

##########################
########################## Set TFtoWeight slot
#' Set generic to  method that sets the Toothfish slot of the FishTrend object.
#' 
#' @name setTFtoWeight
#' @param object A FishTrend object
#' @param value A model relating the seals' weight vs. toothfish abundance
#' @nord
setGeneric("TFtoWeight<-", 
		function(object, value)	standardGeneric("TFtoWeight<-"))

#' Set the TFtoWeight slot of a FishTrend object.
#' 
#' @name setTFtoWeight
#' @param object A FishTrend object
#' @param value A model relating the seals' weight vs. toothfish abundance
setReplaceMethod("TFtoWeight",signature(object="FishTrend"),
		function(object,value) {
			slot(object,"TFtoWeight")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the TFtoWeight slot value of a FishTrend object.
#' 
#' @name TFtoWeight
#' @param object A FishTrend object
#' @nord
setGeneric("TFtoWeight", 
		function(object) standardGeneric("TFtoWeight"))

#' Retrieve the TFtoWeight slot value of a FishTrend object.
#' 
#' @name TFtoWeight
#' @param object A FishTrend object
setMethod("TFtoWeight", signature(object="FishTrend"),
		function(object) slot(object,"TFtoWeight"))

##########################
############################################ INITIALIZE ####################################################
#' Instantiate a new FishTrend object
#' 
#' @name initialize
#' @nord
#' @exportMethod initialize 
setMethod("initialize",
		signature(.Object = "FishTrend"),
		function (.Object, ...) 
		{
			.Object@TFabundance<-list()
			.Object@CurrentWeight<-numeric()
			.Object@CurrentTF<-numeric()
			.Object@CurrentSF<-numeric()
			.Object@Timestep<-integer()
			.Object@Toothfish<-list()
			.Object@Silverfish<-list()
			.Object@TFtoWeight<-list()
			.Object
		}
)


############################################ FISHTREND METHODS #######################################
########################## Generate trend values for CurrentTF and CurrentSf from trend data and timestep
#' Set generic to  method that generates current values of seal weight, and toothfish and silverfish abundance from toothfish trend model and timestep
#' 
#' @name UpdateFish
#' @param object A FishTrend object
setGeneric("UpdateFish",
		function(object,  ...) standardGeneric("UpdateFish"))

#' Generates current values of toothfish and silverfish abundance from trend models and timestep
#' 
#' @param object A FishTrend object
#' @param timestep Integer, a value for the current timestep
setMethod("UpdateFish", signature(object = "FishTrend"),
		function(object,timestep) {
			ea.call<-match.call()
			if (is.null(object)) stop("A FishTrend object is required.")
			if (is.null(timestep) | (class(timestep)!="integer")) stop("A valid timestep value is required.")
			oldStep<-Timestep(object)
			#NOTE: 	The whole modeling is initialized with year 1 data, so oldStep is length=1 at the beginning and has the first step value (=0)
			if(oldStep!=(timestep-1))stop("The new timestep value is not 1 + old value.")
			
			#need to get the tfabundance from timestep
			tfabund.mdl<-TFabundance(object)[[1]]
			newdata<-data.frame(year=timestep)	#assuming that the x-variable (predictor) is simply called "time" and there is no other predictor
			tfa<-predict(tfabund.mdl,newdata=newdata)
			
			sealwgt<-TFtoWeight(object)[[1]]
			tfmodel<-Toothfish(object)[[1]]
			sfmodel<-Silverfish(object)[[1]]
			tfab<-data.frame(TFabund=tfa)
			
			newWt<-try(predict(sealwgt,newdata=tfab),silent=TRUE)
			newTF<-try(predict(tfmodel,newdata=tfab),silent=TRUE)
			newSF<-try(predict(sfmodel,newdata=tfab),silent=TRUE)
			if(!inherits(newWt,"try-error") & !inherits(newTF,"try-error") & !inherits(newSF,"try-error")){
				CurrentWeight(object)<-newWt
				CurrentTF(object)<-newTF
				CurrentSF(object)<-newSF
				Timestep(object)<-timestep
				return(object)
			}else{
				stop("Failed to obtain new seal weight, toothfish or silverfish consumed")
			}
		}
)
########################## 
