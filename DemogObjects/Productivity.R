# TODO: Add comment
# 
# Author: lsalas
###############################################################################

## This file sets the Productivity object for the simple WESE simulation
#' Abstract class for Productivity
#' 
#' Abstract class for Productivity
#'
#' @slot CurrentProd  A 10x10 matrix object with the current breeding productivity matrix
#' @slot ModelsProd A list object, each entry contains a model relating each parameter of the productivity matrix to WESE body condition
#' @slot HistoryProd A list object that holds the history of productivity matrices by time step
#' @exportClass Productivity
setClass(Class="Productivity", representation(
				CurrentProd = "matrix",
				ModelsProd = "list",
				HistoryProd = "list"
		))

############################################ SLOT METHODS #######################################
########################## Set CurrentProd slot
#' Set generic to  method that sets the CurrentProd slot of the Productivity object.
#' 
#' @name setCurrentProd
#' @param object A Productivity object
#' @param value The current productivity matrix to put into the CurrentProd slot
#' @nord
setGeneric("CurrentProd<-", 
		function(object, value)	standardGeneric("CurrentProd<-"))

#' Set the CurrentProd slot of a Productivity object.
#' 
#' @name setCurrentProd
#' @param object A Productivity object
#' @param value The current productivity matrix to put into the CurrentProd slot

setReplaceMethod("CurrentProd",signature(object="Productivity"),
		function(object,value) {
			slot(object,"CurrentProd")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the CurrentProd slot value of a Productivity object.
#' 
#' @name CurrentProd
#' @param object A Productivity object
#' @nord
setGeneric("CurrentProd", 
		function(object) standardGeneric("CurrentProd"))

#' Retrieve the CurrentProd slot value of a Productivity object.
#' 
#' @name CurrentProd
#' @param object A Productivity object

setMethod("CurrentProd", signature(object="Productivity"),
		function(object) slot(object,"CurrentProd"))

##########################
########################## Set ModelsProd slot
#' Set generic to  method that sets the ModelsProd slot of the Productivity object.
#' 
#' @name setModelsProd
#' @param object A Productivity object
#' @param value A list object, each entry contains a model relating each parameter of the productivity matrix to WESE body condition, to put into the ModelsProd slot
#' @nord
setGeneric("ModelsProd<-", 
		function(object, value)	standardGeneric("ModelsProd<-"))

#' Set the ModelsProd slot of a Productivity object.
#' 
#' @name setModelsProd
#' @param object A Productivity object
#' @param value A list object, each entry contains a model relating each parameter of the productivity matrix to WESE body condition, to put into the ModelsProd slot

setReplaceMethod("ModelsProd",signature(object="Productivity"),
		function(object,value) {
			slot(object,"ModelsProd")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the ModelsProd slot value of a Productivity object.
#' 
#' @name ModelsProd
#' @param object A Productivity object
#' @nord
setGeneric("ModelsProd", 
		function(object) standardGeneric("ModelsProd"))

#' Retrieve the ModelsProd slot value of a Productivity object.
#' 
#' @name ModelsProd
#' @param object A Productivity object

setMethod("ModelsProd", signature(object="Productivity"),
		function(object) slot(object,"ModelsProd"))

##########################
########################## Set HistoryProd slot
#' Set generic to  method that sets the HistoryProd slot of the Productivity object.
#' 
#' @name setHistoryProd
#' @param object A Productivity object
#' @param value A list object that holds the history of productivity matrices by time step
#' @nord
setGeneric("HistoryProd<-", 
		function(object, value)	standardGeneric("HistoryProd<-"))

#' Set the HistoryProd slot of a Productivity object.
#' 
#' @name setHistoryProd
#' @param object A Productivity object
#' @param value A list object that holds the history of productivity matrices by time step

setReplaceMethod("HistoryProd",signature(object="Productivity"),
		function(object,value) {
			slot(object,"HistoryProd")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the HistoryProd slot value of a Productivity object.
#' 
#' @name HistoryProd
#' @param object A Productivity object
#' @nord
setGeneric("HistoryProd", 
		function(object) standardGeneric("HistoryProd"))

#' Retrieve the HistoryProd slot value of a Productivity object.
#' 
#' @name HistoryProd
#' @param object A Productivity object

setMethod("HistoryProd", signature(object="Productivity"),
		function(object) slot(object,"HistoryProd"))

##########################

############################################ INITIALIZE ####################################################
#' Instantiate a new Productivity object
#' 
#' @name initialize
#' @nord
#' @exportMethod initialize 
setMethod("initialize",
		signature(.Object = "Productivity"),
		function (.Object, ...) 
		{
			.Object@CurrentProd<-matrix()
			.Object@ModelsProd<-list()
			.Object@HistoryProd<-list()
			.Object
		}
)

############################################ PRODUCTIVITY METHODS #######################################
########################## Create current productivity, update history of productivity
#' Set generic to  method that creates current productivity matrix from a sampled value of WESE body condition
#' 
#' @name UpdateProd
#' @param object A Productivity object
setGeneric("UpdateProd",
		function(object,  ...) standardGeneric("UpdateProd"))

#' Create current productivity matrix from from a sampled value of WESE body condition
#' 
#' @param object A Productivity object
#' @param bodyCond Numeric. A sampled value of body condition
#' @param scalePar Numeric. The value of the scale parameter to adjust the value of the demographic parameter.
#' @param timestep Integer, a value for the current timestep
setMethod("UpdateProd", signature(object = "Productivity"),
		function(object, bodyCond,scalePar=1, timestep) {
			ea.call<-match.call()
			if (is.null(object)) stop("A Productivity object is required.")
			if (is.null(bodyCond)) stop("A value of WESE body condition is required.")
			if (is.null(timestep) | (class(timestep)!="integer")) stop("A valid timestep value is required.")
			histLst<-HistoryProd(object)
			#NOTE: 	The first productivity matrix is provided by direct assignment to the slot - all other updates go though this method
			#		Thus, HistoryProd starts with length 1, not 0. As a consequence, the length of histLst should always match the timestep-1
			if(length(histLst)!=timestep-1)stop("The timestep-1 value does not match the current lenght of the HistoryProd list.")
			
			## The Productivity matrix has values for the first row, all else is 0's
			## The functions in the ModelsProd slot are, however, generated by a general function and apply to all cells in the matrix
			mdls<-ModelsProd(object)
			nr<-length(mdls);nc<-length(mdls[[1]])
			if(nr!=nc) stop("The number of row and columns in the models object are not the same.")
			tmpPredict<-numeric()
			
			#Presently not affected by BC.
			
			for(rr in 1:nr){
				for(cc in 1:nc){
					mdl<-mdls[[rr]][[cc]]
					if(class(mdl)=="numeric"){
						res<-mdl
					}else{
						res.pred<-predict(mdl,newdata=data.frame(bodyCond=bodyCond),se.fit=FALSE)	#careful here to ensure we are getting a vector
						res<-as.numeric(res.pred[1]); res<-res*scalePar
					}
					tmpPredict<-c(tmpPredict,res)
				}
			}
			newProductivity<-try(matrix(tmpPredict,nrow=nr,ncol=nc,byrow=TRUE),silent=TRUE)
			if(inherits(newProductivity,"try-error")){
				stop("Failed to generate the new productivity matrix - check input body condition and models")
			}else{
				nstp<-timestep
				histLst[[nstp]]<-newProductivity
				HistoryProd(object)<-histLst
				CurrentProd(object)<-newProductivity
			}
			return(object)
		}
)
########################## 
########################## Create trend table of Productivity
#' Set generic to a method that creates output tables from the history of Productivity (HistoryProd slot) in the object
#' 
#' @name SummarizeProd
#' @param object A Productivity object
setGeneric("SummarizeProd",
		function(object,  ...) standardGeneric("SummarizeProd"))

#' A method that creates output tables from tthe history of Productivity (HistoryProd slot) in the object
#' 
#' @param object A Productivity object
setMethod("SummarizeProd", signature(object = "Productivity"),
		function(object) {
			ea.call<-match.call()
			if (is.null(object)) stop("A Productivity object is required.")
			histLst<-HistoryProd(object)
			if(length(histLst)<5)stop("The HistoryProd list has very little or no data. Check to see that a simulation has been run.")
			## NOTE: the Productivity matrix has values of productivity along the first row only. Those are compiled here.
			by.cat<-data.frame()
			for(ii in 1:length(histLst)){
				tmp.mx<-histLst[[ii]];dd<-tmp.mx[1,]
				nr<-NROW(dd);tm<-rep(ii,times=nr);agecats<-paste("Age",1:(nr),sep="")
				dat<-data.frame(time=tm,age=agecats,prodct=dd)
				by.cat<-rbind(by.cat,dat)
			}
			return(by.cat)
		}
)
########################## 


