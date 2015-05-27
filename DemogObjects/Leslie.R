# TODO: Add comment
# 
# Author: lsalas
###############################################################################

############################################# Dependencies ######################################

## This file sets the Leslie object for the simple WESE simulation
#' Abstract class for Leslie
#' 
#' Abstract class for Leslie
#' 
#' @slot CurrentLeslie A 10x10 matrix object with the current Leslie matrix
#' @slot LambdaVals A numeric vector keeping the history of Lambda values
#' @slot Abunds A numeric vector with current stage abundances, of length = one dimension of the Leslie
#' @slot Outs A data frame object, each row is a value of Abunds at a time step
#' @slot Size An integer vector with the size of the matrix (nrow,ncol)
#' @exportClass Leslie
setClass(Class="Leslie", representation(
				CurrentLeslie = "matrix",
				LambdaVals = "numeric",
				Abunds = "numeric",
				Outs = "data.frame",
				Size = "integer"
		))

############################################ SLOT METHODS #######################################
########################## Set CurrentLeslie slot
#' Set generic to  method that sets the CurrentLeslie slot of the Leslie object.
#' 
#' @name setCurrentLeslie
#' @param object A Leslie object
#' @param value The current Leslie matrix to put into the CurrentLeslie slot
#' @nord
setGeneric("CurrentLeslie<-", 
		function(object, value)	standardGeneric("CurrentLeslie<-"))

#' Set the CurrentLeslie slot of a Leslie object.
#' 
#' @name setCurrentLeslie
#' @param object A Leslie object
#' @param value The current Leslie matrix to put into the CurrentLeslie slot

setReplaceMethod("CurrentLeslie",signature(object="Leslie"),
		function(object,value) {
			slot(object,"CurrentLeslie")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the CurrentLeslie slot value of a Leslie object.
#' 
#' @name CurrentLeslie
#' @param object A Leslie object
#' @nord
setGeneric("CurrentLeslie", 
		function(object) standardGeneric("CurrentLeslie"))

#' Retrieve the CurrentLeslie slot value of a Leslie object.
#' 
#' @name CurrentLeslie
#' @param object A Leslie object

setMethod("CurrentLeslie", signature(object="Leslie"),
		function(object) slot(object,"CurrentLeslie"))

##########################
########################## Set LambdaVals slot
#' Set generic to  method that sets the LambdaVals slot of the Leslie object.
#' 
#' @name setLambdaVals
#' @param object A Leslie object
#' @param value The vector of Lambda values to put into the LambdaVals slot
#' @nord
setGeneric("LambdaVals<-", 
		function(object, value)	standardGeneric("LambdaVals<-"))

#' Set the LambdaVals slot of a Leslie object.
#' 
#' @name setLambdaVals
#' @param object A Leslie object
#' @param value The vector of Lambda values to put into the LambdaVals slot

setReplaceMethod("LambdaVals",signature(object="Leslie"),
		function(object,value) {
			slot(object,"LambdaVals")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the LambdaVals slot value of a Leslie object.
#' 
#' @name LambdaVals
#' @param object A Leslie object
#' @nord
setGeneric("LambdaVals", 
		function(object) standardGeneric("LambdaVals"))

#' Retrieve the LambdaVals slot value of a Leslie object.
#' 
#' @name LambdaVals
#' @param object A Leslie object

setMethod("LambdaVals", signature(object="Leslie"),
		function(object) slot(object,"LambdaVals"))

##########################
########################## Set Abunds slot
#' Set generic to  method that sets the Abunds slot of the Leslie object.
#' 
#' @name setAbunds
#' @param object A Leslie object
#' @param value A 1x10 matrix with current stage abundances to put into the Abunds slot
#' @nord
setGeneric("Abunds<-", 
		function(object, value)	standardGeneric("Abunds<-"))

#' Set the Abunds slot of a Leslie object.
#' 
#' @name setAbunds
#' @param object A Leslie object
#' @param value A 1x10 matrix with current stage abundances to put into the Abunds slot

setReplaceMethod("Abunds",signature(object="Leslie"),
		function(object,value) {
			slot(object,"Abunds")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the Abunds slot value of a Leslie object.
#' 
#' @name Abunds
#' @param object A Leslie object
#' @nord
setGeneric("Abunds", 
		function(object) standardGeneric("Abunds"))

#' Retrieve the Abunds slot value of a Leslie object.
#' 
#' @name Abunds
#' @param object A Leslie object

setMethod("Abunds", signature(object="Leslie"),
		function(object) slot(object,"Abunds"))

##########################
########################## Set Outs slot
#' Set generic to  method that sets the Outs slot of the Leslie object.
#' 
#' @name setOuts
#' @param object A Leslie object
#' @param value A list object that holds a collection of list objects, each holding output parameters per year of simulation, to put into the Outs slot
#' @nord
setGeneric("Outs<-", 
		function(object, value)	standardGeneric("Outs<-"))

#' Set the Outs slot of a Leslie object.
#' 
#' @name setOuts
#' @param object A Leslie object
#' @param value A list object that holds a collection of list objects, each holding output parameters per year of simulation, to put into the Outs slot

setReplaceMethod("Outs",signature(object="Leslie"),
		function(object,value) {
			slot(object,"Outs")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the Outs slot value of a Leslie object.
#' 
#' @name Outs
#' @param object A Leslie object
#' @nord
setGeneric("Outs", 
		function(object) standardGeneric("Outs"))

#' Retrieve the Outs slot value of a Leslie object.
#' 
#' @name Outs
#' @param object A Leslie object

setMethod("Outs", signature(object="Leslie"),
		function(object) slot(object,"Outs"))

##########################
########################## Set Size slot
#' Set generic to  method that sets the Size slot of the Leslie object.
#' 
#' @name setSize
#' @param object A Leslie object
#' @param value An integer vector with the size of the matrix (nrow,ncol) 
#' @nord
setGeneric("Size<-", 
		function(object, value)	standardGeneric("Size<-"))

#' Set the Size slot of a Leslie object.
#' 
#' @name setSize
#' @param object A Leslie object
#' @param value An integer vector with the size of the matrix (nrow,ncol)

setReplaceMethod("Size",signature(object="Leslie"),
		function(object,value) {
			slot(object,"Size")<-value
			validObject(object)
			object
		})

#' Set generic to the method that retrieves the Size slot value of a Leslie object.
#' 
#' @name Size
#' @param object A Leslie object
#' @nord
setGeneric("Size", 
		function(object) standardGeneric("Size"))

#' Retrieve the Size slot value of a Leslie object.
#' 
#' @name Size
#' @param object A Leslie object

setMethod("Size", signature(object="Leslie"),
		function(object) slot(object,"Size"))

##########################

############################################ INITIALIZE ####################################################
#' Instantiate a new Leslie object
#' 
#' @name initialize
#' @nord
#' @exportMethod initialize 
setMethod("initialize",
		signature(.Object = "Leslie"),
		function (.Object, ...) 
		{
			.Object@CurrentLeslie<-matrix()
			.Object@Abunds<-numeric()
			.Object@Outs<-data.frame()
			.Object@Size<-integer()
			.Object
		}
)

############################################ LESLIE METHODS #######################################
########################## Create current Leslie
#' Set generic to  method that creates current Leslie matrix from Propensity, Productivity, SurvBreeding and SurvNonBreeding matrices
#' 
#' @name UpdateLeslie
#' @param object A Leslie object
setGeneric("UpdateLeslie",
		function(object,  ...) standardGeneric("UpdateLeslie"))

#' Create current Leslie matrix from Propensity, Productivity, SurvBreeding and SurvNonBreeding matrices
#' 
#' @param object A Leslie object
#' @param propenMx A propensity matrix - must be 10x10
#' @param prodMx A productivity matrix - must be 10x10
#' @param survBrdMx A survivalBreeding matrix - must be 10x10
#' @param survNonBrdMx A survivalNonBreeding matrix - must be 10x10
setMethod("UpdateLeslie", signature(object = "Leslie"),
		function(object, propenMx, prodMx, survBrdMx, survNonBrdMx) {
			ea.call<-match.call()
			if (is.null(object)) stop("A Leslie object is required.")
			if (is.null(propenMx)) stop("A Propensity matrix is required.")
			if (is.null(prodMx)) stop("A Productivity matrix is required.")
			if (is.null(survBrdMx)) stop("A Survival-Breeding matrix is required.")
			if (is.null(survNonBrdMx)) stop("A Survival-nonBreeding matrix is required.")
			sz<-Size(object)
			if("FALSE" %in% (dim(propenMx)==sz)) stop(paste("Propensity matrix is of the wrong dimensions - should be",paste(sz,collapse="x")))
			if("FALSE" %in% (dim(prodMx)==sz)) stop(paste("Productivity matrix is of the wrong dimensions - should be",paste(sz,collapse="x")))
			if("FALSE" %in% (dim(survBrdMx)==sz)) stop(paste("Survival-Breeding matrix is of the wrong dimensions - should be",paste(sz,collapse="x")))
			if("FALSE" %in% (dim(survNonBrdMx)==sz)) stop(paste("Survival-non Breeding matrix is of the wrong dimensions - should be",paste(sz,collapse="x")))
			
			newLeslie<-try(((prodMx %*% propenMx) + (survBrdMx %*% survNonBrdMx)),silent=TRUE)
			if(inherits(newLeslie,"try-error")) stop("Failed to generate the new Leslie - check input matrices' values")
			else CurrentLeslie(object)<-newLeslie
			return(object)
		}
)
########################## 
########################## Provide the stable age distribution from the Current Leslie, set the first Lambda value into the LambdaVals slot, and total abundance value and puts it in the Abunds slot
#' Set generic to  method that creates the stable age distribution from the current Leslie matrix using the characteristic equation to obtain first right eigenvector,
#' then use a total abundance value and puts it in the Abunds and Outs slots. This method is to be called only at the initial setup of a run.
#' 
#' @name GetStableAgeDistribution
#' @param object A Leslie object
setGeneric("GetStableAgeDistribution",
		function(object,  ...) standardGeneric("GetStableAgeDistribution"))

#' Create the stable age distribution from the current Leslie matrix using the characteristic equation to obtain first right eigenvector
#' then use a total abundance value and puts it in the Abunds and Outs slots. This method is to be called only at the initial setup of a run.
#' 
#' @param object A Leslie object
#' @param startAbund An integer with total abundance
setMethod("GetStableAgeDistribution", signature(object = "Leslie"),
		function(object, startAbund) {
			ea.call<-match.call()
			if (is.null(object)) stop("A Leslie object is required.")
			mx<-CurrentLeslie(object)
			eig<-eigen(mx,symmetric=FALSE)
			fev<-eig[["vectors"]][,1]
			eigv<-eigen(mx,symmetric=FALSE,only.values=TRUE)
			lbd<-eigv[["values"]][1]
			if(class(fev[1])=="complex"){
				fev<-Re(fev)
			}
			sadv<-(fev/sum(fev))*startAbund; sadv<-round(sadv,0)
			LambdaVals(object)<-Re(lbd)
			Abunds(object)<-sadv
			Outs(object)<-as.data.frame(t(sadv))
			return(object)
		}
)
########################## 
########################## Calculate current stage abundances and update demographic outputs list, updates the LambdaVals vector too
#' Set generic to  method that updates stage abundances from values of current Leslie and stage abundances in the object, and update list of output parameters and vector of Lambda values
#' 
#' @name UpdateAbunds
#' @param object A Leslie object
setGeneric("UpdateAbunds",
		function(object,  ...) standardGeneric("UpdateAbunds"))

#' Update stage abundances from values of current Leslie and stage abundances in the object, and update list of output parameters
#' 
#' @param object A Leslie object
#' @param timestep Integer, a value for the current timestep
setMethod("UpdateAbunds", signature(object = "Leslie"),
		function(object,timestep) {
			ea.call<-match.call()
			if (is.null(object)) stop("A Leslie object is required.")
			if (is.null(timestep) | (class(timestep)!="integer")) stop("A valid timestep value is required.")
			outsdf<-Outs(object)
			#NOTE: 	The first set of abundances is provided by direct assignment to the slot - all other updates go though this method
			#		Thus, Outs starts with length 1, not 0. As a consequence, the length of Outs should always match the timestep-1
			if(nrow(outsdf)!=timestep-1)stop("The timestep value does not match the current number of rows of the outputs data frame.")
			
			currL<-CurrentLeslie(object)
			abundMx<-Abunds(object)
			newAbund<-try((currL %*% abundMx),silent=TRUE)
			newAbund<-round(newAbund[,1],digits=0)
			if(inherits(newAbund,"try-error")){
				stop("Failed to generate the new abundances - check input matrices' values")
			}else{
				Abunds(object)<-newAbund
			} 
			
			eig<-eigen(currL,symmetric=FALSE,only.values=TRUE)
			lbd<-eig[["values"]][1]
			ldv<-LambdaVals(object);ldv<-c(ldv,Re(lbd))
			LambdaVals(object)<-ldv
			
			nstp<-timestep+1
			newOutsdf<-rbind(outsdf,as.data.frame(t(newAbund)))
			Outs(object)<-newOutsdf
			
			return(object)
		}
)
########################## 
########################## Create trend tables
#' Set generic to a method that creates output tables from the stage abundances (Outs slot) in the object
#' 
#' @name SummarizeLeslie
#' @param object A Leslie object
setGeneric("SummarizeLeslie",
		function(object,  ...) standardGeneric("SummarizeLeslie"))

#' A method that creates output tables from the stage abundances (Outs slot) in the object
#' 
#' @param object A Leslie object
setMethod("SummarizeLeslie", signature(object = "Leslie"),
		function(object) {
			ea.call<-match.call()
			if (is.null(object)) stop("A Leslie object is required.")
			res<-Outs(object)
			if(nrow(res)<5)stop("The outputs table has very little or no data. Check to see that a simulation has been run.")
			names(res)<-c(paste("Age",1:ncol(res),sep=""))
			res$totals<-apply(res,1,sum,na.rm=TRUE)
			res$time<-c(1:(nrow(res)))
			return(res)
		}
)
########################## 

############################################ FUNCTIONS #######################################
## Validate the inputs to the Leslie object slots
#' Function to validate inputs to Leslie object slots. It checks that the CurrentLeslie and Abunds matrices are of the right dimension
#' @param snam A string providing the slot name, whose contents are being validated: CurrentLeslie, Abunds, or Outs
#' @param sval The valid value of the slot. 
#' @param dimv The expected dimension of the Leslie or Abunds matrices
#' @nord
checkLeslieInput<-function(snam,sval,dimv){
	rep<-""
	if(snam=="CurrentLeslie"){
		if("FALSE" %in% (dim(sval)==dimv)){
			rep<-paste("Leslie matrix is of the wrong dimensions - should be",paste(dimv,collapse="x"))
		}
	}else if(snam=="Abunds"){
		if("FALSE" %in% (dim(sval)==dimv)){
			rep<-paste("Abunds matrix is of the wrong dimensions - should be",paste(dimv,collapse="x"))
		}
	}else if(snam=="Outs"){
		rep<-""	#object already checks that it must be a list
	}else{
		rep<-"Not a valid slot name for a Leslie object"
	}
	if(rep!="")class(rep)<-"try-error"
	return(rep)
}


