.simHandleValidity <- function(object){
    errors <- character()
    allPathsDF <- expand.grid(object@simDir, c("input", "output"))
    allPaths <- apply(allPathsDF, 1, function(x) file.path(x[1], x[2]))
    dirMissingBool <- sapply(allPaths, function(x) !dir.exists(x))
    if (any(dirMissingBool)) {
        msg <- paste0("Directory doesn't exist: ",
                      paste0(object@simDir[dirMissingBool], collapse = ","))
        errors <- c(errors, msg)
    }
    #check sim version is consistent
    if (!nrow(object@versionInfo) != 1) {
        errors <- c(errors, "inconsistent version info. expected one row in @versionInfo slot for
                'SimulationHandle' type object")
    }
    #todo-check sim version being used is compatible
    return(ifelse(test = length(errors) == 0,
                  yes = TRUE,
                  no = errors))
}


.dirFilesValidity <- function(object){

    require(stringr)
    errors <- character()
    nFiles <- nrow(object@files)
    if (length(nFiles) == 0) {
        msg <- "No files found."
        errors <- c(errors, msg)
    }
    filesMissing <- sapply(object@files$fileName, function(x) !file.exists(x))
    if (any(filesMissing)) {
        msg <- paste("Missing files:",
                     paste0(
                         paste0("..", str_sub(object@files$fileName[filesMissing], -50)), collapse = "; "))
        errors <- c(errors, msg)
    }
    return(ifelse(test = length(errors) == 0,
                  yes = TRUE,
                  no = errors))
}


.simFilterValidity <- function(object){
    bandsPrompt <- "Set as e.g. c('BAND0', 'BAND1')"
    itersPrompt <- "Set as e.g. c('ITER1', 'ILLUDIR')"
    splitVarsPrompt <- "'typeNum' is invalid. Should be '[numeric]_[character]' e.g. '2_Ground'"
    allowedVariables <- c("BRF", "RADIATIVE_BUDGET", "Tapp", "Radiance")
    variablesPrompt <- paste("Invalid variables. Should be ONE of:",
                             paste0(allowedVariables, collapse = ","))
    errors <- character()
    #allowed projects
    allowedProducts <- c("directions", "rb3D", "images")
    product <- try(match.arg(arg = product(object), choices = allowedProducts,
                             several.ok = FALSE), silent = TRUE)
    #allowed products
    if (class(product) == "try-error") {
        errors <- c(errors, paste0("Product set to ", product(object), " but must
                                   be one of ", paste0(allowedProducts, collapse = ",")))
    }
    #allowed bands
    if (all(object@bands == "")) errors <- c(errors, paste("Empty bands.", bandsPrompt))
    if (any(!grepl("BAND", object@bands))) errors <- c(errors, paste("Invalid bands.", bandsPrompt))

    #allowed variables
    if (length(object@variables) > 1 || object@variables == "") {
        errors <- c(errors, variablesPrompt)
    }
    if (!any(allowedVariables %in% object@variables)) {
        errors <- c(errors, variablesPrompt)
    }
    #allowed iters
    if (all(object@iters == "")) errors <- c(errors, paste("Empty iters.", itersPrompt))
    if (any(!grepl("ITER|ILLUDIR|ILLUDIFF", object@iters))) {
        errors <- c(errors, paste("Invalid iters.", itersPrompt))
    }
    #allowed RB3Dvars
    if (all(object@variablesRB3D == "")) errors <- c(errors, "Empty RB3D.")
    #allowed typeNums
    if (object@typeNums != "") {
        splitVars <- strsplit(object@typeNums, split = "_")
        for (i in 1:length(splitVars)) {
            if (length(splitVars[[i]]) < 2) {
                errors <- c(errors, paste(object@typeNums[i], splitVarsPrompt))
            }
            if (is.na(as.numeric(splitVars[[i]][1]))) {
                errors <- c(errors, paste(object@typeNums[i], splitVarsPrompt))
            }
        }
    }
    #allowed images
    if (all(object@imageType == "")) errors <- c(errors, "No 'imageType'.")
    if (any(!grepl("ima|camera", object@imageType))) {
        errors <- c(errors, paste("Invalid imageType. Should be either/or 'ima,camera'"))
    }
    #allowed imageNo
    if (any(is.na(object@imageNo))) {
        errors <- c(errors, "imageNo contains non-numeric (NAs)")
    }

    return(ifelse(test = length(errors) == 0,
                  yes = TRUE,
                  no = errors))
}


.dirValidity <- function(object){
    errors <- character()

    if (!is.data.frame(object@data)) {
        msg <- "Data slot for directions is not data.frame"
        errors <- c(errors, msg)
    }
    if (nrow(object@data) == 0) {
        msg <- "No data for directions object"
        errors <- c(errors, msg)
    }
    if (!any(colnames(object@data) %in%
             c("zenith", "azimuth", "value", "band", "variable", "iter", "typeNum"))) {
        msg <- "Column name mismatch for directions data"
        errors <- c(errors, msg)
    }
    return(ifelse(test = length(errors) == 0,
                  yes = TRUE,
                  no = errors))
}


.RB3DFilesValidity <- function(object){
    TRUE
}


.RB3DValidity <- function(object){
    TRUE
}

.imagesValidity <- function(object){
    TRUE
}


#' SimulationFilter class.
#'
#' @slot bands character.
#' @slot variables character.
#' @slot iters character.
#' @slot variablesRB3D character.
#' @slot typeNums character.
#' @slot imageType character.
#' @slot imageNo numeric.
#' @slot product character.
#'
#' @return
#' @export
#' @seealso \code{\link{simulationFilter}}
#'
#' @examples
setClass(
    Class = "SimulationFilter",
    slots = list(bands = "character",
                 variables = "character",
                 iters = "character",
                 variablesRB3D = "character",
                 typeNums = "character",
                 imageType = "character",
                 imageNo = "numeric",
                 product = "character"))
setValidity("SimulationFilter", .simFilterValidity)


setClass(
    Class = "SimulationHandle",
    slots = list(simDir = "character",
                 simName = "character",
                 isSequence = "logical",
                 sequenceInfo = "data.frame",
                 versionInfo = "data.frame"))
setValidity("SimulationHandle", .simHandleValidity)

#todo-error message on missing files also returns the files that exist


#' SimulationFiles class
#'
#'
#' @name SimulationFiles-class
#' @description An S4 class to represent the files within a simulation or simulations.
#' Created using the \code{\link{getFiles}} method. Specific files within the class are modified
#' by the object with class \link{SimulationFilter-class}
#'
#' @slot simulationFilter contains \link{SimulationFilter-class} object
#' @slot files a data.frame, with each row describing the file
#' @slot sequenceInfoList a list, with each list element showing the variable permutation(s) within this specific simulation sequence.
#'
#' @return
#' @export
#'
#' @examples
#'
setClass(
    Class = "SimulationFiles", contains = "SimulationHandle",
    slots = list(simulationFilter = "SimulationFilter",
                 files = "data.frame",
                 sequenceInfoList = "list"))
setValidity("SimulationFiles", .dirFilesValidity)


#' Generic SimulationData class
#'
#' @name SimulationData-class
#' @description Generic SimulationData class that extends to data classes for specific DART products
#' @slot data data.frame.
#'
#' @return
#' @export
#' @seealso \link{Images-class} \link{Directions-class} \link{RB3D-class}
#' @examples
setClass(
    Class = "SimulationData", contains = "SimulationFiles",
    slots = list(data = "data.frame"))


#' Directions data class
#'
#' @name Directions-class
#' @description Directions data class that extends \link{SimulationData-class} class.
#' @return
#' @export
#'
#' @examples
setClass(
    Class = "Directions", contains = "SimulationData")
setValidity("Directions", .dirValidity)


#' Images data class
#'
#' @name Images-class
#' @description Image data class extends \link{SimulationData-class} class.
#' @return
#' @export
#'
#' @examples
setClass(
    Class = "Images", contains = "SimulationData")
setValidity("Images", .imagesValidity)


#' RB3D class
#'
#' @name RB3D-class
#' @description RB3D (Radiative Budget 3D) class that extends \link{SimulationData-class} class.
#' @return
#' @export
#'
#' @examples
setClass(
    Class = "RB3D", contains = "SimulationData")
setValidity("RB3D", .RB3DValidity)
