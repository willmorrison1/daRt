.simHandleValidity <- function(object){
    errors <- character()
    print("validating sim handle(s)")
    allPathsDF <- expand.grid(object@simDir, c("input", "output"))
    allPaths <- apply(allPathsDF, 1, function(x) file.path(x[1], x[2]))
    dirMissingBool <- sapply(allPaths, function(x) !dir.exists(x))
    if (any(dirMissingBool)) {
        msg <- paste0("Directory doesn't exist: ",
                      paste0(object@simDir[dirMissingBool], collapse = ","))
        errors <- c(errors, msg)
    }
    #todo-check sim version is consistent

    #todo-check sim version being used is compatible
    return(ifelse(test = length(errors) == 0,
                  yes = TRUE,
                  no = errors))
}


.dirFilesValidity <- function(object){

    require(stringr)
    print("validating directions files")
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


#todo:simFilter() needs some basic checks e.g.
#grepl "BAND"
#grepl "BRF", Tapp, Radiance
#grepl iter
#grepl [vars3Drb hmm]
#grepl strsplit(_) first part numeric
.simFilterValidity <- function(object){
    errors <- character()
    allowedProducts <- c("directions", "rb3D", "images")
    product <- try(match.arg(arg = product(object), choices = allowedProducts,
                         several.ok = FALSE), silent = TRUE)
    if (class(product) == "try-error") {
        errors <- c(errors, paste0("Product set to ", product(object), " but must
                                   be one of ", paste0(allowedProducts, collapse = ",")))
    }

    return(ifelse(test = length(errors) == 0,
                  yes = TRUE,
                  no = errors))
}


.dirValidity <- function(object){
    print("validating directions")
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
                 sequenceInfo = "data.frame"))
setValidity("SimulationHandle", .simHandleValidity)

#todo-error message on missing files also returns the files that exist


#' SimulationFiles class
#'
#'
#' @name SimulationFiles-class
#' @description An S4 class to represent the files within a simulation or simulations.
#' Created using the \code{\link{getFiles}} method. Specific files within the class are modified
#' by the object with class \link{SimulationFilter-class}
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
