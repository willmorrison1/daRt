.simHandleValidity <- function(object){

    errors <- character()
    allPathsDF <- expand.grid(baseDir(object), c("input", "output"))
    allPaths <- apply(allPathsDF, 1, function(x) file.path(x[1], x[2]))
    dirMissingBool <- sapply(allPaths, function(x) !dir.exists(x))
    if (any(dirMissingBool)) {
        msg <- paste0("Directory doesn't exist: ",
                      paste0(allPaths[dirMissingBool], collapse = ",  "))
        #first and most important check - if this fails then all else will fail
        #so return error message here
        return(msg)
    }
    #check sim version is consistent
    if (!nrow(object@softwareVersion) == 1) {
        errors <- c(errors, "Inconsistent version info. Expected one row in @softwareVersion slot for
                'SimulationHandle' type object")
    }

    #check sim version being used is compatible
    if (!dplyr::between(x = object@softwareVersion$buildNumber,
                        left = .__daRtVersion["minBuildVersion"],
                        right = .__daRtVersion["maxBuildVersion"])) {
        errors <- c(errors, paste("DART build:", object@softwareVersion$buildNumber,
                                  "is incompatible/untested with daRt package."))
    }
    #check if there are sequences/nonsequences together- won't work. i.e. sequenceInfo should
    #have length 1
    if (length(unique(object@isSequence)) != 1) {
        errors <- c(errors, "Simulations contain 'sequence' and 'non-sequence' type simulations.
                    Can only have one or the other.")
    }


    return(ifelse(test = length(errors) == 0,
                  yes = TRUE,
                  no = errors))
}


.simFilesValidity <- function(object){

    errors <- character()
    nFiles <- nrow(object@files)
    if (length(nFiles) == 0) {
        msg <- "No files found."
        errors <- c(errors, msg)
    }
    if (Sys.info()["sysname"] == "Windows") {
        lenDirs <- nchar(object@files$fileName)
        if (any(lenDirs > 259)) {
            msg <- "Files with paths longer than 259 found. Not compatible with windows."
            errors <- c(errors, msg)
        }
    }

    if (nrow(object@wavelengths) == 0) {
        errors <- c(errors, "No data in @wavelengths slot.
                    A SimulationFiles method has not set the 'wavelengths' slot.")
    }

    return(ifelse(test = length(errors) == 0,
                  yes = TRUE,
                  no = errors))
}


.allowedVariables <- function() {

    c(
        "BRF",
        "RADIATIVE_BUDGET",
        "Tapp",
        "Radiance"
    )

}


.allowedProducts <- function() {

    c(
        "directions",
        "rb3D",
        "images"
    )
}


.allowedImageTypes <- function() {

    c(
        "ima",
        "camera",
        "ima_transmittance",
        "camera_transmittance",
        ""
    )
}


.simFilterValidity_bands <- function(object) {

    .bandsErr <- function() paste("Invalid bands. Set as numeric e.g. 0:1 for bands 'BAND0' and 'BAND1'")
    bandVals <- bands(object)
    if (any(bandVals < 0)) return(.bandsErr())

    return()
}


.simFilterValidity_variables <- function(object) {

    .variablesErr <- function() {
        paste("Invalid variables. Must be one of:",
              paste0(.allowedVariables(), collapse = ";"))
    }

    varVals <- variables(object)

    if (length(varVals) > 1 || varVals == "")
        return(.variablesErr())

    if (!any(.allowedVariables() %in% varVals))
        return(.variablesErr())

    return()

}


.simFilterValidity_product <- function(object) {

    productVals <- product(object)
    product <- try(match.arg(arg = productVals, choices = .allowedProducts(),
                             several.ok = FALSE), silent = TRUE)

    if (class(product) == "try-error") {
        return(paste0("Product set to ", productVals, " but must
                                   be one of ",
                      paste0(.allowedProducts(), collapse = ";")))
    }

    return()
}


.simFilterValidity_iters <- function(object) {

    .itersErr <- function() "Invalid iters. Set as e.g. c('ITER1', 'ILLUDIR')"

    iterVals <- iters(object)
    grepValues_iter <- c("ITER", "ILLUDIR", "ILLUDIFF", "ORDER", "ORDRE")
    itersWithNoNumber <- c("ILLUDIR", "ILLUDIFF")

    if (all(iterVals == "")) return(paste("Empty iters.", .itersErr()))
    nonNumericIterExceptions <- c("X")

    for (i in 1:length(iterVals)) {
        iterInd <- sapply(grepValues_iter, function(x) grepl(x, iterVals[i]))
        if (length(which(iterInd)) != 1) return(paste(iterVals[i], .itersErr()))
        iterSplit <- strsplit(iterVals[i], grepValues_iter[iterInd])[[1]]
        if (!grepValues_iter[iterInd] %in% itersWithNoNumber) {
            if (length(iterSplit) != 2) return(paste(iterVals[i], .itersErr()))
            if (iterSplit[1] != "") return(paste(iterVals[i], .itersErr()))
            if (!nonNumericIterExceptions %in% iterSplit[2]) {
                if (is.na(as.numeric(iterSplit[2]))) return(paste(iterVals[i], .itersErr()))
            }
        }
    }

    return()

}


.simFilterValidity_variablesRB3D <- function(object) {

    varRB3DVals <- variablesRB3D(object)

    if (all(varRB3DVals == "") & product(object) == "rb3D") return("No RB3D variables set.")

    return()

}


.simFilterValidity_typeNums <- function(object) {
    typeNumVals <- typeNums(object)

    .typeNumErr <- function() {
        "'typeNum' is invalid. Should be unique vector of '[numeric]_[character]' e.g. '2_Ground'. \n
        See input/dart.typ for list of dart surface types e.g. 101_Default_Object if using
        an imported 3D model with no grouping of triangles."
    }

    for (i in 1:length(typeNumVals)) {
        if (typeNumVals[i] == "") next
        splitVars <- strsplit(typeNumVals[i], split = "_")[[1]]
        if (length(splitVars) < 2) return(paste(typeNumVals[i], .typeNumErr()))
        if (is.na(as.numeric(splitVars[1]))) return(paste(typeNumVals[i], .typeNumErr()))
    }

    if (length(typeNumVals) != length(unique(typeNumVals))) return(.typeNumErr())

}


.simFilterValidity_images <- function(object) {
    imageTypesVal <- imageTypes(object)
    if (all(imageTypesVal == "")) return("No 'imageTypes' entered")
    if (any(!imageTypesVal %in% .allowedImageTypes())) {
        return(paste("Invalid imageTypes. Should be any of:",
                     paste(.allowedImageTypes(), collapse = ";")))
    }

    return()

}


.simFilterValidity_imageNums <- function(object) {

    imageNumsVal <- imageNums(object)
    if (any(is.na(imageNumsVal))) return("imageNums contains non-numeric (NAs)")

    return()
}


.simFilterValidity <- function(object){

    errors <- character()
    #allowed products
    errors <- c(errors, .simFilterValidity_product(object))
    #allowed bands
    errors <- c(errors, .simFilterValidity_bands(object))
    #allowed variables
    errors <- c(errors, .simFilterValidity_variables(object))
    #allowed iters
    errors <- c(errors, .simFilterValidity_iters(object))
    #allowed rb3D vars
    errors <- c(errors, .simFilterValidity_variablesRB3D(object))
    #allowed images
    errors <- c(errors, .simFilterValidity_images(object))
    #allowed imageNums
    errors <- c(errors, .simFilterValidity_imageNums(object))
    #allowed typeNums
    errors <- c(errors, .simFilterValidity_typeNums(object))

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
#' @slot bands integer e.g. 0 for "BAND0"
#' @slot variables character e.g. "BRF".
#' @slot iters character e.g. "ITERX".
#' @slot variablesRB3D character e.g. "Irradiance".
#' @slot typeNums character e.g. "2_Ground".
#' @slot imageTypes character e.g. "ima".
#' @slot imageNums integer
#' @slot product character e.g. "directions".
#'
#' @export
#' @seealso \code{\link{simulationFilter}}
#'
setClass(
    Class = "SimulationFilter",
    slots = list(bands = "integer",
                 variables = "character",
                 iters = "character",
                 variablesRB3D = "character",
                 typeNums = "character",
                 imageTypes = "character",
                 imageNums = "integer",
                 product = "character"))
setValidity("SimulationFilter", .simFilterValidity)


setClass(
    Class = "SimulationHandle",
    slots = list(baseDir = "character",
                 simName = "character",
                 isSequence = "logical",
                 sequenceInfo = "data.frame",
                 softwareVersion = "data.frame"))
setValidity("SimulationHandle", .simHandleValidity)


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
#' @slot sequenceInfoDf a data frame, with each row containing one simulation, and each column a parameter ('parametre') specific to the sequence.
#' A condensed version of sequenceInfoList.
#' @slot wavelengths a data frame containing spectral information on each band for each simulation
#' @slot sunAngles a data frame containing sun angles straight from simulation.properties.txt
#'
#' @export
#'
setClass(
    Class = "SimulationFiles", contains = "SimulationHandle",
    slots = list(simulationFilter = "SimulationFilter",
                 files = "data.frame",
                 sequenceInfoList = "list",
                 sequenceInfoDf = "data.frame",
                 wavelengths = "data.frame",
                 sunAngles = "data.frame"))

setValidity("SimulationFiles", .simFilesValidity)


#' Generic SimulationData class
#'
#' @name SimulationData-class
#' @description Generic SimulationData class that extends to data classes for specific DART products
#' @slot data data.frame.
#'
#' @export
#' @seealso \link{Images-class} \link{Directions-class} \link{RB3D-class}
setClass(
    Class = "SimulationData", contains = "SimulationFiles",
    slots = list(data = "data.frame"))


#' Directions data class
#'
#' @name Directions-class
#' @description Directions data class that extends \link{SimulationData-class} class.
#' @export
#'
setClass(
    Class = "Directions", contains = "SimulationData")
setValidity("Directions", .dirValidity)


#' Images data class
#'
#' @name Images-class
#' @description Image data class extends \link{SimulationData-class} class.
#' @export
#'
setClass(
    Class = "Images", contains = "SimulationData")
setValidity("Images", .imagesValidity)


#' RB3D class
#'
#' @name RB3D-class
#' @description RB3D (Radiative Budget 3D) class that extends \link{SimulationData-class} class.
#' @export
#'
setClass(
    Class = "RB3D", contains = "SimulationData")
setValidity("RB3D", .RB3DValidity)
