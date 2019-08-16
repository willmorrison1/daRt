#' @rdname SimulationFilter-class
setGeneric(name = "product<-", def = function(x, value) standardGeneric("product<-"))
#' @rdname SimulationFilter-class
setGeneric(name = "iters<-", def = function(x, value) standardGeneric("iters<-"))
#' @rdname SimulationFilter-class
setGeneric(name = "bands<-", def = function(x, value) standardGeneric("bands<-"))
#' @rdname SimulationFilter-class
setGeneric(name = "variablesRB3D<-", def = function(x, value) standardGeneric("variablesRB3D<-"))
#' @rdname SimulationFilter-class
setGeneric(name = "variables<-", def = function(x, value) standardGeneric("variables<-"))
#' @rdname SimulationFilter-class
setGeneric(name = "typeNums<-", def = function(x, value) standardGeneric("typeNums<-"))
#' @rdname SimulationFilter-class
setGeneric(name = "imageType<-", def = function(x, value) standardGeneric("imageType<-"))
#' @rdname SimulationFilter-class
setGeneric(name = "imageNo<-", def = function(x, value) standardGeneric("imageNo<-"))

#' Access object information
#'
#' @param x
#' @name accessors
#' @description Generic functions to access information from the objects with classes
#' defined in this package
#' @examples
#' #get SimulationFilter object with default values
#' sF <- simulationFilter(product = "directions")
#' #show all default values
#' sF
#' #show bands
#' bands(sF)
#' \dontrun{
#' #access information within SimulationFiles object
#' #define the simulation directory
#' simDir <- "C:/Users/.../DART/user_data/simulations/cesbio/
#' simFiles <- getFiles(simDir)
#' #show bands that are selected
#' bands(simFiles)
#' #show 'type numbers' that have been selected
#' typeNums(simFiles)
#' }
#'
NULL
#' @rdname accessors
setGeneric(name = "product", def = function(x) standardGeneric("product"))
#' @rdname accessors
setGeneric(name = "simname", def = function(x) standardGeneric("simname"))
#' @rdname accessors
setGeneric(name = "files", def = function(x) standardGeneric("files"))
#' @rdname accessors
setGeneric(name = "bands", def = function(x) standardGeneric("bands"))
#' @rdname accessors
setGeneric(name = "iters", def = function(x) standardGeneric("iters"))
#' @rdname accessors
setGeneric(name = "variables", def = function(x) standardGeneric("variables"))
#' @rdname accessors
setGeneric(name = "variablesRB3D", def = function(x) standardGeneric("variablesRB3D"))
#' @rdname accessors
setGeneric(name = "typeNums", def = function(x) standardGeneric("typeNums"))
#' @rdname accessors
setGeneric(name = "imageType", def = function(x) standardGeneric("imageType"))
#' @rdname accessors
setGeneric(name = "imageNo", def = function(x) standardGeneric("imageNo"))

#' @rdname SimulationFiles-class
setGeneric(name = "simdir", def = function(x) standardGeneric("simdir"))

#' Create \linkS4class{SimulationFilter} class
#'
#' @name simulationFilter
#' @description Function for creating the \linkS4class{SimulationFilter} class
#' @param product One of "directions", "rb3D", "images".
#' @param ...
#'
#' @return
#' @export
#' @seealso \code{\link{SimulationFilter-class}}
#' @examples
#'
setGeneric(name = "simulationFilter", def = function(product = "character", ...)
    standardGeneric("simulationFilter"))

#' Get DART output filenames
#'
#' @name getFiles
#' @descriptio Function for getting \link{SimulationFiles-class} type object. Useful to perform a 'dry run' of
#' \link{getData} by exploring the files that will vary based on the contents of \code{x} and
#' the configuration of \code{sF}.
#'
#' @param x simulation directory or directories (character)
#' @param sF \link{SimulationFilter-class} object
#'
#' @return
#' @export
#'
#' @examples
setGeneric(name = "getFiles", def = function(x = "character", sF = "SimulationFilter")
    standardGeneric("getFiles"))

#' Main function: get DART data
#'
#' Main function to get data from DART simulation outputs in a friendly 'long' data format that is part of
#' an object that extends a \link{SimulationData-class} type object
#' @name getData
#' @param x simulation directory or directories (character) or \link{SimulationFiles-class} object
#' @param sF \link{SimulationFilter-class} if x = \code{character}
#'
#' @return
#' @export
#'
#' @examples
setGeneric(name = "getData", signature = c("x", "sF"), def = function(x, sF)
    standardGeneric("getData"))

#' @export
setGeneric(name = "versionInfo", def = function(x) standardGeneric("versionInfo"))

setGeneric(name = "directions", def = function(x = "character")
    standardGeneric("directions"))
setGeneric(name = "directionsFiles", def = function(x = "character", sF = "SimulationFilter")
    standardGeneric("directionsFiles"))
setGeneric(name = "images", def = function(x = "character")
    standardGeneric("images"))
setGeneric(name = "imageFiles", def = function(x = "character", sF = "SimulationFilter")
    standardGeneric("imageFiles"))
setGeneric(name = "rb3D",  def = function(x = "character")
    standardGeneric("rb3D"))
setGeneric(name = "rb3DFiles", def = function(x = "character", sF = "SimulationFilter")
    standardGeneric("rb3DFiles"))
setGeneric(name = "simulationHandle",  def = function(x = "character")
    standardGeneric("simulationHandle"))
