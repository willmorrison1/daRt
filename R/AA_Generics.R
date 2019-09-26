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
#' simDir <- "C:/Users/<Username>/DART/user_data/simulations/cesbio/
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
#' @param ... Optional arguments of: \code{nCores}: number of cores to use when loading data.
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
setGeneric(name = "getData", signature = c("x", "sF"), def = function(x, sF, ...)
    standardGeneric("getData"))

#' Aggregate images to single values
#' @title imagesToDirectionsDF
#' @description Convert an \link{Images-class} object to a {Directions-class} object
#'
#' @param x \link{Images-class} object
#' @param fun Function to apply across each image.
#'
#' @return
#' @export
#'
setGeneric(name = "imagesToDirectionsDF", def = function(x, fun) standardGeneric("imagesToDirectionsDF"))

#' Return resource use
#' @title ResourceUse
#' @description Return a data frame with information on the resource use for a \link{SimulationFiles-class} type object
#' @param x \link{SimulationFiles-class} type object
#' @export
setGeneric(name = "versionInfo", def = function(x) standardGeneric("versionInfo"))

#' @param x
#'
#' @export
setGeneric(name = "resourceUse", def = function(x = "SimulationFiles") standardGeneric("resourceUse"))

#' Get data frame of all sequence parameters
#' @title sequenceParameters
#' @description return a data frame. A row describes the parameters (parametre*) for a simulation (simName).
#' @param   \link{SimulationFiles-class} or \link{SimulationData-class} class object
#'
#' @return
#' @export
#'
#' @examples
setGeneric(name = "sequenceParameters", def = function(x) standardGeneric("sequenceParameters"))

#' Remove underlying orography
#' @title removeRelief
#' @description Remove underlying orography from a \link{RB3D-class} dataset using a digital elevation
#' model (DEM) of class \code{RasterLayer} that is georeferenced to \link{RB3D-class}.
#'
#' @param x \link{RB3D-class} type object.
#' @param DSM \code{RasterLayer} type object with height above ground level (m) and - preferably - a finer
#' horizontal reoslution than that of the radiative budget cells in x. The center of the DSM must be georeferenced
#' to the center of the radiarive budget data in x. The DSM can have a larger extent than x.
#' @return
#' @export
#'
setGeneric(name = "removeRelief", def = function(x = "RB3D", DEM = "RasterLayer")
    standardGeneric("removeRelief"))


#' Convert radiative budget .bin to .nc
#' @title rb3DtoNc
#' @description DART radiative budget .bin files can be very large. This function replaces all
#' .bin files with .nc files, which can be compressed and are faster to read.
#'
#'
#' @param x \link{SimulationFiles-class} type object.
#' @param ncCompressionFactor Compression factor (0 - 9) for writing ncdf files (see ncdf4 package)
#'
#' @return
#' @export
#'
#' @examples
setGeneric(name = "rb3DtoNc", def = function(x = "SimulationFiles", ...)
    standardGeneric("rb3DtoNc"))

#' Delete potentially large input files
#' @title deleteFiles
#' @description DART input files can be very large. This function deletes those large files
#' that are not required for post-processing of data in this package.
#'
#'
#' @param x \link{SimulationFiles-class} type object.
#' @param trianglesInput remove "triangles" input files? (bool)
#' @param maketOutput remove "maket.txt" output file? (bool)
#'
#' @return
#' @export
#'
#' @examples
setGeneric(name = "deleteFiles", def = function(x = "SimulationFiles",
                                                trianglesInput = "logical",
                                                maketOutput = "logical")
    standardGeneric("deleteFiles"))

setGeneric(name = "directions", def = function(x = "character", ...)
    standardGeneric("directions"))
setGeneric(name = "directionsFiles", def = function(x = "character", sF = "SimulationFilter")
    standardGeneric("directionsFiles"))
setGeneric(name = "images", def = function(x = "character", ...)
    standardGeneric("images"))
setGeneric(name = "imageFiles", def = function(x = "character", sF = "SimulationFilter")
    standardGeneric("imageFiles"))
setGeneric(name = "rb3D",  def = function(x = "character", ...)
    standardGeneric("rb3D"))
setGeneric(name = "rb3DFiles", def = function(x = "character", sF = "SimulationFilter")
    standardGeneric("rb3DFiles"))
setGeneric(name = "simulationHandle",  def = function(x = "character")
    standardGeneric("simulationHandle"))
