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
setGeneric(name = "imageTypes<-", def = function(x, value) standardGeneric("imageTypes<-"))
#' @rdname SimulationFilter-class
setGeneric(name = "imageNums<-", def = function(x, value) standardGeneric("imageNums<-"))

#' Access object information

#' @name accessors
#' @param x \linkS4class{SimulationFilter} or \linkS4class{SimulationFiles} class
#' @description Generic functions to access information from the objects with classes defined in this package
#' @examples
#' sF <- simulationFilter(product = "directions")
#' bands(sF)
#'
#' \dontrun{
#' #access information within SimulationFiles object
#' #define the simulation directory
#' simDir <- "C:/Users/<Username>/DART/user_data/simulations/cesbio/"
#' simFiles <- getFiles(simDir)
#' #show bands that are selected
#' bands(simFiles)
#' #show 'type numbers' that have been selected
#' typeNums(simFiles)
#' }
#'


#' @rdname accessors
setGeneric(name = "product", def = function(x) standardGeneric("product"))
#' @rdname accessors
setGeneric(name = "simname", def = function(x) standardGeneric("simname"))
#' @rdname accessors
setGeneric(name = "fileName", def = function(x) standardGeneric("fileName"))
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
setGeneric(name = "imageTypes", def = function(x) standardGeneric("imageTypes"))
#' @rdname accessors
setGeneric(name = "imageNums", def = function(x) standardGeneric("imageNums"))


#' @rdname SimulationFiles-class
setGeneric(name = "baseDir", def = function(x) standardGeneric("baseDir"))

#' @export
#' @rdname SimulationFiles-class
setGeneric(name = "simulationFilter<-", def = function(x, value)
    standardGeneric("simulationFilter<-"))

#' @rdname SimulationFiles-class
#' @rdname SimulationFilter-class
setGeneric(name = "subDir", def = function(x)
    standardGeneric("subDir"))

#' Create \linkS4class{SimulationFilter} class
#'
#' @name simulationFilter
#' @description Function for creating the \linkS4class{SimulationFilter} class. Define a product, then
#' Optional arguments of: `bands`, `variables`, `iterations`, `variablesRB3D`,
#' `typeNums`, `imageTypes`, `imageNums`. See \link{SimulationFilter-class} for full
#' description.
#' @param product One of: `directions`, `rb3D`, `images`.
#' @param x \link{SimulationFiles-class} object if product is missing.
#'
#' @return \linkS4class{SimulationFilter} type object
#' @export
#' @seealso \code{\link{SimulationFilter-class}}
#' @examples
#' sF <- daRt::simulationFilter(product = "images",
#'                              bands = as.integer(0:2),
#'                              iters = c("ITER1", "ITER2"),
#'                              variables = "BRF",
#'                              imageNums = as.integer(c(5, 7)),
#'                              imageTypes = c("ima", "ima_transmittance"))
#'

setGeneric(name = "simulationFilter", def = function(product = "character", x, ...)
    standardGeneric("simulationFilter"))

#' Get DART output filenames
#'
#' @name getFiles
#' @description Function for getting \link{SimulationFiles-class} type object. Useful to perform a 'dry run' of
#' \link{getData} by exploring the files that will vary based on the contents of \code{x} and
#' the configuration of \code{sF}.
#'
#' @param x simulation directory or directories (character)
#' @param sF \link{SimulationFilter-class} object
#' @param ... Optional arguments of: \code{nCores}: number of cores to use when loading data.
#' @export
#'
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
#' @export
#'
setGeneric(name = "getData", signature = c("x", "sF"), def = function(x, sF, ...)
    standardGeneric("getData"))

#' wavelengths
#' @description Get full information on wavelengths for each band
#' @param x sF \link{SimulationFiles-class}
#' @return data frame
#' @export
#'
setGeneric(name = "wavelengths", def = function(x = "SimulationFiles")
    standardGeneric("wavelengths"))




#' Aggregate images to single values
#' @title imagesToDirectionsDF
#' @description Convert an \link{Images-class} object to a {Directions-class} object
#'
#' @param x \link{Images-class} object
#' @param fun Function to apply across each image.
#'
#' @return data frame
#' @export
#'
setGeneric(name = "imagesToDirectionsDF", def = function(x, fun) standardGeneric("imagesToDirectionsDF"))


#' Simulation version info
#' @title versionInfo
#' @description Get the version used for the given simulation data
#' @param x \link{SimulationFiles-class} object
#'
#' @export
setGeneric(name = "versionInfo", def = function(x) standardGeneric("versionInfo"))


#' Return resource use
#'
#' @title ResourceUse
#' @description Return a data frame with information on the resource use for a \link{SimulationFiles-class} type object
#' @param x \link{SimulationFiles-class} type object
#'
#' @export
setGeneric(name = "resourceUse", def = function(x = "SimulationFiles") standardGeneric("resourceUse"))

#' Get data frame of all sequence parameters
#' @title sequenceParameters
#' @description Return a data frame where rows describe a parameter (parametre*) for a simulation (simName).
#' @param   \link{SimulationFiles-class} or \link{SimulationData-class} class object
#'
#' @return data frame
#' @export
#'
setGeneric(name = "sequenceParameters", def = function(x) standardGeneric("sequenceParameters"))

#' Remove underlying orography
#' @title removeRelief
#' @description Remove underlying orography from a \link{RB3D-class} dataset using a digital elevation
#' model (DEM) of class \code{RasterLayer} that is georeferenced to \link{RB3D-class}.
#'
#' @param x \link{RB3D-class} type object.
#' @param DSM \code{RasterLayer} type object with height above ground level (m) and - preferably - a finer
#' @param BOAextrapolation Character. When the 3D radiative budget is height-adjusted, the BOA layer is no longer
#' plane-parallel with the ground. How to make the BOA layer plane-parallel with the grund? One of "extrapolate" or "clip".
#' Extrapolate: the highest BOA cell with a recorded value is the new BOA layer. Other cells in this horizontal layer
#' may be empty. Empty cells are filed using values from lower vertical layers (most accurate, most cells).
#' Clip: the first BOA cell where all cells in its horizontal layer have a recorded value is the new BOA layer.
#' All cells above this layer are removed. (Least accurate, least cells).
#' @param `maxUndergroundCells` Integer. How many cells below the "ground" should be kept? I.e. the 3D RB array will
#' be offset with Z=0 as the new ground level, and Z=-maxUndergroundCells as the lowest elevation to keep. Cells below
#' -maxUndergroundCells are removed as this saves a lot of memory. If there is lots of small-scale variation in topography then
#' this parameter should be relaxed at the expense of array size and memory usage.
#' @export
#'
setGeneric(name = "removeRelief", def = function(x = "RB3D", DEM = "RasterLayer", ...)
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
#' @return \link{SimulationFiles-class} type object.
#' @export
#'
setGeneric(name = "rb3DtoNc", def = function(x = "SimulationFiles", ...)
    standardGeneric("rb3DtoNc"))

#' Delete potentially large input files
#' @title deleteFiles
#' @description DART input files can be very large. This function deletes those large files
#' that are not required for post-processing of data in this package.
#'
#'
#' @param x \link{SimulationFiles-class} type object.
#' @param deleteSimulationFiles logical A hard check that you are happy to delete the files in x, shown by files(x).
#' @param ... \code{trianglesInput} remove "triangles" input files? (bool)
#' @param ... \code{trianglesOutput} remove "triangles.txt" output file? (bool)
#' @param ... \code{maketOutput} remove "maket.txt" output file? (bool)
#'
#' @return NULL
#' @export
#'
setGeneric(name = "deleteFiles", def = function(x = "SimulationFiles", deleteSimulationFiles = "logical", ...)
    standardGeneric("deleteFiles"))

#' Convert Tapp to Radiance
#' @title tappToRadiance
#' @description Convert Tapp (K) to Radiance (W m2 sr-1 um-1) using Planck function at the equivalent Band wavelength
#'
#' @param x \link{SimulationData-class} type object.
#'
#' @return \link{SimulationData-class} type object.
#' @export
#'
setGeneric(name = "tappToRadiance", def = function(x = "SimulationData")
    standardGeneric("tappToRadiance"))

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

