#' @export
setMethod("imageType", "SimulationFilter", function(x) x@imageType)
#' @export
setMethod("imageType<-", "SimulationFilter", function(x, value){
    x@imageType <- value
    if (validObject(x, complete = TRUE)) return(x)
})
#' @export
setMethod("imageType", "SimulationFiles", function(x) x@simulationFilter@imageType)
