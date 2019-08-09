#' @export
setMethod("imageNo", "SimulationFilter", function(x) x@imageNo)
#' @export
setMethod("imageNo<-", "SimulationFilter", function(x, value){
    x@imageNo <- value
    if (validObject(x, complete = TRUE)) return(x)
})
#' @export
setMethod("imageNo", "SimulationFiles", function(x) x@simulationFilter@imageNo)
