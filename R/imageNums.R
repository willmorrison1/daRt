#' @export
setMethod("imageNums", "SimulationFilter", function(x) x@imageNums)
#' @export
setMethod("imageNums<-", "SimulationFilter", function(x, value){
    x@imageNums <- value
    if (validObject(x, complete = TRUE)) return(x)
})
#' @export
setMethod("imageNums", "SimulationFiles", function(x) x@simulationFilter@imageNums)
