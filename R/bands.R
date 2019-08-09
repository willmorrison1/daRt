#' @export
setMethod("bands", "SimulationFilter", function(x) x@bands)
#' @export
setMethod("bands<-", "SimulationFilter", function(x, value){
    x@bands <- value
    if (validObject(x, complete = TRUE)) return(x)
})
#' @export
setMethod("bands", "SimulationFiles", function(x) x@simulationFilter@bands)
