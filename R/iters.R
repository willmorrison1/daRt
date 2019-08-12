#' @export
setMethod("iters", "SimulationFilter", function(x) x@iters)
#' @export
setMethod("iters<-", "SimulationFilter", function(x, value){
    x@iters <- value
    if (validObject(x, complete = TRUE)) return(x)
})
#' @export
setMethod("iters", "SimulationFiles", function(x) x@simulationFilter@iters)
