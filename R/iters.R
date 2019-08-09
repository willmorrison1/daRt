#' @export
setMethod("iters", "SimulationFilter", function(x) x@iters)
#' @export
setMethod("iters<-", "SimulationFilter", function(x, value){
    x@iters <- value
    if (validObject(x, complete = TRUE)) return(x)
})
