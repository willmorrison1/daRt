#' @export
setMethod("product", "SimulationFilter", function(x) x@product)
#' @export
setMethod("product<-", "SimulationFilter", function(x, value){
    x@product <- value
    if (validObject(x, complete = TRUE)) return(x)
})
#' @export
setMethod("product", "SimulationFiles", function(x) x@simulationFilter@product)
