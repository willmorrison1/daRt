#' @export
setMethod("typeNums", "SimulationFilter", function(x) x@typeNums)
#' @export
setMethod("typeNums<-", "SimulationFilter", function(x, value){
    x@typeNums <- value
    if (validObject(x, complete = TRUE)) return(x)
})
#' @export
setMethod("typeNums", "SimulationFiles", function(x) x@simulationFilter@typeNums)
