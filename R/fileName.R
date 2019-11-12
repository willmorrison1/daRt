#' @export
setMethod(f = "fileName", signature = "SimulationFiles",
          definition = function(x){
              x@files$fileName
          }
)
