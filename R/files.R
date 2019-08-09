#' @export
setMethod(f = "files", signature = "SimulationFiles",
          definition = function(x){
              x@files$fileName
          }
)
