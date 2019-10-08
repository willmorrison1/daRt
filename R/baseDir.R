#' @export
setMethod(f = "baseDir", signature = "SimulationHandle",
          definition = function(x){
              x@baseDir
          }
)
