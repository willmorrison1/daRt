#' as.data.frame
#'
#' @param x SimulationData.
#'
#' @return
#' @export
#'
#' @examples
setMethod(f = "as.data.frame",
          signature = signature(x = "SimulationData"),
          definition = function(x, as.tibble = TRUE){

              DF <- x@data
              if (as.tibble) {
                  require(tibble)
                  require(dplyr)
                  DF <- DF %>%
                      dplyr::group_by(band, iter, typeNum, simName)
              }
              return(DF)
          }
)
