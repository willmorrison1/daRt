#' as.data.frame
#'
#' @param x SimulationData.
#' @param as.tibble Return as a tibble-type data frame?
#'
#' @return data.frame or tibble
#' @export
setMethod(f = "as.data.frame",
          signature = signature(x = "SimulationData"),
          definition = function(x, as.tibble = TRUE){

              DF <- x@data
              if (as.tibble) {
                  DF <- DF %>%
                      dplyr::group_by(band, iter, typeNum, simName)
              }
              return(DF)
          }
)
