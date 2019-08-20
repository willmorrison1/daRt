#' imagesToDirections
#' @description Convert an {Images-class} object to a {Directions-class} object
#'
#' @param \link{Images-class} object
#' @param Function to apply across each image. Defaults to \code{function(x) mean(x[x != -1])
#'
#' @return
#' @export
#'
#' @examples
setMethod("imagesToDirections", signature = signature(x = "Images", fun = "function"),
          definition = function(x, fun = function(x) mean(x[x != -1])){

              require(dplyr)
              if (any(x@data$imgType != "ima")) {
                  stop("imagesToDirections() only valid with the orthograpic 'ima' image type")
              }

              aggData <- x@data %>%
                  dplyr::group_by(band, variable, iter, typeNum, imageNo, VZ, VA, simName) %>%
                  dplyr::summarise(value = fun(value)) %>%
                  ungroup() %>%
                  as.data.frame()

              outObj <- as(object = x, Class = "Directions")
              outObj@data <- aggData
              outObj@data$azimuth <- outObj@data$VA
              outObj@data$zenith <- outObj@data$VZ

              return(outObj)
          }
)
