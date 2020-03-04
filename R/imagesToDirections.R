#' @export
setMethod("imagesToDirectionsDF", signature = signature(x = "Images", fun = "function"),
          definition = function(x, fun = function(x) mean(x[x != -1])){

              if (any(x@data$imgType != "ima")) {
                  stop("imagesToDirections() only valid with the orthograpic 'ima' image type")
              }

              aggData <- x@data %>%
                  dplyr::group_by(band, variable, iter, typeNum, imageNum, VZ, VA, simName) %>%
                  dplyr::summarise(value = fun(value)) %>%
                  ungroup() %>%
                  as.data.frame()

              aggData$azimuth <- aggData$VA
              aggData$zenith <- aggData$VZ

              return(aggData)
          }
)
