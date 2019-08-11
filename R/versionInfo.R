#' @export
setMethod(f = "versionInfo", signature = signature(x = "character"),
          definition = function(x){

              require(xml2)
              require(dplyr)
              OUTlist <- vector("list", length(x))
              for (i in 1:length(x)) {
                  xmlFile <- file.path(x[i], "input", "directions.xml")
                  xmlData <- xml2::read_xml(xmlFile)
                  versionNumber <- xml2::xml_attr(xmlData, attr = "version")
                  buildNumber <- xml2::xml_attr(xmlData, attr = "build")
                  OUTlist[[i]] <- data.frame("version" = versionNumber, "build" = buildNumber)
              }
              OUT <- unique(dplyr::bind_rows(OUTlist))
              return(OUT)
          }
)

#' @export
setMethod(f = "versionInfo", signature = signature(x = "SimulationHandle"),
          definition = function(x){
              OUT <- versionInfo(simdir(x))
              return(OUT)
          }
)
