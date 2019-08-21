#' @export
setMethod(f = "versionInfo", signature = signature(x = "character"),
          definition = function(x){

              require(xml2)
              require(dplyr)
              if (any(!dir.exists(x))) return(data.frame())
              OUTlist <- vector("list", length(x))
              for (i in 1:length(x)) {
                  xmlFile <- file.path(x[i], "input", c("maket.xml",
                                                        "phase.xml", "directions.xml",
                                                        "coeff_diff.xml"))
                  existingXmlFile <- which(file.exists(xmlFile))[1]
                  if (is.na(existingXmlFile)) return(data.frame())
                  xmlData <- xml2::read_xml(xmlFile[existingXmlFile])
                  versionNumber <- xml2::xml_attr(xmlData, attr = "version")
                  buildFull <- xml2::xml_attr(xmlData, attr = "build")
                  buildNumber <- as.numeric(gsub("v", "", xml2::xml_attr(xmlData, attr = "build")))
                  if (is.na(buildNumber)) stop("Problem parsing DART simulation version info")
                  OUTlist[[i]] <- data.frame("version" = versionNumber,
                                             "buildFull" = buildFull,
                                             "buildNumber" = buildNumber,
                                             stringsAsFactors = FALSE)
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
