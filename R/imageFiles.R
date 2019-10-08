#' @export
setMethod(f = "imageFiles",
          signature = signature(x = "character", sF = "SimulationFilter"),
          definition = function(x, sF){

              require(tools)
              require(dplyr)

              simHandle <- simulationHandle(x)
              imageFiles <- as(object = simHandle, Class = "SimulationFiles")
              #TODOmake "attachSimulationFilter" method - whereby i can e.g. make sure the sF bands are updated accordingly
              #e.g. if sF has integer() bands i need to add the .getWavelengthsDF(SimulationFiles)$band
              #function to get all bands.
              imageFiles@simulationFilter <- sF
              #TODO - why does simdir do vastly different things for sF and SimulationFiles??
              #simdir should be removed (to make sure it's not used stupidly) and replaced with subDirectories() and the
              #SimulationFiles should use the @sF slot, so it basically uses the same thing. Then baseDirectories() is for the simulation dir
              #for the SimulationFiles type object.
              subDirs <- simdir(sF)
              subDirs$dirName <- file.path(subDirs$dirName, "IMAGES_DART")
              imgTypeDF <- .parseimageTypes(sF)
              imgInfoDFList <- vector(mode = "list", length = nrow(subDirs) * nrow(imgTypeDF))
              iterTrack <- 1
              for (v in 1:nrow(imgTypeDF)) {
                  for (i in 1:nrow(subDirs)) {
                      if (imgTypeDF$isTransmittance[v]) {
                          subDirFull <- file.path(simdir(imageFiles), subDirs$dirName[i],
                                                  "Transmittance")
                      } else {
                          subDirFull <- file.path(simdir(imageFiles), subDirs$dirName[i])
                      }
                      if (!dir.exists(subDirFull)) {
                          stop(paste("No image files in", subDirs$dirName[i]))
                      }
                      allImagesFull <- list.files(subDirFull, pattern = ".mpr", full.names = TRUE)
                      imgInfoDFList[[iterTrack]] <- dplyr::bind_rows(lapply(allImagesFull, .imgInfo))
                      imgInfoDFList[[iterTrack]] <- imgInfoDFList[[iterTrack]] %>%
                          dplyr::filter(grepl(imgTypeDF$imageTypes[v], imgType))
                      if (length(sF@imageNos) != 0) {
                          imgInfoDFList[[iterTrack]] <- imgInfoDFList[[iterTrack]] %>%
                              dplyr::filter(imageNos %in% sF@imageNos)
                      }
                      if (nrow(imgInfoDFList[[iterTrack]]) == 0) {
                          stop("No images found after 'imageNos' filter applied")
                      }
                      imgInfoDFList[[iterTrack]]$band <- subDirs$band[i]
                      imgInfoDFList[[iterTrack]]$variable <- subDirs$variable[i]
                      imgInfoDFList[[iterTrack]]$iter <- subDirs$iter[i]
                      imgInfoDFList[[iterTrack]]$typeNum <- subDirs$typeNum[i]
                      imgInfoDFList[[iterTrack]]$transmittance <- imgTypeDF$isTransmittance[v]
                      iterTrack <- iterTrack + 1
                  }
              }
              imgInfoDF <- dplyr::bind_rows(imgInfoDFList)
              imageFiles@files <- imgInfoDF
              validObject(imageFiles)

              return(imageFiles)

          })

.imgInfo <- function(imageFile) {

    imageFileName <- tools::file_path_sans_ext(basename(imageFile))
    imageFileSplit <- strsplit(imageFileName, "_")[[1]]
    imgInfoDF <- data.frame("imgType" = character(1),
                            "imageNos" = numeric(1),
                            "VZ" = numeric(1),
                            "VA" = numeric(1),
                            "fileName" = character(1))
    if (grepl("ima", imageFileSplit[1])) {
        imgInfoDF$imageNos <- as.numeric(gsub("ima", "", imageFileSplit[1]))
        imgInfoDF$imgType <- "ima"
        imgInfoDF$VZ <- as.numeric(paste0(gsub("VZ=", "", imageFileSplit[2]), ".", imageFileSplit[3]))
        imgInfoDF$VA <- as.numeric(paste0(gsub("VA=", "", imageFileSplit[4]), ".", imageFileSplit[5]))
    } else if (grepl("camera", imageFileSplit[1])) {
        imgInfoDF$imageNos <- as.numeric(imageFileSplit[2])
        imgInfoDF$imgType <- "camera"
        imgInfoDF$VZ <- as.numeric(paste0(gsub("VZ=", "", imageFileSplit[3]), ".", imageFileSplit[4]))
        imgInfoDF$VA <- as.numeric(paste0(gsub("VA=", "", imageFileSplit[5]), ".", imageFileSplit[6]))
    } else {
        stop(paste("Unknown image filename (does not start with 'ima' or 'camera'):",
                   imageFileName))
    }
    imgInfoDF$fileName <- imageFile

    return(imgInfoDF)

}

.parseimageTypes <- function(object) {

    imageTypesRaw <- imageTypes(object)
    imageTypesSplit <- strsplit(imageTypesRaw, "_")
    isTransmittance <- sapply(imageTypesSplit, function(x) x[length(x)] == "transmittance")
    imageTypesVal <- sapply(imageTypesSplit, function(x) x[1])
    outDF <- data.frame(imageTypes = imageTypesVal, isTransmittance = isTransmittance,
                        stringsAsFactors = FALSE)

    return(outDF)

}
