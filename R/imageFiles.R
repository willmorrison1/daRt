#' @export
setMethod(f = "imageFiles",
          signature = signature(x = "character", sF = "SimulationFilter"),
          definition = function(x, sF){

              require(tools)
              require(dplyr)
              imageFiles <- new("SimulationFiles",
                                simDir = x,
                                isSequence = isSequence(x),
                                sequenceInfo = sequenceInfo(x))
              imageFiles@simulationFilter <- sF
              subDirs <- simdir(sF)
              subDirs$dirName <- file.path(subDirs$dirName, "IMAGES_DART")
              imgInfoDFList <- vector(mode = "list", length = nrow(subDirs))
              for (i in 1:nrow(subDirs)) {
                  subDirFull <- file.path(simdir(imageFiles), subDirs$dirName[i])
                  if (!dir.exists(subDirFull)) {
                      stop(paste("No image files in", subDirs$dirName[i]))
                  }
                  allImagesFull <- list.files(subDirFull, pattern = ".mpr", full.names = TRUE)
                  imgInfoDFList[[i]] <- dplyr::bind_rows(lapply(allImagesFull, .imgInfo))
                  imgInfoDFList[[i]] <- imgInfoDFList[[i]] %>%
                      dplyr::filter(grepl(paste(imageType(sF), collapse = "|"), imgType))
                  if (length(sF@imageNo) != 0) {
                      imgInfoDFList[[i]] <- imgInfoDFList[[i]] %>%
                          dplyr::filter(imageNo %in% sF@imageNo)
                  }
                  imgInfoDFList[[i]]$band <- subDirs$band[i]
                  imgInfoDFList[[i]]$variable <- subDirs$variable[i]
                  imgInfoDFList[[i]]$iter <- subDirs$iter[i]
                  imgInfoDFList[[i]]$typeNum <- subDirs$typeNum[i]
              }
              imgInfoDF <- dplyr::bind_rows(imgInfoDFList)
              imageFiles@files <- imgInfoDF
              validObject(imageFiles)
              return(imageFiles)

          })

.imgInfo <- function(imageFile) {
    if (!file.exists(imageFile)) {
        stop(".imgInfo() file doesn't exist - needs full path")
    }

    imageFileName <- tools::file_path_sans_ext(basename(imageFile))
    imageFileSplit <- strsplit(imageFileName, "_")[[1]]
    imgInfoDF <- data.frame("imgType" = character(1),
                            "imageNo" = numeric(1),
                            "VZ" = numeric(1),
                            "VA" = numeric(1),
                            "fileName" = character(1))
    if (grepl("ima", imageFileSplit[1])) {
        imgInfoDF$imageNo <- as.numeric(gsub("ima", "", imageFileSplit[1]))
        imgInfoDF$imgType <- "ima"
        imgInfoDF$VZ <- as.numeric(paste0(gsub("VZ=", "", imageFileSplit[2]), ".", imageFileSplit[3]))
        imgInfoDF$VA <- as.numeric(paste0(gsub("VA=", "", imageFileSplit[4]), ".", imageFileSplit[5]))
    } else if (grepl("camera", imageFileSplit[1])) {
        imgInfoDF$imageNo <- as.numeric(imageFileSplit[2])
        imgInfoDF$imgType <- "camera"
        imgInfoDF$VZ <- as.numeric(paste0(gsub("VZ=", "", imageFileSplit[3]), ".", imageFileSplit[4]))
        imgInfoDF$VA <- as.numeric(paste0(gsub("VA=", "", imageFileSplit[5]), ".", imageFileSplit[6]))
    } else {
        stop("Unknown image file type (was not 'ima' or 'camera'")
    }
    imgInfoDF$fileName <- imageFile
    return(imgInfoDF)

}
