.readILWIS <- function(fullDir){

    #add checks that this is numeric and txt file has been read correctly
    rSizeNumeric <- .getILWISsize(fullDir)
    toRead <- file(paste(fullDir, ".mp#", sep = ""), "rb")
    ReadFile <- readBin(toRead, double(), n = rSizeNumeric[1] * rSizeNumeric[2])
    close(toRead)
    rMatrix <- matrix(ReadFile, nrow = rSizeNumeric[1], byrow = TRUE)
    return(rMatrix)
}


.getILWISsize <- function(fullDir){

    rSize <- readLines(paste(fullDir, ".mpr", sep = ""))[17]
    rSizeNumeric <- as.numeric(strsplit(gsub("Size=", "", rSize), " ")[[1]])

    return(rSizeNumeric)
}


setMethod(f = "images",
          signature = signature(x = "SimulationFiles"),
          definition = function(x){

              require(tools)
              require(reshape2)
              require(data.table)
              imagesData <- as(object = x, Class = "Images",
                               strict = TRUE)
              imagesDataRaw <- vector(mode = "list", length = nrow(imagesData@files))
              filesWithoutExt <- tools::file_path_sans_ext(imagesData@files$fileName)
              for (i in 1:nrow(imagesData@files)) {
                  fileRow <- imagesData@files[i, ]
                  rawData <- .readILWIS(filesWithoutExt[i])
                  imagesDataRaw[[i]] <- reshape2::melt(rawData, varnames = c("x", "y"))
                  imagesDataRaw[[i]]$band <- fileRow$band
                  imagesDataRaw[[i]]$variable <- fileRow$variable
                  imagesDataRaw[[i]]$iter <- fileRow$iter
                  imagesDataRaw[[i]]$typeNum <- fileRow$typeNum
                  imagesDataRaw[[i]]$imgType <- fileRow$imgType
                  imagesDataRaw[[i]]$imageNo <- fileRow$imageNo
                  imagesDataRaw[[i]]$VZ <- fileRow$VZ
                  imagesDataRaw[[i]]$VA <- fileRow$VA
                  imagesDataRaw[[i]]$simName <- fileRow$simName
              }
              gc()
              imagesData@data <- data.table::rbindlist(imagesDataRaw, use.names = FALSE)

              return(imagesData)
          }
)
