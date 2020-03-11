.readILWIS <- function(fullDir){

    #add checks that this is numeric and txt file has been read correctly
    rSizeNumeric <- .getILWISsize(fullDir)
    toRead <- file(paste(fullDir, ".mp#", sep = ""), "rb")
    ReadFile <- readBin(toRead, double(), n = rSizeNumeric[1] * rSizeNumeric[2])
    close(toRead)
    return(matrix(ReadFile, nrow = rSizeNumeric[1], byrow = TRUE))
}


.getILWISsize <- function(fullDir){

    rSize <- readLines(paste(fullDir, ".mpr", sep = ""), n = 17)[17]
    rSizeNumeric <- as.numeric(strsplit(gsub("Size=", "", rSize), " ")[[1]])

    return(rSizeNumeric)
}


setMethod(f = "images",
          signature = signature(x = "SimulationFiles"),
          definition = function(x, nCores = 1){

              imagesData <- as(object = x, Class = "Images",
                               strict = TRUE)
              filesWithoutExt <- tools::file_path_sans_ext(imagesData@files$fileName)
              if (nrow(imagesData@files) < nCores) nCores <- nrow(imagesData@files)
              if (nCores > 1) {
                  cl <- parallel::makeCluster(nCores)
                  doParallel::registerDoParallel(cl)
              }
              imagesDataRaw <- foreach(i = 1:nrow(imagesData@files), .export = c(".readILWIS", ".getILWISsize"),
                                       .packages = "reshape2") %dopar% {
                                           imgDat <- reshape2::melt(.readILWIS(filesWithoutExt[i]), varnames = c("x", "y"))
                                           nrowsData <- nrow(imgDat)
                                           fileRow <- imagesData@files[i, ]
                                           metaDF <- data.frame("band" = rep(fileRow$band, nrowsData),
                                                                "variable" = rep(fileRow$variable, nrowsData),
                                                                "iter" = rep(fileRow$iter, nrowsData),
                                                                "typeNum" = rep(fileRow$typeNum, nrowsData),
                                                                "imgType" = rep(fileRow$imgType, nrowsData),
                                                                "imageNum" = rep(fileRow$imageNum, nrowsData),
                                                                "VZ" = rep(fileRow$VZ, nrowsData),
                                                                "VA" = rep(fileRow$VA, nrowsData),
                                                                "simName" = rep(fileRow$simName, nrowsData),
                                                                "transmittance" = rep(fileRow$transmittance, nrowsData))
                                           imgDat <- cbind(imgDat, metaDF)
                                           return(imgDat)
                                       }
              gc()
              if (nCores > 1) stopCluster(cl)
              imagesData@data <- as.data.frame(data.table::rbindlist(imagesDataRaw, use.names = FALSE))

              return(imagesData)
          }
)
