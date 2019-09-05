# how to get x - for below
# library(daRt)
# setwd("V:/Tier_processing/hv867657/DART_5-7-5_1126/user_data/simulations/SPARTACUS_SW/daRtinput/2019241_233930/")
# simDir <- c("sequence1o_0","sequence1o_1")
# sF <- simulationFilter(product = "directions", iters = "ITERX")
# simFiles <- daRt::getFiles(x = simDir, sF = sF)
# x <- daRt::getData(simFiles)
# resourceUse(x)
#remove these comments when you have finished debugging
#

#' @export
setMethod("resourceUse", "SimulationFiles",
          function(x){
    require(dplyr)
    dartTxtFiles <- getDartTxtFileName(x)
    outList <- vector("list", length = length(dartTxtFiles))
    simNames <- simname(x)
    for (i in 1:length(dartTxtFiles)) {
        rawFileDATA <- readLines(dartTxtFiles[i], warn = FALSE)
        timeTaken <- searchDartTxtVal(rawFileDATA, searchQuote = "Processing time")
        timeTaken <- as.ITime(as.POSIXct(timeTaken, format = '%H %M %S'))
        memUsed <- as.numeric(memUsed)
        memUsed <- searchDartTxtVal(rawFileDATA, searchQuote = "Memory usage")
        #KB: format "timeTaken" as POSIXct
        outList[[i]] <- data.frame("simName" = simNames[i],
                                   "timeTaken" = timeTaken,
                                   "memUsage" = memUsed,
                                   stringsAsFactors = FALSE)
    }
    outDF <- dplyr::bind_rows(outList)
    return(outDF)
})

searchDartTxtVal <- function(rawFileData, searchQuote) {
    rawVal <- strsplit(rawFileData[grepl(searchQuote, rawFileData)], "[*]")
    if (length(rawVal) == 0) {
        rawVal <- NA
        warning(paste("Could not get '",searchQuote, "' info from dart.txt"))
    } else {
        rawVal <- rawVal[[1]][3]
    }
    return(rawVal)
}



