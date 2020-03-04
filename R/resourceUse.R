#' @export
setMethod("resourceUse", "SimulationFiles",
          function(x){

              dartTxtFiles <- getDartTxtFileName(x)
              outList <- vector("list", length = length(dartTxtFiles))
              simNames <- simname(x)

              for (i in 1:length(dartTxtFiles)) {
                  rawFileDATA <- readLines(dartTxtFiles[i], warn = FALSE)
                  timeTaken <- searchDartTxtVal(rawFileDATA, searchQuote = "Processing time")
                  timeTakenSplit <- lapply(timeTaken, function(x){
                      as.numeric(strsplit(x, split = ' ')[[1]][-1])})
                  timeTakenSecs <- unlist(lapply(timeTakenSplit, function(x)
                      sum(x[1] * 60*60, x[2] * 60, x[3])))

                  memUsed <- searchDartTxtVal(rawFileDATA, searchQuote = "Memory usage")
                  memUsed <- as.numeric(memUsed)
                  outList[[i]] <- data.frame("simName" = simNames[i],
                                             "timeTaken" = timeTakenSecs,
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
