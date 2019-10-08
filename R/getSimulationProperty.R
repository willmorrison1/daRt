getSimulationProperty <- function(x, strSearch, allow_multiLines = FALSE) {

    simulationPropertiesFileNames <- .getSimulationPropertiesFileNames(x)

    outList <- lapply(simulationPropertiesFileNames,
                      function(x) .returnSimProperties(x, strSearch, allow_multiLines))

    names(outList) <- simname(x)
    return(outList)
}


.returnSimProperties <- function(simPropertyFileName, strSearch, allow_multiLines = FALSE) {

    rawLines <- readLines(simPropertyFileName)
    splitLines <- strsplit(rawLines, ":")
    splitLines <- do.call(rbind, splitLines)
    strSeachBool <- grep(strSearch, splitLines[,1])
    strSearchResult <- splitLines[strSeachBool,]

    if (is.matrix(strSearchResult) & !allow_multiLines) {
        stop(paste0("Expected 1 result but got ", nrow(strSearchResult)))
    }

    return(strSearchResult)
}


.getSimulationPropertiesFileNames <- function(x) {

    file.path(baseDir(x), "output", "simulation.properties.txt")

}
