getSimulationProperty <- function(x, strSearch) {

    simulationPropertiesFileNames <- getSimulationPropertiesFileNames(x)

    outList <- lapply(simulationPropertiesFileNames, function(x) .returnSimProperties(x, strSearch))

    names(outList) <- simname(x)
    return(outList)
}


.returnSimProperties <- function(simProperttyFileName, strSearch) {

    rawLines <- readLines(simProperttyFileName)
    splitLines <- strsplit(rawLines, ":")
    splitLines <- do.call(rbind, splitLines)
    strSeachBool <- grep(strSearch, splitLines[,1])
    strSearchResult <- splitLines[strSeachBool,]

    if (is.matrix(strSearchResult)) {
        stop(paste0("Expected 1 result but got", nrow(strSearchResult)))
    }
    return(strSearchResult)
}


getSimulationPropertiesFileNames <- function(x) {

    file.path(simdir(x), "output", "simulation.properties.txt")

}

