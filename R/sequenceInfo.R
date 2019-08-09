sequencerFilename <- function(simDir){

    file.path(simDir, "output", "dart.sequenceur.properties")

}

isSequence <- function(simDir){
    sequenceFiles <- sequencerFilename(simDir)
    sequenceFilesExists <- sapply(sequenceFiles, function(x) file.exists(x))
    for (i in seq_along(sequenceFiles[sequenceFilesExists])) {
        firstLine <- readLines(sequenceFiles[sequenceFilesExists][i])[1]
        if (grepl(pattern = "Fichier", x = firstLine)) {
            sequenceFilesExists[sequenceFilesExists][i] <- FALSE
        }else{
            sequenceFilesExists[sequenceFilesExists][i] <- TRUE
        }
    }
    return(sequenceFilesExists)
}

sequenceInfo <- function(simDir){
    isSeq <- isSequence(simDir)

    if (!isSeq) {
        return(data.frame())
    }

    require(dplyr)
    sequencePropertiesFileName <- sequencerFilename(simDir)
    rawLines <- readLines(sequencePropertiesFileName)[-1]
    rawSplit_names <- strsplit(rawLines[seq(1, length(rawLines) - 1, by = 2)], "[.]")
    rawSplit_vals <- strsplit(rawLines[seq(2, length(rawLines), by = 2)], "[:]")
    parameterNamesList <- vector(mode = "list", length = length(rawSplit_names))
    parameterNo <- sapply(rawSplit_names, function(x) x[2])
    for (i in 1:length(parameterNamesList)) {
        parameterBaseName <- strsplit(rawSplit_names[[i]][3], ":")[[1]][2]
        parameterOtherNames <- paste0(rawSplit_names[[i]][4:length(rawSplit_names[[i]])],
                                      collapse = "_")
        parameterNamesList[[i]]$parameterFullName <-
            paste(parameterBaseName, parameterOtherNames,  sep = "_")
        parameterNamesList[[i]]$parameterNo <- parameterNo[i]
    }
    parameterNamesDF <- dplyr::bind_rows(parameterNamesList)
    parameterValsList <- vector(mode = "list", length = length(rawSplit_vals))
    for (i in 1:length(rawSplit_vals)) {
        splittedVals <- strsplit(rawSplit_vals[[i]], ":")
        parameterValsList[[i]]$parameterNo <- strsplit(splittedVals[[1]], "[.]")[[1]][2]
        parameterValsList[[i]]$parameterVal <- splittedVals[[2]]
    }
    parameterValsDF <- dplyr::bind_rows(parameterValsList)
    parametersOUT <- dplyr::full_join(parameterNamesDF, parameterValsDF,
                                      by = "parameterNo") %>%
        dplyr::arrange(parameterNo, parameterFullName, parameterVal)

    return(parametersOUT)
}
