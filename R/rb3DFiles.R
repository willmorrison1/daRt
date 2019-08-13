setMethod(f = "rb3DFiles",
          signature = signature(x = "character", sF = "SimulationFilter"),
          definition = function(x, sF){

              RB3DFiles <- new("SimulationFiles",
                                simDir = x,
                                isSequence = isSequence(x),
                                sequenceInfo = sequenceInfo(x))
              RB3DFiles@simulationFilter <- sF
              if (variables(sF) != "RADIATIVE_BUDGET") {
                  warning("Forcing 'RADIATIVE_BUDGET' variable in 'simulationFilter' variables.")
                  variables(sF) <- "RADIATIVE_BUDGET"
              }
              sFnoTypeNums <- sF; typeNums(sFnoTypeNums) <- "" #RB3D 'typeNums' appear under typeNums = "" only
              subDirs <- simdir(sFnoTypeNums)
              fileDir <- file.path(simdir(RB3DFiles), subDirs$dirName)
              expectedFiles <- prod(nrow(subDirs), length(typeNums(sF)))
              OUT <- vector("list", length = expectedFiles)
              RBTypeNums <- .RB3DtypeNums(typeNums(sF))
              nProcessed <- 0
              typeNumsFound <- character()
              variablesRB3DFound <- character()
              for (i in 1:nrow(subDirs)) {
                  allBinFiles <- list.files(fileDir[i], pattern = ".bin", full.names = TRUE)
                  allFiles <- allBinFiles[grepl("3D_", basename(allBinFiles))]
                  nonTypeNumFile <- allFiles[!grepl("TypeNum|Ground", basename(allFiles))]
                  if (length(nonTypeNumFile) != 1) {
                      stop(paste( "No radiative budget files found. RB3DFiles() expected at least one radiative 
                                  budget .bin file (the non-'typeNum' file) in:", fileDir[i]))
                  }
                  for (j in 1:length(RBTypeNums)) {
                      for (v in 1:length(allFiles)) {
                          RB3Dinfo <- .parse3DRBfileName(allFiles[v])
                          varsInFile <- unique(variablesRB3D(sF) %in% RB3Dinfo$variables)
                          typeNumsFound <- unique(c(typeNumsFound, RB3Dinfo$typeNum))
                          variablesRB3DFound <- unique(c(variablesRB3DFound,  RB3Dinfo$variables))
                          if (RB3Dinfo$typeNum == RBTypeNums[j] & any(varsInFile)) {
                              nProcessed <- nProcessed + 1
                              OUT[[nProcessed]] <- data.frame("band" = subDirs[i,]$band,
                                                              "variable" = subDirs[i,]$variable,
                                                              "iter" = subDirs[i,]$iter,
                                                              "typeNum" = typeNums(sF)[j],
                                                              "fileName" = allFiles[v],
                                                              stringsAsFactors = FALSE)
                              break
                          }
                      }
                  }
              }
              RB3DFiles@files <-  dplyr::bind_rows(OUT)
              if (nProcessed != expectedFiles) {
                  stop(paste0("RB3DFiles() couldn't find all expected files for the given simFilter.\n",
                              "TypeNums found     : '", paste0(typeNumsFound, collapse = ","), "'\n",
                              "variablesRB3D found: '", paste0(variablesRB3DFound, collapse = ","), "'\n
                             Use this information to adjust your 'simulationFilter'"))
              }
              varRB3DnotFound <- !variablesRB3D(sF) %in% variablesRB3DFound
              if (any(varRB3DnotFound)) {
                  stop(paste0("RB3DFiles() couldn't find all expected RB3Dvariables for the given simFilter.\n",
                              "variablesRB3D found: '", paste0(variablesRB3DFound, collapse = ","), "'\n
                             Use this information to adjust your 'simulationFilter'"))
              }
              return(RB3DFiles)
          })

.RB3DtypeNums <- function(typeNumVals){
    for (i in 1:length(typeNumVals)) {
        if (typeNumVals[i] == "") next
        typeNumSplit <- strsplit(typeNumVals[i], "_")[[1]]
        if (typeNumSplit[2] == "Ground") {
            typeNumVals[i] <- "Ground"
        } else {
            typeNumVals[i] <- typeNumSplit[1]
        }
    }
    return(typeNumVals)
}

.parse3DRBfileName <- function(RB3DfileName){
    require(tools)
    RB3DfileName <- basename(RB3DfileName)
    if (grepl("_TypeNum=|Ground", RB3DfileName)) {
        isTypeNum <- TRUE
    }else{
        isTypeNum <- FALSE
    }
    fileExt <- tools::file_ext(RB3DfileName)
    if (fileExt != "bin") .isNot3DRBstop(RB3DfileName)
    rawSplit <- strsplit(RB3DfileName, "_")[[1]]
    if (rawSplit[1] != "3D") .isNot3DRBstop(RB3DfileName)
    nCells <- as.numeric(c(rawSplit[2], rawSplit[3], rawSplit[4]))
    if (any(is.na(nCells))) .isNot3DRBstop(RB3DfileName)
    if (isTypeNum) {
        variables <- rawSplit[-c(1:4, length(rawSplit))]
        typeNumRaw <- gsub(".bin", "", rawSplit[length(rawSplit)])
        typeNum <- gsub("TypeNum=", "", typeNumRaw)
    } else {
        variables <- rawSplit[-c(1:4)]
        variables[length(variables)] <- gsub(".bin", "", variables[length(variables)])
        typeNum <- ""
    }
    OUT <- list("nCells" = nCells,
                "variables" = variables,
                "typeNum" = typeNum)

    return(OUT)
}
