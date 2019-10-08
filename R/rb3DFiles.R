setMethod(f = "rb3DFiles",
          signature = signature(x = "character", sF = "SimulationFilter"),
          definition = function(x, sF){

              require(tools)

              simHandle <- simulationHandle(x)
              RB3DFiles <- as(object = simHandle, Class = "SimulationFiles")
              if (variables(sF) != "RADIATIVE_BUDGET") {
                  warning("Forcing 'RADIATIVE_BUDGET' variable in 'simulationFilter' variables.")
                  variables(sF) <- "RADIATIVE_BUDGET"
              }
              simulationFilter(RB3DFiles) <- sF
              #RB3D 'typeNums' appear under typeNums = "" only
              sFnoTypeNums <- simulationFilter(x = RB3DFiles); typeNums(sFnoTypeNums) <- ""
              subDirs <- subDir(sFnoTypeNums)
              fileDir <- file.path(baseDir(RB3DFiles), subDirs$dirName)
              expectedFiles <- prod(nrow(subDirs), length(typeNums(sF)))
              OUT <- vector("list", length = expectedFiles)
              RBTypeNums <- .RB3DtypeNums(typeNums(sF))
              nProcessed <- 0
              typeNumsFound <- character()
              variablesRB3DFound <- character()
              for (i in 1:nrow(subDirs)) {
                  allFiles <- list.files(fileDir[i], pattern = ".bin|.nc", full.names = TRUE)
                  all3DrbFiles <- allFiles[grepl("3D_", basename(allFiles))]
                  all3DrbFiles_duplicated <- duplicated(tools::file_path_sans_ext(all3DrbFiles))
                  if (any(all3DrbFiles_duplicated)) {
                      warning(paste("Found duplicate rb3D files in:", subDirs$dirName[i], "(different extensions).
                                    Cleaning duplicates. Run rb3DtoNcdf() if warning persists."))
                  }
                  all3DrbFiles <- all3DrbFiles[!all3DrbFiles_duplicated]
                  file.remove(all3DrbFiles[all3DrbFiles_duplicated])
                  if (length(all3DrbFiles) == 0) {
                      stop(paste("No RB files in:", fileDir[i]))
                  }
                  for (j in 1:length(RBTypeNums)) {
                      for (v in 1:length(all3DrbFiles)) {
                          RB3Dinfo <- .parse3DRBfileName(all3DrbFiles[v])
                          varsInFile <- unique(variablesRB3D(sF) %in% RB3Dinfo$variables)
                          typeNumsFound <- unique(c(typeNumsFound, RB3Dinfo$typeNum))
                          variablesRB3DFound <- unique(c(variablesRB3DFound,  RB3Dinfo$variables))
                          if (RB3Dinfo$typeNum == RBTypeNums[j] & any(varsInFile)) {
                              nProcessed <- nProcessed + 1
                              OUT[[nProcessed]] <- data.frame("band" = subDirs[i, ]$band,
                                                              "variable" = subDirs[i, ]$variable,
                                                              "iter" = subDirs[i, ]$iter,
                                                              "typeNum" = typeNums(sF)[j],
                                                              "fileName" = all3DrbFiles[v],
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
                  stop(paste0("RB3DFiles() couldn't find all expected RB3Dvariables for the given simFilter",
                              "variablesRB3D found: '", paste0(variablesRB3DFound, collapse = ","), "'
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
    if (!fileExt %in% c("bin", "nc")) .isNot3DRBstop(RB3DfileName)
    rawSplit <- strsplit(RB3DfileName, "_")[[1]]
    if (rawSplit[1] != "3D") .isNot3DRBstop(RB3DfileName)
    nCells <- as.numeric(c(rawSplit[2], rawSplit[3], rawSplit[4]))
    if (any(is.na(nCells))) .isNot3DRBstop(RB3DfileName)
    if (isTypeNum) {
        variables <- rawSplit[-c(1:4, length(rawSplit))]
        typeNumRaw <- gsub(".bin|.nc", "", rawSplit[length(rawSplit)])
        typeNum <- gsub("TypeNum=", "", typeNumRaw)
    } else {
        variables <- rawSplit[-c(1:4)]
        variables[length(variables)] <- gsub(".bin|.nc", "", variables[length(variables)])
        typeNum <- ""
    }
    OUT <- list("nCells" = nCells,
                "variables" = variables,
                "typeNum" = typeNum,
                "fileExtension" = fileExt)

    return(OUT)
}

.isNot3DRBstop <- function(RB3DfileName) {

    stop(paste(RB3DfileName, "is not a rb3D file"))

}
