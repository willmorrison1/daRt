setMethod(f = "rb3D",
          signature = signature(x = "SimulationFiles"),
          definition = function(x, nCores = 1){
              require(data.table)
              require(reshape2)
              require(foreach)
              require(parallel)
              require(doParallel)

              RB3d <- as(object = x, Class = "RB3D",
                         strict = TRUE)
              cl <- parallel::makeCluster(nCores)
              doParallel::registerDoParallel(cl)
              listData <- foreach(i = 1:nrow(RB3d@files),
                                  .export = ".readBin3DRadiativeBudget") %dopar% {

                                      fileRow <- RB3d@files[i, ]
                                      sF <- RB3d@simulationFilter
                                      RBdata <- .readBin3DRadiativeBudget(fileName = fileRow$fileName,
                                                                          requiredVars = variablesRB3D(sF))
                                      RBdata_melted <- reshape2::melt(data = RBdata)
                                      rm(RBdata); gc()
                                      colnames(RBdata_melted) <- c("X", "Y", "Z", "value", "variablesRB3D")
                                      RBdata_melted$band <- fileRow$band
                                      RBdata_melted$iter <- fileRow$iter
                                      RBdata_melted$typeNum <- fileRow$typeNum
                                      RBdata_melted$simName <- fileRow$simName
                                      return(RBdata_melted)
                                  }
              stopCluster(cl)
              gc()
              RB3d@data <- data.table::rbindlist(listData, use.names = FALSE)
              rm(listData); gc()
              validObject(RB3d)
              return(RB3d)
          }
)


.readBin3DRadiativeBudget <- function(fileName, requiredVars = NULL){

    RBfileDetails <- .parse3DRBfileName(basename(fileName))
    nCells <- RBfileDetails$nCells
    radiativeBudgetNames3D <- RBfileDetails$variables
    RAW <- readBin(fileName, numeric(), endian = "little", n = prod(nCells) * length(radiativeBudgetNames3D))
    RAWarray <- array(RAW, dim = c(nCells[2], nCells[1], nCells[3] * length(radiativeBudgetNames3D)))
    #split the array up into each radiative budget product
    thirdArrayDimIndex <- seq(1, nCells[3] * length(radiativeBudgetNames3D), by = nCells[3])
    #return each radiative budget array, by type, to a named list of arrays
    RAWarrayLists <- lapply(thirdArrayDimIndex, function(x) RAWarray[, , (nCells[3] + x - 1):x])
    names(RAWarrayLists) <- radiativeBudgetNames3D
    if (!is.null(requiredVars)) {
        requiredVarsInd <- names(RAWarrayLists) %in% requiredVars
        if (all(requiredVarsInd == FALSE)) {
            stop(paste0("The'required' RB3D vars are: ", paste0(requiredVars, collapse = ","), ". ",
                        "But the available RB3D vars are: ", paste0(names(RAWarrayLists), collapse = ","),
                        " in file: ", fileName))
        }
        RAWarrayLists <- RAWarrayLists[names(RAWarrayLists) %in% requiredVars]
    }
    RAWarrayListsAperm <- lapply(RAWarrayLists, function(x) aperm(x, c(2, 1, 3)))
    return(RAWarrayListsAperm)

}
