setMethod(f = "rb3D",
          signature = signature(x = "SimulationFiles"),
          definition = function(x){
              require(data.table)
              require(reshape2)

              RB3d <- as(object = x, Class = "RB3D",
                                   strict = TRUE)
              listData <- vector(mode = "list", length = nrow(RB3d@files))
              for (i in 1:nrow(RB3d@files)) {
                  fileRow <- RB3d@files[i, ]
                  RBdata <- .readBin3DRadiativeBudget(fileName = fileRow$fileName,
                                                       requiredVars = variablesRB3D(sF))
                  listData[[i]] <- reshape2::melt(data = RBdata)
                  rm(RBdata); gc()
                  colnames(listData[[i]]) <- c("X", "Y", "Z", "value", "variablesRB3D")
                  listData[[i]]$band <- fileRow$band
                  listData[[i]]$iter <- fileRow$iter
                  listData[[i]]$typeNum <- fileRow$typeNum
                  listData[[i]]$simName <- fileRow$simName
              }
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
