setMethod(f = "rb3D",
          signature = signature(x = "SimulationFiles"),
          definition = function(x, nCores = 1){

              RB3d <- as(object = x, Class = "RB3D",
                         strict = TRUE)
              if (nrow(RB3d@files) < nCores) nCores <- nrow(RB3d@files)
              if (nCores > 1) {
                  cl <- parallel::makeCluster(nCores)
                  doParallel::registerDoParallel(cl)
              } else{
                  registerDoSEQ()
              }
              listData <- foreach(i = 1:nrow(RB3d@files),
                                  .packages = "bitops",
                                  .export = c(".readBin3DRadiativeBudget",
                                              ".parse3DRBfileName",
                                              ".readNcdf3DRadiativeBudget",
                                              ".read3DRadiativeBudget")) %dopar% {

                                                  fileRow <- RB3d@files[i, ]
                                                  sF <- RB3d@simulationFilter
                                                  RBdata <- .read3DRadiativeBudget(fileName = fileRow$fileName,
                                                                                   requiredVars = variablesRB3D(sF))
                                                  RBdata_melted <- reshape2::melt(data = RBdata)
                                                  rm(RBdata); gc()
                                                  colnames(RBdata_melted) <- c("X", "Y", "Z", "value", "variableRB3D")
                                                  RBdata_melted$band <- fileRow$band
                                                  RBdata_melted$iter <- fileRow$iter
                                                  RBdata_melted$typeNum <- fileRow$typeNum
                                                  RBdata_melted$simName <- fileRow$simName
                                                  return(RBdata_melted)
                                              }

              if (nCores > 1) stopCluster(cl)
              gc()
              RB3d@data <- data.table::rbindlist(listData, use.names = FALSE)
              rm(listData); gc()
              validObject(RB3d)

              return(RB3d)
          }
)


.read3DRadiativeBudget <- function(fileName, requiredVars = NULL){

    rbFileNameDetails <- .parse3DRBfileName(fileName)
    if (rbFileNameDetails$fileExtension == "bin") {
        RBdata <- .readBin3DRadiativeBudget(fileName = fileName,
                                            requiredVars = requiredVars)
    } else {
        RBdata <- .readNcdf3DRadiativeBudget(fileName = fileName,
                                             requiredVars = requiredVars)
    }
    return(RBdata)
}


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

.readNcdf3DRadiativeBudget <- function(fileName, requiredVars = NULL){

    require(ncdf4)

    if (length(fileName) != 1) {
        stop(".readNcdf3DRadiativeBudget() Expected one file")
    }

    ncin  <- ncdf4::nc_open(fileName)
    vars <- names(ncin$var)
    OUTDATA <- vector("list", length(vars))
    names(OUTDATA) <- .formatRB3DVarsForNcdf(vars, fromNc = TRUE)

    if (!is.null(requiredVars)) {
        requiredVarsInd <- names(OUTDATA) %in% requiredVars
        if (all(requiredVarsInd == FALSE)) {
            stop(paste0("The'required' RB3D vars are: ", paste0(requiredVars, collapse = ","), ". ",
                        "But the available RB3D vars are: ", paste0(names(OUTDATA), collapse = ","),
                        " in file: ", fileName))
        }
        OUTDATA <- OUTDATA[names(OUTDATA) %in% requiredVars]
    }

    for (i in 1:length(OUTDATA)) {
        varRawVals <- ncdf4::ncvar_get(ncin, .formatRB3DVarsForNcdf(names(OUTDATA)[i], fromNc = FALSE))
        varRawVals <- aperm(varRawVals, c(2, 1, 3))
        OUTDATA[[i]] <- varRawVals
    }

    ncdf4::nc_close(ncin)

    return(OUTDATA)
}
