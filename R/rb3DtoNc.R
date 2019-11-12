setMethod(f = "rb3DtoNc",
          signature = signature(x = "SimulationFiles"),
          definition = function(x, ncCompressionFactor = 5){
              require(tools)
              if (product(x) != "rb3D") {
                  warning("rb3DtoNc needs rb3D product. Nothing to do.")
                  return(x)
              }
              #check which files are .bin
              #loop over bin files, else return all .nc with "no work to do"
              RB3DfileNames <- fileName(x)
              for (i in 1:length(RB3DfileNames)) {
                  RBfileNameInfo <- .parse3DRBfileName(RB3DfileNames[i])
                  if (RBfileNameInfo$fileExtension == "nc") next
                  binRBdata <- .readBin3DRadiativeBudget(RB3DfileNames[i])
                  oFile <- paste0(tools::file_path_sans_ext(RB3DfileNames[i]), ".nc")
                  .RB3DbinToNc(radiativeBudget3DData = binRBdata, outFile = oFile,
                               compressionFactor = ncCompressionFactor)
                  ncRBdata <- .readNcdf3DRadiativeBudget(fileName = oFile)
                  if (!all.equal(sapply(binRBdata, dim), sapply(ncRBdata, dim))) {
                      stop(paste("rb3DtoNc() reported that original .bin and created .nc files have different dims for:",
                                 RB3DfileNames[i], "and", oFile))
                  }
                  file.remove(RB3DfileNames[i])
                  x@files$fileName[i] <- oFile
              }
              return(x)
          }
)


.RB3DbinToNc <- function(radiativeBudget3DData,
                         outFile,
                         fillValue = -999,
                         compressionFactor = 4){

    require(ncdf4)
    #clean up old file, else sometimes won't write
    if (file.exists(outFile)) {
        file.remove(outFile)
    }

    OUT <- array(0, dim = c(dim(radiativeBudget3DData[[1]]), length(radiativeBudget3DData)))
    for (i in 1:length(radiativeBudget3DData)) {
        #set inf to NA
        radiativeBudget3DData[[i]][is.infinite(radiativeBudget3DData[[i]])] <- NA
        OUT[,,, i] <- radiativeBudget3DData[[i]]
    }
    OUT <- aperm(OUT, c(2, 1, 3, 4))
    dimnames(OUT)[[4]] <- .formatRB3DVarsForNcdf(names(radiativeBudget3DData), fromNc = FALSE)
    rm(radiativeBudget3DData); gc()

    xdim <- ncdim_def("x", '', as.integer(1:dim(OUT)[1]))
    ydim <- ncdim_def("y", '', as.integer(dim(OUT)[2]:1))
    zdim <- ncdim_def("z", '', as.integer(1:dim(OUT)[3]))

    varListOUT <- vector("list", length = dim(OUT)[4])
    for (i in 1:dim(OUT)[4]) {
        #define variables
        varListOUT[[i]] <- ncdf4::ncvar_def(name = dimnames(OUT)[[4]][i], units = "unknown",
                                            dim = list(xdim, ydim, zdim),
                                            missval = fillValue, longname = "unknown",
                                            compression = compressionFactor)
    }
    ncout <- ncdf4::nc_create(outFile, varListOUT,
                              force_v4 = TRUE)
    # put variables
    for (i in 1:length(varListOUT)) {
        ncdf4::ncvar_put(nc = ncout, varid = varListOUT[[i]], vals = OUT[,,,i])
    }
    ncdf4::nc_close(ncout)
}


.formatRB3DVarsForNcdf <- function(radiativeBudget3DVars, fromNc = TRUE) {

    if (fromNc) {
        radiativeBudget3DVars <- gsub("__PLUS__", "+", radiativeBudget3DVars)
        radiativeBudget3DVars <- gsub("__MINUS__", "-", radiativeBudget3DVars)
    } else {
        radiativeBudget3DVars <- gsub("[+]", "__PLUS__", radiativeBudget3DVars)
        radiativeBudget3DVars <- gsub("[-]", "__MINUS__", radiativeBudget3DVars)
    }
    return(radiativeBudget3DVars)
}
