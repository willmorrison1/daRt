#' @export
setMethod(f = "baseDir", signature = "SimulationHandle",
          definition = function(x){
              x@baseDir
          }
)


#' @export
setMethod(f = "subDir", signature(x = "SimulationFilter"),
          definition = function(x){

              return(.getSubDir(x))
          })


#' @export
setMethod(f = "subDir", signature(x = "SimulationFiles"),
          definition = function(x){

              return(.getSubDir(x@simulationFilter))
          })


.getSubDir <- function(sF) {

    bands <- paste0("BAND", bands(sF))
    variables <- variables(sF)
    iters <- iters(sF)
    typeNums <- typeNums(sF)
    nDirsExpected <- prod(length(bands), length(variables),
                          length(iters), length(typeNums))
    dirsExpected <- character(length = nDirsExpected)
    nIter <- 1
    OUTlist <- vector("list", length = nDirsExpected)
    for (a in 1:length(typeNums)) {
        for (i in 1:length(bands)) {
            for (j in 1:length(variables)) {
                for (v in 1:length(iters)) {
                    dirsExpected[nIter] <- file.path(
                        "output", typeNums[a], bands[i],
                        variables[j], iters[v])
                    OUTlist[[nIter]]$typeNum <- typeNums[a]
                    OUTlist[[nIter]]$band <- bands[i]
                    OUTlist[[nIter]]$variable <- variables[j]
                    OUTlist[[nIter]]$iter <- iters[v]
                    OUTlist[[nIter]]$dirName <- dirsExpected[nIter]

                    nIter <- nIter + 1
                }
            }
        }
    }

    return(dplyr::bind_rows(OUTlist))
}
