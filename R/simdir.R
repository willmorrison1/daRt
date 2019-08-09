setMethod(f = "simdir", signature = "SimulationHandle",
          definition = function(x){
              x@simDir
          }
)

setMethod(f = "simdir", signature(x = "SimulationFilter"),
          definition = function(x){

              simFltr <- x
              bands <- bands(simFltr)
              variables <- variables(simFltr)
              iters <- iters(simFltr)
              typeNums <- typeNums(simFltr)
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

              dplyr::bind_rows(OUTlist)
          })
