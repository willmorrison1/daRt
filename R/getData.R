#' @export
setMethod(f = "getData",
          signature = signature(x = "character", sF = "SimulationFilter"),
          definition = function(x, sF, nCores = 1){
              fileObj <- getFiles(x = x, sF = sF)

              if (product(sF) == "directions") dataFun <- directions
              if (product(sF) == "rb3D") dataFun <- rb3D
              if (product(sF) == "images") dataFun <- images
              simData <- dataFun(x = fileObj, nCores = nCores)

              return(simData)

          })


setMethod(f = "getData",
          signature = signature(x = "SimulationFiles", sF = "missing"),
          definition = function(x, sF, nCores = 1){

              if (product(x) == "directions") dataFun <- directions
              if (product(x) == "rb3D") dataFun <- rb3D
              if (product(x) == "images") dataFun <- images
              simData <- dataFun(x = x, nCores = nCores)

              return(simData)

          })


