#' @export
setMethod(f = "getData",
          signature = signature(x = "character", sF = "SimulationFilter"),
          definition = function(x, sF){
              fileObj <- getFiles(x = x, sF = sF)

              if (product(sF) == "directions") dataFun <- directions
              if (product(sF) == "rb3D") dataFun <- rb3D
              if (product(sF) == "images") dataFun <- images
              simData <- dataFun(x = fileObj)

              return(simData)

          })


setMethod(f = "getData",
          signature = signature(x = "SimulationFiles", sF = "missing"),
          definition = function(x, sF){

              if (product(x) == "directions") dataFun <- directions
              if (product(x) == "rb3D") dataFun <- rb3D
              if (product(x) == "images") dataFun <- images
              simData <- dataFun(x = x)

              return(simData)

          })


