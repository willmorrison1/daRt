setMethod(f = "getData",
          signature = signature(x = "character", sF = "SimulationFilter"),
          definition = function(x, sF){


              if (product(sF) == "directions") dataFun <- directions
              if (product(sF) == "rb3D") dataFun <- rb3D
              if (product(sF) == "images") dataFun <- images

              simData <- dataFun(x = x, sF = sF)

              return(simData)

          })
