setMethod(f = "show",  signature = "SimulationHandle",
          function(object){
            print(object@simDir)
          }
)


setMethod(f = "show",  signature = "SimulationFilter",
          function(object){
              cat(paste("'SimulationFilter' object for DART product:", object@product, "\n"))
              cat("\n")
              cat(paste("bands:         ", paste0(object@bands, collapse = ", "), "\n"))
              cat(paste("variables:     ", paste0(object@variables, collapse = ", "), "\n"))
              cat(paste("iterations:    ", paste0(object@iters, collapse = ", "), "\n"))
              cat(paste("variablesRB3D: ", paste0(object@variablesRB3D, collapse = ", "), "\n"))
              cat(paste("typeNums:      ", paste0(object@typeNums, collapse = ", "), "\n"))
              cat(paste("imageType:     ", paste0(object@imageType, collapse = ", "), "\n"))
              cat(paste("imageNo:       ", paste0(object@imageNo, collapse = ", "), "\n"))
          }
)

