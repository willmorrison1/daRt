setMethod(f = "show",  signature = "SimulationHandle",
          function(object){

              print(baseDir(object))

          })


setMethod(f = "show",  signature = "SimulationFilter",
          function(object){
              cat(paste("'SimulationFilter' object for DART product:", object@product, "\n"))
              cat("\n")
              cat(paste("bands:         ", paste0(bands(object), collapse = ", "), "\n"))
              cat(paste("variables:     ", paste0(variables(object), collapse = ", "), "\n"))
              cat(paste("iterations:    ", paste0(iters(object), collapse = ", "), "\n"))
              cat(paste("variablesRB3D: ", paste0(variablesRB3D(object), collapse = ", "), "\n"))
              cat(paste("typeNums:      ", paste0(typeNums(object), collapse = ", "), "\n"))
              cat(paste("imageTypes:     ", paste0(imageTypes(object), collapse = ", "), "\n"))
              cat(paste("imageNums:       ", paste0(imageNums(object), collapse = ", "), "\n"))
          }
)

