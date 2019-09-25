setMethod(f = "deleteInputFiles",
          signature = signature(x = "SimulationFiles", triangles = "logical"),
          definition = function(x, triangles) {
              validObject(x, complete = TRUE)

              simDirs <- simdir(x)
              for (i in 1:length(simDirs)) {

                  inputDir <- file.path(simDirs[i], "input")

                  if (triangles) {
                      unlink(file.path(inputDir, "triangles"), recursive = TRUE)
                  }

                  return(x)
              }
          }
)
