setMethod(f = "deleteFiles",
          signature = signature(x = "SimulationFiles", trianglesInput = "logical",
                                maketOutput = "logical"),
          definition = function(x, trianglesInput, maketOutput) {
              validObject(x, complete = TRUE)
              simDirs <- simdir(x)
              for (i in 1:length(simDirs)) {

                  if (trianglesInput) {
                      .deleteTrianglesInput(simDirs[i])
                  }

                  if (maketOutput) {
                      .deleteMaketOutput(simDirs[i])
                  }


              }
              return(x)
          }
)


.deleteTrianglesInput <- function(simulationDir) {

    inputDir <- file.path(simulationDir, "input")
    unlink(file.path(inputDir, "triangles"), recursive = TRUE)

}

.deleteMaketOutput <- function(simulationDir) {

    outputDir <- file.path(simulationDir, "input")
    unlink(file.path(outputDir, "maket.txt"))
}
