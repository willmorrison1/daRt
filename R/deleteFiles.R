setMethod(f = "deleteFiles",
          signature = signature(x = "SimulationFiles", deleteSimulationFiles = "boolean"),
          definition = function(x, deleteSimulationFiles = FALSE,
                                trianglesInput = FALSE, maketOutput = FALSE) {
              library(tools)
              #here i am - sort out inputs for deleteFiles

              validObject(x, complete = TRUE)
              simDirs <- baseDir(x)
              for (i in 1:length(simDirs)) {
                  if (trianglesInput) {
                      .deleteTrianglesInput(simDirs[i])
                  }
                  if (maketOutput) {
                      .deleteMaketOutput(simDirs[i])
                  }
              }

              if (deleteSimulationFiles) {
                  fileNamesToDelete <- files(x)$fileName
                  if (product(x) == "images") {
                      fileNamesToDelete <- paste0(tools::file_path_sans_ext(fileNamesToDelete),
                                                  c(".grf", ".mpr", ".gr#"))
                  }
                  unlink(fileNamesToDelete, recursive = FALSE)
              }

              invisible(return(NULL))
          }
)


.deleteTrianglesInput <- function(simulationDir) {

    inputDir <- file.path(simulationDir, "input")
    unlink(file.path(inputDir, "triangles"), recursive = TRUE)

}

.deleteMaketOutput <- function(simulationDir) {

    outputDir <- file.path(simulationDir, "output")
    unlink(file.path(outputDir, "maket.txt"))
}
