setMethod(f = "deleteFiles",
          signature = signature(x = "SimulationFiles", deleteSimulationFiles = "logical"),
          definition = function(x, deleteSimulationFiles = FALSE,
                                trianglesInput = FALSE, maketOutput = FALSE,
                                trianglesOutput = FALSE) {
              library(tools)

              validObject(x, complete = FALSE)
              simDirs <- baseDir(x)
              for (i in 1:length(simDirs)) {
                  if (trianglesInput) {
                      .deleteTrianglesInput(simDirs[i])
                  }
                  if (maketOutput) {
                      .deleteMaketOutput(simDirs[i])
                  }
                  if (trianglesOutput) {
                      .deleteTrianglesOutput(simDirs[i])
                  }
              }

              if (deleteSimulationFiles) {
                  fileNamesToDelete <- fileName(x)
                  if (product(x) == "images") {
                      fileNamesToDelete <- apply(expand.grid(tools::file_path_sans_ext(fileNamesToDelete),
                                                             c(".grf", ".mpr", ".gr#")), 1,
                                                 function(x) paste0(x[1], ".", x[2]))
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

.deleteTrianglesOutput <- function(simulationDir) {

    outputDir <- file.path(simulationDir, "output")
    unlink(file.path(outputDir, "triangles.txt"))

}

.deleteMaketOutput <- function(simulationDir) {

    outputDir <- file.path(simulationDir, "output")
    unlink(file.path(outputDir, "maket.txt"))
}
