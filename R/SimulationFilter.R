setMethod(f = "simulationFilter",
          signature(product = "character", x = "missing"),

          definition = function(product,
                                x,
                                bands = .defaultBands(),
                                variables = .defaultVariables(),
                                iters = .defaultIters(),
                                variablesRB3D = .defaultVariablesRB3D(),
                                typeNums = .defaultTypeNums(),
                                imageTypes = .defaultimageTypes(),
                                imageNums = .defaultimageNums()){

              s <- new("SimulationFilter",
                       bands = bands,
                       variables = variables,
                       iters = iters,
                       variablesRB3D = variablesRB3D,
                       typeNums = typeNums,
                       imageTypes = imageTypes,
                       imageNums = imageNums,
                       product = product)

              validObject(s, complete = TRUE)
              return(s)
          }
)

#' @export
setMethod(f = "simulationFilter<-",
          signature(x = "SimulationFiles", value = "SimulationFilter"),
          definition = function(x, value){

              #bands update
              wavelengthVals <- .getWavelengthsDF(x)
              x@wavelengths <- wavelengthVals
              if (length(value@bands) == 0) {
                  value@bands <- wavelengthVals$band
              }
              x@simulationFilter <- value

              return(x)

          })

#' @export
setMethod(f = "simulationFilter",
          signature(product = "missing", x = "SimulationFiles"),
          definition = function(product, x){

              x@simulationFilter
          })


.defaultBands <- function(){

    as.integer(0:1)

}


.defaultVariables <- function(){

    c("BRF")

}

.defaultIters <- function(){

    c("ITER1", "ITER2")

}


.defaultVariablesRB3D <- function(){

    c("Intercepted", "Scattered", "Emitted", "Absorbed", "+ZFaceExit", "+ZFaceEntry")

}


.defaultTypeNums <- function(){

    c("")

}

.defaultimageTypes <- function(){

    c("ima", "camera")

}

.defaultimageNums <- function(){

    integer()

}
