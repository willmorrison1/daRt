setMethod(f = "simulationFilter",
          signature(product = "character"),

          definition = function(product, ...,
                                bands = .defaultBands(),
                                variables = .defaultVariables(),
                                iters = .defaultIters(),
                                variablesRB3D = .defaultVariablesRB3D(),
                                typeNums = .defaultTypeNums(),
                                imageTypes = .defaultimageTypes(),
                                imageNos = .defaultimageNos()){

              s <- new("SimulationFilter",
                       bands = bands,
                       variables = variables,
                       iters = iters,
                       variablesRB3D = variablesRB3D,
                       typeNums = typeNums,
                       imageTypes = imageTypes,
                       imageNos = imageNos,
                       product = product)

              validObject(s, complete = TRUE)
              return(s)
          }
)


.defaultBands <- function(){

    as.integer(0)

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

.defaultimageNos <- function(){

    integer()

}
