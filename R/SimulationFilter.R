setMethod(f = "simulationFilter",
          signature(product = "character"),

          definition = function(product, ...,
                                bands = .defaultBands(),
                                variables = .defaultVariables(),
                                iters = .defaultIters(),
                                variablesRB3D = .defaultVariablesRB3D(),
                                typeNums = .defaultTypeNums(),
                                imageType = .defaultImageType(),
                                imageNo = .defaultImageNo()){

              s <- new("SimulationFilter",
                       bands = bands,
                       variables = variables,
                       iters = iters,
                       variablesRB3D = variablesRB3D,
                       typeNums = typeNums,
                       imageType = imageType,
                       imageNo = imageNo,
                       product = product)

              validObject(s, complete = TRUE)
              return(s)
          }
)
#to check

# setMethod(f = "simulationFilter",
#           signature(product = "missing"),
#           definition = function(x){
#               x@simulationFilter
#           }
# )


.defaultBands <- function(){

    "BAND0"

}


.defaultVariables <- function(){

    c("BRF")

}

.defaultIters <- function(){

    c("ITER1", "ITERX")

}


.defaultVariablesRB3D <- function(){

    c("Intercepted", "Scattered", "Emitted", "Absorbed", "+ZFaceExit", "+ZFaceEntry")

}


.defaultTypeNums <- function(){

    c("")

}

.defaultImageType <- function(){

    c("")

}

.defaultImageNo <- function(){

    numeric()

}
