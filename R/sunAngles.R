#' @export
setMethod("sunAngles", "SimulationFiles", function(x){

    if (nrow(x@sunAngles) == 0) return(.getSunAnglesDF(x))

    return(x@sunAngles)

})

.getSunAnglesDF <- function(x) {

    sunPhi <- getSimulationProperty(x, "sunPhi", allow_multiLines = FALSE)

    sunPhiDF <- data.frame(simName = names(sunPhi), sunPhi = as.numeric(sapply(sunPhi, function(x) x[2])))

    sunTheta <- getSimulationProperty(x, "sunTheta", allow_multiLines = FALSE)

    sunThetaDF <- data.frame(simName = names(sunTheta), sunTheta = as.numeric(sapply(sunTheta, function(x) x[2])))

    sunAngleDF <- sunPhiDF %>%
        dplyr::left_join(sunThetaDF, by = "simName") %>%
        dplyr::arrange(simName)

    return(sunAngleDF)
}
