#' @export
setMethod(f = "tappToRadiance",
          signature = signature(x = "SimulationData"),
          definition = function(x){

              require(dplyr)

              if (!"Tapp" %in% variables(x)) {
                  warning("Tapp is not in SimulationData variables")
                  return(x)
              }

              dataDF <- as.data.frame(x) %>%
                  dplyr::left_join(wavelengths(x) %>%
                                       dplyr::select(-c(lambdamin, lambdamid, lambdamax))) %>%
                  dplyr::mutate(value = .planck(equivalentWavelength, value)[variable == "Tapp"]) %>%
                  dplyr::select(-equivalentWavelength)

              x@simulationFilter@variables[x@simulationFilter@variables == "Tapp"] <- "Radiance"
              x@data <- dataDF

              return(x)
          })


.planck <- function(lam, Temp){
    #lam is wavelenght in microns
    #Temp is temperature in Kelvin

    #boltzmann constant m^2 kg s^-2 K^-1
    k <- 1.38064852e-23
    #planck constant m^2 kg s^-1
    h <- 6.62607004e-34
    #speed of light m s^-1
    cc <- 299792458


    lam <- lam * 1e-6
    specRad <- (((2 * h) * (cc^2))/((lam^5) * (exp((h * cc) / (lam * k * Temp)) - 1))) * 1e-6

    return(specRad)

}
