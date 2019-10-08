#' @export
setMethod("wavelengths", "SimulationFiles", function(x){

    if (nrow(x@wavelengths) == 0) return(.getWavelengthsDF(x))

    return(x@wavelengths)

})


.getWavelengthsDF <- function(x) {

    rawResultsList <- getSimulationProperty(x, "lambdamin|lambdamax|equivalentWavelength",
                                            allow_multiLines = TRUE)

    for (i in 1:length(rawResultsList)) {
        rawResultsList[[i]] <- as.data.frame(rawResultsList[[i]])
        rawResultsList[[i]]$simName <- names(rawResultsList)[i]
    }

    rawResults <- do.call(rbind, rawResultsList)

    bandDF <- rawResults %>%
        tidyr::separate(col = V1, into = c("module", "band", "var"), sep = "[.]") %>%
        dplyr::select(-c("module")) %>%
        dplyr::mutate(band = gsub("band", "", band)) %>%
        dplyr::distinct(band, var, simName, .keep_all = TRUE) %>%
        reshape2::dcast(band + simName ~ var, value.var = "V2")

    if (!"equivalentWavelength" %in% names(bandDF)) {
        bandDF$equivalentWavelength <- NA
    }

    bandDF <- bandDF %>%
        dplyr::mutate_at(c("equivalentWavelength", "lambdamax", "lambdamin"),
                         as.numeric) %>%
        dplyr::mutate_at("band", as.integer) %>%
        dplyr::mutate(lambdamid = lambdamin + ((lambdamax - lambdamin) / 2)) %>%
        dplyr::arrange(simName, band) %>%
        dplyr::select(simName, band, lambdamin, lambdamid, lambdamax, equivalentWavelength)

    return(bandDF)
}
