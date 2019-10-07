#' @export
setMethod("bandInfo", "SimulationFiles", function(x){

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
        reshape2::dcast(band + simName ~ var, value.var = "V2") %>%
        dplyr::mutate_at(c("band", "equivalentWavelength", "lambdamax", "lambdamin"),
                         as.numeric) %>%
        dplyr::mutate(lambdamid = lambdamin + ((lambdamax - lambdamin) / 2)) %>%
        dplyr::arrange(simName, band)

    return(bandDF)

})
