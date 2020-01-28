#' @export
setMethod(f = "getFiles",
          signature = signature(x = "character", sF = "SimulationFilter"),
          definition = function(x, sF){

              require(dplyr)

              if (product(sF) == "directions") filesFun <- directionsFiles
              if (product(sF) == "rb3D") filesFun <- rb3DFiles
              if (product(sF) == "images") filesFun <- imageFiles

              simFilesList <- vector(mode = "list", length = length(x))

              for (i in 1:length(x)) {
                  simFilesList[[i]] <- filesFun(x = x[i], sF = sF)
              }

              sequenceInfoList <- lapply(simFilesList, function(x) x@sequenceInfo)
              uniqueSequenceInfo <- unique(lapply(simFilesList, function(x) x@sequenceInfo[-3]))
              names(sequenceInfoList) <- sapply(simFilesList, function(x) x@simName)

              if (length(uniqueSequenceInfo) > 1) {
                  stop("When merging multiple simulations,
                       there was a sequence info mismatch between simulations
                       i.e. the sequence elements use different variables")
              }

              sequenceInfoOut <- uniqueSequenceInfo[[1]]
              simFilesStacked <- new(Class = class(simFilesList[[1]])[1],
                                     baseDir = sapply(simFilesList, baseDir),
                                     isSequence = sapply(simFilesList, function(x) x@isSequence),
                                     sequenceInfo = sequenceInfoOut)
              simFilesStacked@sequenceInfoList <- sequenceInfoList
              simFilesStacked@sequenceInfoDf <- .sequenceInfoListToDf(sequenceInfoList)
              simulationFilter(simFilesStacked) <- simFilesList[[1]]@simulationFilter
              simFilesStacked@softwareVersion <- versionInfo(simFilesStacked)

              for (i in 1:length(simFilesList)) {
                  simFilesList[[i]]@files$simName <- simname(simFilesList[[i]])
                  sequenceParamDF <- simFilesList[[i]]@sequenceInfo
                  if (nrow(sequenceParamDF) > 0) {
                      simFilesList[[i]]@files <- cbind(
                          simFilesList[[i]]@files,
                          reshape2::dcast(sequenceParamDF, .~parameterNo,
                                          value.var = "parameterVal")[-1])
                  }
              }

              simFilesStacked@files <- dplyr::bind_rows(lapply(simFilesList, function(x) x@files))
              simFilesStacked@wavelengths <- wavelengths(simFilesStacked)
              validObject(simFilesStacked, complete = TRUE, test = TRUE)

              return(simFilesStacked)
          })

.sequenceInfoListToDf <- function(sequenceInfoList) {

    sequenceInfoList_len <- sapply(sequenceInfoList, length)

    if (all(sequenceInfoList_len == 0))
        return(data.frame())

    if (any(sequenceInfoList_len == 0))
        stop("Cannot handle a mixture of sequenced (i.e. using DART sequencer) and non-sequenced simulations")

    sequenceInfoMelted <- reshape2::melt(sequenceInfoList, id.vars = c("parameterFullName", "parameterNo"))

    sequenceInfoDf <- sequenceInfoMelted %>%
        dplyr::select(-parameterFullName) %>%
        reshape2::dcast(L1 ~ parameterNo, value.var = "value") %>%
        dplyr::rename(simName = L1)

    return(sequenceInfoDf)
}


