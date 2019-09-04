#' @export
setMethod(f = "sequenceParameters", signature(x = "SimulationFiles"),
          definition = function(x){

              if (any(!x@isSequence)) return(NULL)

              seqList <- x@sequenceInfoList

              seqMelt <- reshape2::melt(
                  seqList,
                  id.vars = c("parameterFullName", "parameterNo", "parameterVal"))

              seqCast <- seqMelt %>%
                  reshape2::dcast(L1 ~ parameterNo, value.var = "parameterVal")

              colnames(seqCast)[1] <- "simName"

              return(seqCast)
          }
)
