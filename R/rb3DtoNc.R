# setMethod(f = "rb3DtoNc",
#           signature = signature(x = "SimulationFiles"),
#           definition = function(x){
#
#               if (product(x) != "rb3D") {
#                   warning("rb3DtoNc needs rb3D product. Nothing to do.")
#                   return(x)
#               }
#               #check which files are .bin
#               #loop over bin files, else return all .nc with "no work to do"
#               for (i in 1:length(files(x))) {
#
#                   #load file as RB3D
#                   #write file as nc
#                   #read file as nc
#                   #check files are "identical" via dims
#                   #update "files" entry as .nc
#                   #delete .bin entry
#               }
#               return(x)
#           }
# )
