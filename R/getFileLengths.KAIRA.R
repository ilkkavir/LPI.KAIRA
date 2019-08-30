## file:getFileLengths.KAIRA.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Find number of samples in a single data file
##
## Arguments:
##  LPIparam  A LPI parameter list
##
## Returns:
##  dataFileLengths A named list of file lengths
##                  with elements "RX1", "RX2", "TX1", "TX2"
##

getFileLengths.KAIRA <- function( LPIparam )
  {

    # A vector for file lengths
    fileLengths <- rep(NA,4)
    names(fileLengths) <- c( "RX1" , "RX2" , "TX1" , "TX2" )

    # Read the file lengths in another function.
    for( XXN in c( "RX1" , "RX2" , "TX1" , "TX2" ) ){

        if( LPIparam[["nBeamlets"]][XXN]==0){
            ddir <- file.path( LPIparam[["dataDir"]][XXN] , LPIparam[["polarization"]][XXN] )
        }else{
            ddir <- file.path( LPIparam[["dataDir"]][XXN] , gsub(' ','0',format(LPIparam[["beamlets"]][[XXN]][1],width=3)) , LPIparam[["polarization"]][XXN] )
        }

      fileLengths[XXN] <- LPI.gdf:::fileLength.gdf( ddir , prefix=LPIparam[["fileNamePrefix"]][[XXN]] , extension=LPIparam[["fileNameExtension"]][[XXN]] )
    }
    
    return( list( dataFileLengths = fileLengths ) )
    
  }


