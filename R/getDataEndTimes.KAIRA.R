## file:getDataEndTimes.KAIRA.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

## 
## Find the sampling times of latest recorded data sample
## in each data directory, assuming that recording has
## been continuous since starting the sampling.
##
## TX samples will never end because KAIRA analysis
## will re-use old samples as necessary.
##
## Arguments:
##  LPIparam A LPI parameter list
##
## Returns:
##  endTimes A named vector of data end times
##           with entries "RX1", "RX2", "TX1", "TX2"
##

getDataEndTimes.KAIRA <- function( LPIparam )
  {

    endTimes <- rep(NA,4)
    names( endTimes ) <- c( "RX1" , "RX2" , "TX1" , "TX2" )

    for( XXN in c( "RX1" , "RX2" ) ){

      endTimes[XXN] <- LPI.gdf:::latestSampleTime.gdf( LPIparam[['dataDir']][XXN] , prefix="data-" , extension=".gdf" , fLen=LPIparam[["dataFileLengths"]][XXN] , sampFreq=LPIparam[["dataSampleFreqs"]][XXN] , firstSampleTime=LPIparam[["dataStartTimes"]][XXN])
                                             
    }

    endTimes["TX1"] <- Inf
    endTimes["TX2"] <- Inf
    
    return(endTimes)

  }
