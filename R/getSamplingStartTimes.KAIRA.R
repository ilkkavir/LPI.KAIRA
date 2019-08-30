## file:getSamplingStartTimes.KAIRA.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Read the sampiling start times in each data directory and
## return the results as unix-times with fractional seconds
## ( seconds from 1970-01-01 )
##
## KAIRA special that reads the RX sampling start time
## from LOFAR timestamp and sequence number
##
## Arguments:
##  LPIparam A LPI parameter list
##
## Returns:
##  startTimes A named vector of sampling start times in
##             POSIX format
##

getSamplingStartTimes.KAIRA <- function( LPIparam )
  {

    startTimes <- rep(NA,4)
    names( startTimes ) <- c( "RX1" , "RX2" , "TX1" , "TX2" )

    # Transmitter samples are recorded at
    # Tromso with the standard usrp system
    for( XXN in c( "TX1" , "TX2" ) ){
      startTimes[XXN] <- LPI.gdf:::firstSampleTime.gdf( LPIparam[['dataDir']][[XXN]] , prefix=LPIparam[["fileNamePrefix"]][[XXN]] , nInt=6 , extension=".gdf" , stampFile="timestamps.log" , fLen=LPIparam[["dataFileLengths"]][[XXN]] , sampFreq=LPIparam[["dataSampleFreqs"]][[XXN]]  )
    }

    # Receiver samples are recorded using
    # the KAIRA local pipeline
    for( XXN in c( "RX1" , "RX2" ) ){
      startTimes[XXN] <- firstSampleTime.KAIRA( LPIparam[['dataDir']][[XXN]] , LPIparam[["KAIRAoffset.us"]] )
    }

    return( startTimes )

  }
