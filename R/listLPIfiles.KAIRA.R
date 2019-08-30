## file:listLPIfiles.KAIRA.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

## List files and sample indices needed for
## the given integration period.
##
## KAIRA special: re-use TX samples if new ones are not
##                available.
##
## Arguments:
##  intPeriod Integration period number
##  LPIparam  A LPI parameter list
##
## Returns:
##  datalist A named list with elements
##            'files'  A character vector of file names
##            'n'      A numeric vector of number of samples
##                     to read from each file
##            'starts' A numeric vector of sample indices
##                     from which to start in each file
##

listLPIfiles.KAIRA <- function( intPeriod , LPIparam )
  {

    datalist   <- list()

    # List for files
    datalist[["files"]]  <- list()

    # List for data indices
    datalist[["starts"]] <- list()

    # Number of sampeles to read from each file
    datalist[["n"]]      <- list()

    for( XXN in c( "RX1" , "RX2" ) ){

      # Number of samples in between sampling start time
      # and analysis start time
#      sampleDiff        <- round( ( LPIparam[["startTime"]] - LPIparam[["dataStartTimes"]][XXN] ) * LPIparam[["dataSampleFreqs"]][XXN] )
      # add 1, because the dataStartTimes are for trailing of a sample, IV 20151202. The difference is significant when
      # TX and RX are recorded with different sample rates. Do not round yet...
#      sampleDiff        <- round( ( LPIparam[["startTime"]] - LPIparam[["dataStartTimes"]][XXN] ) * LPIparam[["dataSampleFreqs"]][XXN] ) + 1
#      sampleDiff        <- ( LPIparam[["startTime"]] - LPIparam[["dataStartTimes"]][XXN] ) * LPIparam[["dataSampleFreqs"]][XXN]  + 1
      # add only 0.5 to correct for filtering... this will center the final, resampled, data correctly
      sampleDiff        <- ( LPIparam[["startTime"]] - LPIparam[["dataStartTimes"]][XXN] ) * LPIparam[["dataSampleFreqs"]][XXN]  + .5

      # Range of samples included in the integration period,
      # counted from the start of recording
#      intPerSampleRange <- sampleDiff + ceiling( c( (intPeriod - 1) , intPeriod ) * LPIparam[["timeRes.s"]] * LPIparam[["dataSampleFreqs"]][XXN] ) + 1
      # do not ceil but round.. this is better when sample rates of TX and RX are not matched
      intPerSampleRangeF <- sampleDiff + c( (intPeriod - 1) , intPeriod ) * LPIparam[["timeRes.s"]] * LPIparam[["dataSampleFreqs"]][XXN] + 1
      intPerSampleRange <- round(intPerSampleRangeF)

      # Return an emptly list if the sample range starts from negative values
      if( intPerSampleRange[1] <= 0 ) return(datalist)

      if( intPerSampleRange[2] > 1 ){

        # Range of file numbers included
        # in the integration period
        intPerFileRange   <- ceiling( intPerSampleRange / LPIparam[["dataFileLengths"]][XXN] )

        intPerNfiles      <- diff(intPerFileRange) + 1

        datalist[["files"]][[XXN]]           <- vector(mode="character",length=intPerNfiles)
        datalist[["starts"]][[XXN]]          <- rep( 1, intPerNfiles )
        datalist[["n"]][[XXN]]               <- rep( LPIparam[["dataFileLengths"]][XXN] , intPerNfiles )
        datalist[["starts"]][[XXN]][1]       <- ceiling( intPerSampleRange[1] %% LPIparam[["dataFileLengths"]][XXN] )
        datalist[["n"]][[XXN]][1]            <- LPIparam[["dataFileLengths"]][XXN] - datalist[["starts"]][[XXN]][1] + 1
        datalist[["n"]][[XXN]][intPerNfiles] <- floor( intPerSampleRange[2] %% LPIparam[["dataFileLengths"]][XXN]) - datalist[["starts"]][[XXN]][intPerNfiles] + 1
        # If all samples are read from the last file
        # the row above gives a wrong result
        if( datalist[["n"]][[XXN]][intPerNfiles] == 0 ) datalist[["n"]][[XXN]][intPerNfiles] <- LPIparam[["dataFileLengths"]][XXN]

        for( k in 1:intPerNfiles){
          n <- intPerFileRange[1] + k - 1
          fstr <- paste( LPIparam[["fileNamePrefix"]][XXN] , paste(rep('0',(6-ceiling(log10(n+.1)))) , sep='',collapse='') , n , ".gdf" , sep='',collapse='')
#          datalist[["files"]][[XXN]][k] <- file.path( LPIparam[["dataDir"]][XXN] , fstr )
          datalist[["files"]][[XXN]][k] <- fstr
        }
      }
    }

    # make a modified startTime for TX, which makes a better match with the RX decimation.
    # (The TX data is usually sampled faster than the KAIRA data)
    startTimeTX <- LPIparam[["startTime"]]
    if(LPIparam[["dataSampleFreqs"]]["RX2"]<LPIparam[["dataSampleFreqs"]]["TX2"]){
        startTimeTX <- startTimeTX - ( intPerSampleRangeF[1] - intPerSampleRange[1] ) / LPIparam[["dataSampleFreqs"]]["RX2"]
    }


    # KAIRA special for TX samples:
    # if TX data from the current integration period is
    # not available, start looking for nearby integration
    # periods until suitable data is found
    for( XXN in c( "TX1" , "TX2" ) ){

      iperDiff <- 0
      dataend  <- 0

      # Loop until an integration period
      # with suitable data is found
      repeat{

        # The loop must be breaked if there is
        # absolutely no suitable data
        if( ( intPeriod + iperDiff ) == 0 ) dataend <- dataend + 1
        if( ( intPeriod + iperDiff ) == LPIparam[["maxIntPeriod"]] ) dataend <- dataend + 1
        if( dataend == 2 ) break

        # Number of samples in between sampling
        # start time and analysis start time
#        sampleDiff        <- round( ( LPIparam[["startTime"]] - LPIparam[["dataStartTimes"]][XXN] ) * LPIparam[["dataSampleFreqs"]][XXN] )
        # add 1, because the dataStartTimes are for "falling edge" of a sample, IV 20151202. The difference is significant when
        # TX and RX are recorded with different sample rates
#        sampleDiff        <- round( ( LPIparam[["startTime"]] - LPIparam[["dataStartTimes"]][XXN] ) * LPIparam[["dataSampleFreqs"]][XXN] ) + 1
        # do not round yet...
#        sampleDiff        <- ( LPIparam[["startTime"]] - LPIparam[["dataStartTimes"]][XXN] ) * LPIparam[["dataSampleFreqs"]][XXN]  + 1
        # use the modified start time
#        sampleDiff        <- ( startTimeTX - LPIparam[["dataStartTimes"]][XXN] ) * LPIparam[["dataSampleFreqs"]][XXN]  + 1
        sampleDiff        <- ( startTimeTX - LPIparam[["dataStartTimes"]][XXN] ) * LPIparam[["dataSampleFreqs"]][XXN]  + 0.5

        # Range of samples included in the integration period,
        # counted from the start of recording
#        intPerSampleRange <- sampleDiff + ceiling( c( (intPeriod  - 1) ,  intPeriod ) * LPIparam[["timeRes.s"]] * LPIparam[["dataSampleFreqs"]][XXN] )
        # round, not ceil
        intPerSampleRange <- round( sampleDiff + c( (intPeriod  - 1) ,  intPeriod ) * LPIparam[["timeRes.s"]] * LPIparam[["dataSampleFreqs"]][XXN] )

        intPerSampleRange[1] <- max( 1 , intPerSampleRange[1] + 1 )

#        intPerSampleRange <- intPerSampleRange + ceiling( iperDiff * LPIparam[["timeRes.s"]] * LPIparam[["dataSampleFreqs"]][[XXN]] )
        # not ceil, round
        intPerSampleRange <- intPerSampleRange + round( iperDiff * LPIparam[["timeRes.s"]] * LPIparam[["dataSampleFreqs"]][[XXN]] )

        if(intPerSampleRange[1]>0){

          # Range of file numbers included
          # in the integration period
          intPerFileRange   <- ceiling( intPerSampleRange / LPIparam[["dataFileLengths"]][XXN] )

          intPerNfiles      <- diff(intPerFileRange) + 1

          datalist[["files"]][[XXN]]           <- vector(mode="character",length=intPerNfiles)
          datalist[["starts"]][[XXN]]          <- rep( 1, intPerNfiles )
          datalist[["n"]][[XXN]]               <- rep( LPIparam[["dataFileLengths"]][XXN] , intPerNfiles )
          datalist[["starts"]][[XXN]][1]       <- ceiling( intPerSampleRange[1] %% LPIparam[["dataFileLengths"]][XXN] )
          datalist[["n"]][[XXN]][1]            <- LPIparam[["dataFileLengths"]][XXN] - datalist[["starts"]][[XXN]][1] + 1
          datalist[["n"]][[XXN]][intPerNfiles] <- floor( intPerSampleRange[2] %% LPIparam[["dataFileLengths"]][XXN]) - datalist[["starts"]][[XXN]][intPerNfiles] + 1
          # If all samples are read from the last file
          # the row above gives a wrong result
          if( datalist[["n"]][[XXN]][intPerNfiles] == 0 ) datalist[["n"]][[XXN]][intPerNfiles] <- LPIparam[["dataFileLengths"]][XXN]

          for( k in 1:intPerNfiles){
              n <- intPerFileRange[1] + k - 1
              # If we have a fake TX signal with exactly one EISCAT integration period per file, we can just set n=1
              if( LPIparam[["fakeTX"]] ) n <- 1
              fstr <- paste( LPIparam[["fileNamePrefix"]][XXN] , paste(rep('0',(6-ceiling(log10(n+.1)))) , sep='',collapse='') , n , ".gdf" , sep='',collapse='')
#            datalist[["files"]][[XXN]][k] <- file.path( LPIparam[["dataDir"]][XXN] , fstr )
              datalist[["files"]][[XXN]][k] <- fstr
          }

          # Test if the listed files really exist
          fexists <- TRUE
          for( k in 1:intPerNfiles ){
              if( ! file.exists( file.path( LPIparam[["dataDir"]][XXN] , datalist[["files"]][[XXN]][k] )) ){
                  # the file could be also compressed with LPI.gdf_0.1-5
                  if( ! file.exists( paste(file.path( LPIparam[["dataDir"]][XXN] , datalist[["files"]][[XXN]][k]  ) , '.bz2' , sep='' ) ) ){
                      fexists <- FALSE
                      break
                  }
              }
          }
        # If all files exist we can break the loop
          if( fexists ) break

      }

        # Update iperDiff, we will look simultaneously
        # to both directions
        if( iperDiff >= 0 ){
            iperDiff <- -iperDiff - 1
        }else{
            iperDiff <- -iperDiff
        }

    }
  }

    return(datalist)

}
