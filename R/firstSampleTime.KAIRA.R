## file:firstSampleTime.KAIRA.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Read sampling start time from the timestamps file
##
## Arguments:
##  dataDir   Data directory path
##  offset.us offset in KAIRA timestamps, in us.
##            The LOFAR timestamps are obviously for sample edges.
##            Because there is a 16384 tap filter for the 200 MHz signal,
##            the subbands will efficiently be filtered with a 16 tap
##            (16*5.12us=81.92us) filter. The filter mid point is thus
##            40.96 us earlier than the timestamp. In order to mimic a 5.12 us
##            filter, we will need to adjust the timestamps by
##            -40.96us + 5.12us/2 = -38.4 us. This value should not change
##            typically.
##
##            ... correction to this, the times match much better if we
##            guess that the timestamp is for "leading edge of the last sample"
##            i.e. -7*5.12 + 5.12/2 = -33.28
##
##
## Returns:
##  timeStamp Sampling time of first sample in POSIX format
##

firstSampleTime.KAIRA <- function( dataDir , offset.us=-33.28 )
  {

    sFile <- file.path(dataDir,'sampler.log')
    # If the sampler log file is not found try a couple
    # of steps upwards in directory hierarchy
    if(!file.exists(sFile)) sFile <- file.path(dataDir,'..','sampler.log')
    if(!file.exists(sFile)) sFile <- file.path(dataDir,'..','..','sampler.log')
    if(file.exists(sFile)){
      sLines <- readLines(sFile,100)
      for(n in sLines){
        # There are two lines with "Initial_timestamp",
        # we want the first of them
        if(length(grep("Initial_timestamp",n)) > 0 ){
          fullsec <- as.numeric(strsplit(n,' ')[[1]][2] )
          break
        }
      }
      for(n in sLines){
        if(length(grep("Initial_seqnum",n))    > 0 ) secnum <- as.numeric(strsplit(n,' ')[[1]][2]  )
      }

    }

    # The sequence numbering starts from exactly full seconds
    # at even seconds and from half a sample
    # after that att odd seconds.
    timeStamp <- fullsec + 512/1e8 * ( secnum + ifelse( ((fullsec%%2) == 0) , 0 , .5 ) ) + offset.us*1e-6

    return( timeStamp )

  }
