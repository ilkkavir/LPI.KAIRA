#
# generate the EISCAT VHF transmitted waveform from known
# code sequences
#
# I. Virtanen 2015
#


EISCATwaveform <- function(exp='beata',startTime=as.POSIXlt('2015-02-15 19:00:00.000',tz='UTC'),opath='.'){

    expstr <- trimws(tolower(exp))
    if (expstr=='beata'){
        bitlen <- 20 # 20 us
        IPP <- 5580 # 5580 us
        Loopc <- 14 # loop 14 times
        Sync <- 3200 # 32 us sync time after the 14 reps, counted in 0.1 us steps (copy this directly from elan)
        stimeDiff <- 82 # the first TX starts 82 us after the nominal exp start time
        codefile <- 'beata_v.txt' # file where the codes are stored
        srate <- 1 # sampling rate in MhZ
    }else if(expstr=='bella'){
        bitlen <- 45
        IPP <- 11250
        Loopc <- 5
        Sync <- 0
        stimeDiff <- 88
        codefile <- 'bella_v.txt'
        srate <- 1
    }else if(expstr=='manda'){
        bitlen <- 2.4
        IPP <- 1500
        Loopc <- 25
        Sync <- 0
        stimeDiff <- 73
        codefile <- 'manda_v.txt'
        srate <- 1/1.2
    }else{
        stop(paste('EISCATwaveform: unknown experiment',exp))
    }

    # read the codes
    codes <- as.matrix(read.table(file.path(system.file(package='LPI.KAIRA','extdata'),codefile)))
    ncodes <- nrow(codes)
    nbits <- ncol(codes)

    # create a matrix for the waveform, this will then be
    # converted into a vector
    # the IPP/bitlen might not work with all experiments, but works
    # with beata, bella, and manda
    waveformmat <- matrix( 0+0i , ncol=ncodes , nrow=IPP/bitlen )
    for( k in seq( ncodes ) ){
        waveformmat[ 1:nbits , k ] <- codes[ k ,  ]
    }
    
    # then convert into a vector
    waveform <- c( waveformmat )

    # repeat as necessary
    waveform <- rep( waveform , Loopc )

    # upsample to the desired sample rate
    waveform <- rep( waveform , each = bitlen*srate)

    # add the sync period
    waveform <- c( waveform , rep( 0 , Sync/10*srate ) )

    
    ddir <- file.path(opath,paste(exp,"-V-fake-",format(startTime,"%Y.%m.%d_%H.%M.%S"),sep=''))

    dir.create(ddir,recursive=TRUE)
    dir.create(file.path(ddir,'1'))

    
    
    fname <- file.path(ddir,'1',sprintf("data-%06d.gdf",1))
    write.gdf( waveform , waveform , fname=fname )
    
    writeSamplerLog(srate=srate,file=file.path(ddir,'sampler.log'))

    writeTimestamps(stime=as.double(startTime)+stimeDiff*1e-6+1e-6/srate,tstep=length(waveform)/srate*1e-6,n=1,file=file.path(ddir,'1','timestamps.log'))

    invisible(ddir)

}



write.gdf <- function( ddc , ddli , ddlr=NULL , fname="data-000001.gdf" , scale=1 )
    {

        ddr <- Re(ddc) * scale
        ddi <- Im(ddc) * scale
        ddi <- bitwOr( bitwShiftL( ddi , 1 ) , as.numeric( abs(ddli) > 0 ) )
        if(is.null(ddlr)){
            ddr <- bitwShiftL( ddr , 1 )
        }else{
            ddr <- bitwOr( bitwShiftL( ddr , 1 ) , as.numeric( abs(ddlr) > 0 ) )
        }

        dd <- c( rbind( ddr , ddi ) )

        writeBin( as.integer( dd ) , fname , size=2 , endian='big' )

        return()
    }


writeSamplerLog <- function( srate , file='sampler.log')
    {
        #
        #  INPUT:
        #    srate  the sample rate in MHz
        #    file   the log file name
        #
        #

        # a fake sampler log file with sample rate informatio
        cat(sprintf("Sample rate %30.28f MHz\n",srate),file=file)

    }

writeTimestamps <- function( stime , tstep , n=1 , file='timestamps.log')
    {
        #
        # write timestamps log file
        #
        #
        #

        append <- FALSE
        for (k in seq(n)){

            cat(sprintf("data-%06d.gdf %30.20f 0 0\n",k,(stime+(k-1)*tstep)),file=file,append=append)
            append <- TRUE

        }



    }

