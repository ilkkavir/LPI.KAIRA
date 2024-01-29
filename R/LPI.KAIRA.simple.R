## file:LPI.KAIRA.simple.R
## (c) 2024- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## A wrapper to the function LPI.KAIRA::LPI.KAIRA with default parameters based on the experiment names
##
## Arguments:
##    KBTexp      name of the KAIRA kbt experiment
##    EISCATtime  EISCAT experiment start time
##    btime       analysis start time
##    etime       analysis end time
##    datapath    KAIRA data path
##    opath       output path
##    cluster     a computer cluster definition (eg from getMPIcluster)
##    solver      the LPI solver, see documentation of LPI for details
##    rangeCoverage range coverage above and below the nominal beam intersection, degrees in RX beam elevation

LPI.KAIRA.simple <- function( KBTexp , EISCATtime , btime , etime , datapath , opath , cluster , solver='fishs' , rangeCoverage=5){
    
    

    # experiment specific parameters
    expstr <- trimws(tolower(KBTexp))
    if (expstr=='vhf37_beata_d_reg'){
        EISCATexp <- 'beata'
        RXaz <- 313.95
        RXele <- seq(0,9)*2+36
        Nsubband <- 1
        TXaz <- 0
        TXele <- 90
        rlims <- seq(20,300)*20*.299792458/2
        llims <- c( seq(32)*20 , c( rbind( seq(35)*5580-640 , seq(35)*5580+640 ) ) )
        maxr <- c( rep(Inf,31) , rep(c(0,Inf),length.out=(length(llims)-32)))
        filtLen <- 20
        timeRes <- 5
        freqOffset <- c(RX=32812.5,TX=0)
        radarFreq <- 223.6e6
    }else if(expstr=='vhf37_beata_e_reg'){
        EISCATexp <- 'beata'
        RXaz <- 313.95
        RXele <- seq(0,9)*2+44
        Nsubband <- 1
        TXaz <- 0
        TXele <- 90
        rlims <- seq(20,300)*20*.299792458/2
        llims <- seq(32)*20
        maxr <- c( rep(Inf,31) , rep(c(0,Inf),length.out=(length(llims)-32)))
        filtLen <- 20
        timeRes <- 5
        freqOffset <- c(RX=32812.5,TX=0)
        radarFreq <- 223.6e6
    }else if(expstr=='vhf37_beata_f_reg'){
        EISCATexp <- 'beata'
        RXaz <- 313.95
        RXele <- seq(0,9)*2+62
        Nsubband <- 1
        TXaz <- 0
        TXele <- 90
        rlims <- seq(20,300)*20*.299792458/2
        llims <- seq(32)*20
        maxr <- c( rep(Inf,31) , rep(c(0,Inf),length.out=(length(llims)-32)))
        filtLen <- 20
        timeRes <- 5
        freqOffset <- c(RX=32812.5,TX=0)
        radarFreq <- 223.6e6
    }else if(expstr=='vhf7_manda_d_reg'){
        EISCATexp <- 'manda'
        RXaz <- 313.95
        RXele <- seq(0,9)*2+36
        Nsubband <- 3
        TXaz <- 0
        TXele <- 90
        rlims <- seq(166,2224)*2.4*.299792458/2
        llims <- c( seq(61)*2.4 , c( rbind( seq(127)*1500-146.4 , seq(127)*1500+146.4 ) ) )
        maxr <- c( rep(Inf,60) , rep(c(0,Inf),length.out=(length(llims)-61)))
        filtLen <- 2.4
        timeRes <- 4
        freqOffset <- c(RX=14062.5-195312.5,TX=0) # TX is at F10 (224.4 MHz) and we received subbands 124:126 (subband 125 is offset from F10 by 14062.5 Hz and one subband is 195.3125 kHz)
        radarFreq <- 224.4e6
    }else{
        stop(paste('LPI.KAIRA.simple: unknown experiment',KBTexp))
    }

    # parameters common to all KAIRA receptions

    # offset to KAIRA timestamps
    KAIRAoffset.us <- -33.28

    # no ground clutter in the remote site data
    maxClutterRange <- 0

    # no index corrections, as we create the TX waveform by ourselves..
    indexShifts <- list(TX=c(0,0),RX=c(0,0))

    # merge things
    # output sub-directory names
    odirs <- gsub(' ','0',format(as.character(seq(0,9)),width=3,justify='right'))      

    # create the artificial TX waveform
    stime <- ISOdate(EISCATtime[1],EISCATtime[2],EISCATtime[3],EISCATtime[4],EISCATtime[5],EISCATtime[6],tz='utc')
    # write to opath, because we must have write priviledges there...
    TXpath  <- makeEISCATwaveform(exp='EISCATexp',startTime=stime,opath=opath)

    # call LPI.KAIRA for each beam direction and polarization separately
    for(pp in c('y','x')){
        for(bb in c(5,4,6,3,7,2,8,1,9,0)){
            LPI.KAIRA(
                dataDir=c(TX=TXpath,RX=datapath),
                resultDir=file.path(opath,odirs[bb+1],paste(pp,pp,sep='')),
                beamlets=list( RX=seq( bb*Nsubband , ((bb+1)*Nsubband-1) ) , TX=c() ),
                polarization=c(RX=pp,TX=''),
                filterLength.us=filtLen,
                lagLimits.us=llims,
                rangeLimits.km=rlims,
                maxRanges.km=maxr,
                timeRes.s=timeRes,
                backgroundEstimate=TRUE,
                beginTime=btime,
                endTime=etime,
                freqOffset.Hz=freqOffset,
                indexShifts.us=indexShifts,
                remoteRX=TRUE,
                ambInterp=FALSE,
                llhT=radarSites()$TRO,
                llhR=radarSites()$HBA,
                azelT=c(TXaz,TXele),
                azelR=c(RXax,RXele[bb+1]),
                radarFreq=radarFreq,
                rangeCoverage.deg=rangeCoverage,
                solver=solver,
                fakeTX=TRUE,
                KAIRAoffset.us=KAIRAoffset.us,
                maxWait.s=0,
                maxClutterRange.km=maxClutterRange.km,
                cl=cluster
            )
        }
    }
    
    # This function does not return anything,
    # results are written to files.
    invisible()

  }


