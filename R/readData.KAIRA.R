readData.KAIRA <- function( dataDir , nBeamlets, beamlets , polarization , files , n=390625 , istart=1 , bigEndian=FALSE , controlBits=FALSE)
    {
        #
        # Read voltage level data from gdf files and combine KAIRA beamlets 
        # The beamlets must be from consecutive subbands 
        #
        #
        #
        # I. Virtanen 2013
        #
        #

        # repeat the index vectors as necessary
        n <- rep( rep( n , length.out=length(files) ) , max( nBeamlets , 1 ) )
        istart <- rep( rep( istart , length.out=length(files) ) , max( nBeamlets , 1 ) )

        # full data file paths
        if( nBeamlets==0){
            files <- file.path( rep( file.path( dataDir , polarization ) , each=length(files) ) , files )
        }else{
            files <- file.path( rep( file.path( dataDir , gsub(' ','0',format(beamlets,width=3)) , polarization ) , each=length(files) ) , files )
        }

        # read data from all beamlets in a vector
        rawdata <- readData.gdf( files=files , n=n , istart=istart , bigEndian=bigEndian , controlBits=controlBits )

        # then do frequency mixing for each beamlet as necessary
        if( nBeamlets > 1 ){
            rawdata[["cdata"]]  <- rep( rawdata[["cdata"]] , each=nBeamlets )
            rawdata[["idatai"]] <- rep( rawdata[["idatai"]] , each=nBeamlets )
            rawdata[["idatar"]] <- rep( rawdata[["idatar"]] , each=nBeamlets )
            rawdata[["ndata"]]  <- nd <- sum( n )
            for( k in seq(2,nBeamlets)){
                # must mix upwards, because the higher frequencies have been mixed to baseband
                rawdata[["cdata"]][1:nd] <- rawdata[["cdata"]][1:nd] + LPI:::mixFrequency( cdata=rawdata[["cdata"]][((k-1)*nd+1):(k*nd)] , ndata=as.integer(nd) , frequency=as.numeric((k-1)/nBeamlets))[["cdata"]]
            }
        }

        storage.mode(rawdata[["ndata"]]) <- 'integer'

        rawdata[["cdata"]] <- rawdata[["cdata"]][1:rawdata[["ndata"]]]
        rawdata[["idatai"]] <- rawdata[["idatai"]][1:rawdata[["ndata"]]]
        rawdata[["idatar"]] <- rawdata[["idatar"]][1:rawdata[["ndata"]]]

        return(rawdata)

    }
