## file:readLPIdata,spherical.KAIRA.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Read one integration period of raw voltage data for
## lag profile inversion from gdf files.
##
## KAIRA special: TX data is re-used and RX data is always
##                little-endian.
##                A spherical polarization is formed from the
##                linear x and y polarization signals for test
##                purposes. 
##
##
## Arguments:
##  LPIparam     LPI parameter list
##  intPeriod    Integration period number
##
## Returns:
##  LPIrawData  A named list with elements
##              "RX1", "RX2", "TX1", "TX2", and "success".
##              Each of the first four elements is
##                a list with elements
##                'cdata'  complex data vector
##                'idata'  logical vector of usable samples
##                'ndata'  vector length
##              "success" is TRUE if all data was read
##

readLPIdata.spherical.KAIRA <- function( LPIparam , intPeriod)
  {

    LPIfilelist     <- listLPIfiles.KAIRA( intPeriod , LPIparam )
    datalist        <- vector( mode="list" , length=5 )
    k               <- 1
    dTypes          <- c( "RX1" , "RX2" , "TX1" , "TX2" )
    names(datalist) <- c( dTypes , "success" )

    # Check data endiannes, default bigEndian=TRUE
    if(is.null(LPIparam[["bigEndian"]])) LPIparam[["bigEndian"]] <- TRUE
    LPIparam[["bigEndian"]] <- LPIexpand.input( LPIparam[["bigEndian"]] )
    
    # KAIRA data is little-endian
    LPIparam[["bigEndian"]][c("RX1","RX2")] <- FALSE

    # KAIRA data does not have control bits, but VHF data does
    if(is.null(LPIparam[["controlBits"]])) LPIparam[["controlBits"]] <- TRUE
    LPIparam[["controlBits"]] <- LPIexpand.input(LPIparam[["controlBits"]])
    LPIparam[["controlBits"]][c("RX1","RX2")] <- FALSE
    
    for( XXN in dTypes ){

      # If any of the file lists is empty, just set success to FALSE and return
      if( length(LPIfilelist[["files"]][[XXN]] ) == 0 ){
        datalist[["success"]]  <- FALSE
        return( datalist )
      }
      if( any( XXN == c( "RX1" , "RX2" ) ) ){
        datalist[[XXN]] <- readData.KAIRA( dataDir=LPIparam[["dataDir"]][XXN] , nBeamlets=LPIparam[["nBeamlets"]][XXN], beamlets=LPIparam[["beamlets"]][[XXN]] , polarization="x" , files  = LPIfilelist[["files"]][[XXN]] , n  = LPIfilelist[["n"]][[XXN]] , istart = LPIfilelist[["starts"]][[XXN]] , bigEndian = LPIparam[["bigEndian"]][XXN] ,controlBits=LPIparam[["controlBits"]][XXN] )
        datalist[[XXN]][["cdata"]] <- datalist[[XXN]][["cdata"]] + LPIparam[["handedness"]] * 1i * readData.KAIRA( dataDir=LPIparam[["dataDir"]][XXN] , nBeamlets=LPIparam[["nBeamlets"]][XXN], beamlets=LPIparam[["beamlets"]][[XXN]] , polarization="y" , files  = LPIfilelist[["files"]][[XXN]] , n  = LPIfilelist[["n"]][[XXN]] , istart = LPIfilelist[["starts"]][[XXN]] , bigEndian = LPIparam[["bigEndian"]][XXN] ,controlBits=LPIparam[["controlBits"]][XXN] )[["cdata"]]
      }else{
        datalist[[XXN]] <- readData.KAIRA( dataDir=LPIparam[["dataDir"]][XXN] , nBeamlets=LPIparam[["nBeamlets"]][XXN], beamlets=LPIparam[["beamlets"]][[XXN]] , polarization=LPIparam[["polarization"]][XXN] , files  = LPIfilelist[["files"]][[XXN]] , n  = LPIfilelist[["n"]][[XXN]] , istart = LPIfilelist[["starts"]][[XXN]] , bigEndian = LPIparam[["bigEndian"]][XXN] ,controlBits=LPIparam[["controlBits"]][XXN] )
    }
    }

    # Create proper index vectors
    datalist[["RX1"]][["idata"]]   <- !datalist[["RX1"]][["idatai"]]
    datalist[["RX2"]][["idata"]]   <- !datalist[["RX2"]][["idatai"]]
    datalist[["TX1"]][["idata"]]   <-  datalist[["TX1"]][["idatai"]]
    datalist[["TX2"]][["idata"]]   <-  datalist[["TX2"]][["idatai"]]


    # KAIRA special, we do not have (nor need)
    # TX bits on RX data
    datalist[["RX1"]][["idata"]][] <- TRUE
    datalist[["RX2"]][["idata"]][] <- TRUE

    # Remove extra idata vectors
    datalist[["RX1"]][["idatai"]]  <- NULL
    datalist[["RX1"]][["idatar"]]  <- NULL
    datalist[["RX2"]][["idatai"]]  <- NULL
    datalist[["RX2"]][["idatar"]]  <- NULL
    datalist[["TX1"]][["idatai"]]  <- NULL
    datalist[["TX1"]][["idatar"]]  <- NULL
    datalist[["TX2"]][["idatai"]]  <- NULL
    datalist[["TX2"]][["idatar"]]  <- NULL

    # Create a combined success variable which is TRUE
    # only if all reads were successfull
    datalist[["success"]]          <- datalist[["RX1"]][["success"]] & datalist[["RX2"]][["success"]] & datalist[["TX1"]][["success"]] & datalist[["TX2"]][["success"]]

    return(datalist)
  
  }


