## file:LPIparam.default.KAIRA.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## Default LPI analysis parameters.
##
## Arguments:
##  None
##
## Returns:
##  LPIparam A named list of parameters
##

LPIparam.default.KAIRA <- function()
  {
    return(
      list(
        # Cluster definition
        clusterNodes       = list(tesla1=8,tesla2=8,tesla3=8,tesla4=8,tesla5=8,localControl=TRUE),
        # Receiver filter length in us
        filterLength.us    = 10,
        # Voltage level decoding filter.
        decodingFilter     = c("none","matched","inverse"),
        # Time-lag limits in us
        lagLimits.us       = seq(50)*10,
        # Range-gate limits in km
        rangeLimits.km     = c(seq(50,149.9,by=.9),seq(154.4,298.4,by=4.5),seq(343.4,793.5,by=45)),
        # Maximum range for each time-lag, will be
        # padded with the last value as necessary
        maxRanges.km       = Inf,
        # Time resolution (integration time) in s
        timeRes.s          = 10,
        # Maximum clutter suppression range, use negative
        # value to guarantee that suppression is not used
        maxClutterRange.km = 60,
        # Ground clutter decoding integration time,
        # fraction of the full integration period
        clutterFraction    = 1,
        # Background noise autocovariance function estimation
        backgroundEstimate = TRUE,
        # Analysis start time
        beginTime          = c(1970,1,1,0,0,0),
        # Analysis end time
        endTime            = c(3000,1,1,0,0,0),
        # Maximum time to wait for new data 3
        # before stopping the analysis [s]
        maxWait.s          = -1,
        # Frequency offset(s) from baseband
        freqOffset.Hz      = 0,
        # Adjustments to TX / RX indices
        # (written in this way to remind that shifts for
        # TX and RX are most probably different)
        indexShifts.us     = list( TX=c(0,0) , RX=c(0,0) ),
        # Invserse problem solver
        # "rlips", "fishs", "dummy", "deco", or "ffts"
        solver             = "fishs",
        # Number of theory rows to produce
        # before calling the solver
        nBuf               = 10000,
        # Calculate full posterior covariance matrix
        # (set FALSE to calculate only the variances)
        fullCovar          = FALSE,
        # Options to rlips
        rlips.options      = list( type="c" , nbuf=1000 , workgroup.size=128),
        # Is the receiver data from a remote site?
        remoteRX           = TRUE,
        # Should the TX samples be normalised to common
        # amplitude after decimation? Setting this may
        # improve the first lag in matched-filter decoding
        normTX             = FALSE,
        # Number of pulses in a full modulation cycle. Used
        # only for lag-profile pre-averaging, set to NA or
        # negative value to disable the pre-averaging
        nCode              = NA,
        # Should linear interpolation be applied when
        # calculating the range-ambiguity functions
        # (setting to FALSE will work only if the receiver
        # filter is short compared to baud length,
        # or if strong codes are used)
        ambInterp          = FALSE,



        # Functions for data input. Tthese functions
        # will need to be replaced in order to apply
        # new input formats. The replacement functions
        # must be included in a package listed in
        # requiredPackages, and exported in the
        # packages NAMESPACE file.

        # Additional package(s) needed for data input,
        # LPI and parallel are always loaded
        inputPackages      = c("LPI.KAIRA"),
        # Additional functions to call immediately after
        # the first parameter collection step with the LPI
        # parameter list as argument. These must
        # return lists, which will be concatenated to the
        # LPI parameter list.
        extraFunctions     = c( "LPIexpandInput.gdf" , "getFileLengths.KAIRA" ) ,
        # Function for reading full integration
        # periods of raw data
        dataInputFunction  = "readLPIdata.KAIRA",
        # Function for reading sampling start times
        startTimeFunction  = "getSamplingStartTimes.KAIRA",
        # Function for reading sampling end times
        dataEndTimeFunction    = "getDataEndTimes.KAIRA",
        # function for readind data sample rates
        sampleRateFunction = "getSamplingFrequencies.KAIRA",

        # Additional input that the gdf input routines require
        # Raw data directory(ies)
        dataDir            = '.',
        # KAIRA beamlet numbers, these must be consecutive subbands!
        beamlets         = list(RX1=c(0),RX2=c(0),TX1=c(),TX2=c()),
        # polarization x/y
        polarization     = c(RX1='y',RX2='y',TX1='',TX2=''),
        # File name prefix(es)
        fileNamePrefix     = "data-",
        # File name extension(s)
        fileNameExtension  = ".gdf",


        # Output directory, set NA to prevent creation of a directory
        resultDir          = paste(format(Sys.time(),"%Y-%m-%d_%H:%M"),'LP',sep='_'),

        # Function for storing the resolved ACFs
        resultSaveFunction = 'LPIsaveACF.gdf',

        # the spectra do not need to be mirrored
        mirrorSpectra = FALSE,

        # Site location information
        llhT = c( 69.58646, 19.22743,  84.47003 ),
        llhR = c( 69.07106, 20.76103, 525.33570 ),
        azelT = c( 0 , 90 ),
        azelR = c( 313.95 , 45 ),
        radarFreq = 224e6,

        # range coverage at either side of the beam intersection in degrees
        rangeCoverage.deg = 2,

        # do we have a fake TX signal with exactly one EISCAT integration period per file?
        fakeTX = FALSE,

        # additional offset to KAIRA timestamps
        KAIRAoffset.us = -38.4

        )
      )
  }

