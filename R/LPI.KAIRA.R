## file:LPI.KAIRA.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.

##
## A wrapper to the function LPI::LPI
##
## Arguments:
##   ... Arguments to be passed for collectLPIparam.gdf that
##       constructs the final LPI parameter list
##
## 

LPI.KAIRA <- function( ... )
  {
    # Collect the LPI parameter list from default values
    # and optional input arguments.
    LPIparam  <- collectLPIparam.KAIRA( ... )

    # Call the main analysis loop of LPI
    do.call( LPI , LPIparam )

    # This function does not return anything,
    # results are written to files.
    invisible()

  }

