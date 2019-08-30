## file:rangeGateSelect.KAIRA.R
## (c) 2010- University of Oulu, Finland
## Written by Ilkka Virtanen <ilkka.i.virtanen@oulu.fi>
## Licensed under FreeBSD license.
##

##
## Strip off range gates that are outside the beam intersection
##
##

rangeGateSelect.KAIRA <-  function( rangeLimits.km , llhT, azelT , llhR , azelR , phi=2 )
    {

        # elevations to the beam intersections
        n <- length( rangeLimits.km )
        ele <- rep( NA , n )
        for( k in seq(n) ){
            ele[k] <- llhTarget2azelrBeam(
                llhTarget=range2llh( r = rangeLimits.km[k]*1000, llhT = llhT, azelT = azelT, llhR = llhR ),
                llhSite = llhR
                )["el"]
        }

        elediff <-  ele - azelR[2]
        absdiff <- abs( elediff )

        gateinds <- absdiff <= phi

        if(!any(gateinds)){
            gind <- which( absdiff == min( absdiff ) )
            gateinds[ gind ] <- TRUE
            gateinds[ gind + sign( elediff[ gind ] ) ] <- TRUE
        }else{
            iind <- which(gateinds)
            gateinds[ max(1,(iind[1]-1))] <- TRUE
            gateinds[ min(n,(max(iind[1])+1))] <- TRUE
        }

        return( rangeLimits.km[gateinds] )
        
        
    }
        
