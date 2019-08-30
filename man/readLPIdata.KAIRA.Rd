\name{readLPIdata.KAIRA}
\title{readLPIdata.KAIRA}
\alias{readLPIdata.KAIRA}
\description{Read one integration period of voltage level
  data. Transmitter samples are recorded with USRP at Tromso and
  receiver samples with KLP at Kilpisjarvi.}
\usage{readLPIdata.KAIRA( LPIparam , intPeriod )}

\arguments{
  \item{ LPIparam }{ An LPI parameter list from \link{LPI.KAIRA} }
  \item{ intPeriod }{ Integration period number. Integration periods are
    counted in steps of LPIparam$timeres.s, the period number 1 starting
    at LPIparam$startTime.}
  }

  \value{
    A list with the following contents
    \item{'RX1'}{ First receiver samples }
    \item{'RX2'}{ Second receiver samples. Will be identical with 'RX1'
      in autocovariance function estimation}
    \item{'TX1'}{ First transmitter samples}
    \item{'TX2'}{ Second transmitter samples. Will usually be identical
      with 'TX1', but may be different e.g. in orthogonal polarization
      experiments.}
    \item{success}{ TRUE if all requested data was successfully read,
      FALSE otherwise.}

    The elements "RX1", "RX2", "TX1", and "TX2" are lists
    themselves with elements

    \item{'cdata'}{ Complex data vector.}
    \item{'idata'}{ Logical vector, TRUE for samples that should be used
      in LPI.}
    \item{'ndata'}{ Number of samples in the data vectors.}
  }

  \details{
    \describe{
      \item{'LPIparam contents'}{ Following components of the LPI parameter
	list are used for selecting the correct signal samples
	\describe{
	  \item{'startTime'}{'beginTime' converted into POSIX format,
	    i.e. second count from 1970-01-01 00:00:00.
	  }
	  \item{'dataStartTimes'}{A named vector with components 'RX1',
	    'RX2', 'TX1', and 'TX2'. Each element is samling time of the
	    first sample of corresponding data type. The times are in
	    seconds in POSIX format. }
	  \item{'dataSampleFreqs'}{ A named vector with components 'RX1',
	    'RX2', 'TX1', and 'TX2'. Each element is the sample rate of the
	    corresponding data type in Hz.}
	  \item{'timeRes.s'}{ Analysis time resolution (incoherent
	    integration period) in seconds. }
	  \item{'dataFileLengths'}{A named vector with components 'RX1',
	    'RX2', 'TX1', and 'TX2'. Each element is the number of complex
	    samples in one data file of the corresponding data type. }
	  \item{'fileNamePrefix'}{A named vector with components 'RX1',
	    'RX2', 'TX1', and 'TX2'. Each element contains the file name
	    prefix of the corresponding data type as a string. }
	}
      }
    }
  }


\seealso{LPI.gdf , LPI}


  \author{Ilkka Virtanen (University of Oulu, Finland) \cr
  \email{ilkka.i.virtanen@oulu.fi}}
