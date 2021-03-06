% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pairwise_metafor.R
\name{pairwise_metafor}
\alias{pairwise_metafor}
\title{Restruscture data in contrast based format and run pairwise meta-analysis model results using metafor package}
\usage{
pairwise_metafor(armbased = TRUE, treat, event, n, mean, sd, TE, seTE, time,
data = NULL, studlab = NULL, incr = 0.5, allincr = FALSE, addincr = FALSE, 
allstudies = FALSE, nupdate = 1, nobs = NULL, ...)
}
\arguments{
\item{armbased}{A logical indicating if the data are in arm-based format (one row for each study, and one treatment in each column )
if FALSE the data are already in contrast based format.}

\item{treat}{A list or vector with treatment information for individual treatment arms (see Details in netmeta).}

\item{event}{A list or vector with information on number of events for individual treatment arms (see Details in netmeta pairwise function).}

\item{n}{A list or vector with information on number of observations for individual treatment arms (see Details in netmeta pairwise function).}

\item{mean}{A list or vector with estimated means for individual treatment arms (see Details in netmeta pairwise function).}

\item{sd}{A list or vector with information on the standard deviation for individual treatment arms (see Details in netmenta pairwise function).}

\item{TE}{A list or vector with estimated treatment effects for individual treatment arms (see Details in netmta pairwise function).}

\item{seTE}{A list or vector with standard errors of estimated treatment effect for individual treatment arms (see Details in netmta pairwise function).}

\item{time}{A list or vector with information on person time at risk for individual treatment arms (see Details in netmeta pairwise function).}

\item{data}{Data frame in contrast-based or arm-based format}

\item{studlab}{A vector with study labels (optional).}

\item{incr}{A numerical value which is added to each cell frequency for studies with a zero cell count.}

\item{allincr}{A logical indicating if incr is added to each cell frequency of all studies if at least one study has a zero cell count. If FALSE (default), incr is added only to each cell frequency of studies with a zero cell count.}

\item{addincr}{A logical indicating if incr is added to each cell frequency of all studies irrespective of zero cell counts.}

\item{allstudies}{A logical indicating if studies with zero or all events in two treatment arms are to be included in the meta-analysis (applies only if sm is equal to "RR" or "OR").}

\item{nupdate}{number of data updates}

\item{nobs}{a vector with the number of observations for each update}

\item{...}{optional argument to functions, you can include any parameter to run rma function from metafor pkg}
}
\value{
returns to .Rdata one with tde data set in contrast-based format and the second is a list with the pairwise meta analysis for each update and each pair of treatments
}
\description{
Restruscture data in contrast based format and run pairwise meta-analysis model results using metafor package
}
\examples{
\dontrun{
                
modstrMTCdata <- pairwise_metafor(armbased = TRUE, treat = list(treat1, treat2, treat3),
  event = list(event1, event2, event3), n = list(n1, n2, n3), data = MTCdata, nupdate = 2,
  nobs = c(109, 5), measure = "RR")
  
modstrMTCpairsrg <- pairwise_metafor(armbased = TRUE, treat = list(t1, t2, t3, t4),
                TE = list(y1, y2, y3,y4),
               seTE = list(se1, se2, se3, se4),
                data = dat_rungano,
                sm = "MD", nupdate = 1,  nobs = 20)
                
 }
}
