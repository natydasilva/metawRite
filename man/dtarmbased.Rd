% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dtarmbased.R
\name{dtarmbased}
\alias{dtarmbased}
\title{Helper function to restructure the data in arm-based format if is not}
\usage{
dtarmbased(data, study,arms )
}
\arguments{
\item{data}{Data frame with the treatment information in one column.}

\item{study}{Name of the column that contain the study information.}

\item{arms}{Name of the column that contain arm information.}
}
\value{
returns a data frame with arm-based format (one row for each study, and one treatment in each column)
}
\description{
Helper function to restructure the data in arm-based format if is not
}
\examples{
 \dontrun{
data(dat.begg1989)
dtarmbased(dat.begg1989, study,arms )
}
}
