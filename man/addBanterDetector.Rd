% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addBanterDetector.R
\name{addBanterDetector}
\alias{addBanterDetector}
\alias{removeBanterDetector}
\title{Add a BANTER Detector Model}
\usage{
addBanterDetector(
  x,
  data,
  name,
  ntree,
  sampsize = 1,
  importance = FALSE,
  num.cores = 1
)

removeBanterDetector(x, name)
}
\arguments{
\item{x}{a \code{\link{banter_model}} object.}

\item{data}{detector data.frame or named list of detector data.frames. If 
a data.frame, then \code{name} must be provided.}

\item{name}{detector name.}

\item{ntree}{number of trees.}

\item{sampsize}{number or fraction of samples to use in each tree. If < 1, 
then it will be used to select this fraction of the smallest sample size.}

\item{importance}{retain importance scores in model? Defaults to 
\code{FALSE} and will be ignored if \code{num.cores > 1}.}

\item{num.cores}{number of cores to use for Random Forest model. Set to 
\code{NULL} to use the maximum number detected on your system - 1.}
}
\value{
a \code{\link{banter_model}} object with the detector model added or 
  removed.
}
\description{
Add a detector model to a BANTER classifier.
}
\examples{
data(train.data)
# initialize BANTER model with event data
bant.mdl <- initBanterModel(train.data$events)
# add the 'bp' (burst pulse) detector model
bant.mdl <- addBanterDetector(
  x = bant.mdl, 
  data = train.data$detectors$bp, 
  name = "bp",
  ntree = 50, sampsize = 1, num.cores = 1
)
bant.mdl

# remove the 'bp' detector model
bant.mdl <- removeBanterDetector(bant.mdl, "bp")
bant.mdl

}
\references{
Rankin, S., Archer, F., Keating, J. L., Oswald, J. N., 
  Oswald, M. , Curtis, A. and Barlow, J. (2017), Acoustic classification 
  of dolphins in the California Current using whistles, echolocation clicks, 
  and burst pulses. Marine Mammal Science 33:520-540. doi:10.1111/mms.12381
}
\author{
Eric Archer \email{eric.archer@noaa.gov}
}
