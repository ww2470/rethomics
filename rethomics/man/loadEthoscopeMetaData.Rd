% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ethoscope_io.R
\name{loadEthoscopeMetaData}
\alias{loadEthoscopeMetaData}
\title{Retrieves  metadata from a result file.}
\usage{
loadEthoscopeMetaData(FILE)
}
\arguments{
\item{FILE}{the name of the input file.}
}
\value{
A list containing fields for metadata entries
}
\description{
This function is used to obtain metadata -- such as `time and date of the experiment' , `acquisition device', `version of the software' and such--
embedded in a result file generated by the ethoscope platform.
}
\examples{
FILE <- "ethoscope/014/E_014/2016-01-25_21-36-04/2016-01-25_21-36-04_014.db"
path <- getSampleDataPath(FILE)
out <- loadEthoscopeMetaData(path)
names(out)
}
\seealso{
\code{\link{loadEthoscopeData}} to load raw data.
}
