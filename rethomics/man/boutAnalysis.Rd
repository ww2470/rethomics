% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sleep.R
\name{boutAnalysis}
\alias{boutAnalysis}
\title{Finds `bouts' in categorical time series.}
\usage{
boutAnalysis(var, data)
}
\arguments{
\item{var}{the column variable to use in \code{data}}

\item{data}{a data.table}
}
\value{
A data.table with columns for the unique value of the bout variable, bout start time, and bout length (ie. duration). 
Bout analysis will be performed by individual (data.table key), which adds additional columns. Their is one row for each bout.
}
\description{
This function is used to find contiguous regions of unique value in a -- potentially irregular -- univariate time series.
}
\examples{
set.seed(1)
# 1000 points the first 500 points should have higher chance to be 1 than the last 500:
y_var <- round(c(runif(500,0,1),
                   runif(500,0,0.75)))
# first 500 point are for individual "A", next 500 points are for "B":
dt <- data.table( y = y_var,
             t = rep(1:500,2)*12,
             id = rep(c("A","B"),each=500),key="id")
bout_dt <- boutAnalysis(y,dt)
summary <- bout_dt[,
        .(n=.N,
          mean_duration=mean(length))
        ,by=c(key(bout_dt),"y")] 
print(summary)
}
\seealso{
\code{rle} to perform a run length transform manually
}
