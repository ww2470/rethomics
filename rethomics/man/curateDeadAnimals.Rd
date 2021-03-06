% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sleep.R
\name{curateDeadAnimals}
\alias{curateDeadAnimals}
\title{Finds when an animal is `dead' and removes the all consecutive data}
\usage{
curateDeadAnimals(data, max_immobile_live = hours(12))
}
\arguments{
\item{data}{the data (i.e a data.table) from a \emph{single} region. It must contain, at least,
the columns \code{t}and \code{moving}.}

\item{max_immobile_live}{the longest duration an alive animal can remain immobile before being considered dead.}
}
\value{
A data table similar to \code{data} where late time points have potentially been removed
}
\description{
In this context, death is defined by very long periods of immobility.
}
\note{
Death is assumed to be irreversible. Therefore, if an animal is classified as dead, all subsequent data is is removed.
}
\examples{
# Let us load some sample data
data(dam_data)
dt <- dam_data[,
            sleepDAMAnnotation(.SD),
            by=key(dam_data)]
# let us have a look at the pattern of movement.
# Some animals (e.g. 06, 21, 24) died early.
overviewPlot(moving,dt,normalise_var_per_id = FALSE)
dt_curated <- dt[,curateDeadAnimals(.SD,hours(15)),by=key(dt)]
# Note that some data has been removed.
# Also, no data was there for region_id == 06, therefore, it is removed altogether
overviewPlot(moving, dt_curated, normalise_var_per_id = FALSE)
#####
# A simple way to compute total lifespan of each remaining animal:
lifespan_dt <- dt_curated[,
        .(lifespan = max(t) - min(t))
        ,by=key(dt_curated)]
}
\seealso{
\code{\link{sleepAnnotation}} and \code{\link{sleepDAMAnnotation}} to define movement and add a \code{moving} column.
}
