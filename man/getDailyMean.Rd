% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getDailyMean.R
\name{getDailyMean}
\alias{getDailyMean}
\title{Calculate Daily Mean}
\usage{
getDailyMean(x, id.col = "ID", time.col = "Time", ...)
}
\arguments{
\item{x}{a data frame or simple feature.}

\item{id.col}{character string of the name of the ID column}

\item{time.col}{character string of the name of the time column}
}
\value{
A data frame (or simple feature) with the daily mean locations, the day of year (yday), 
the year, and otherwise all continuous variables averaged, and all discrete data reduced to the 
first observation. A data frame will return a data frame, a simple feature will return a simple 
feature conserving the original projection.
}
\description{
This function takes any large movement dataset and return the daily mean locations 
for each individual for each day for each year of observations.
}
\details{
Every continuous variable (X, Y, Lon, Lat, Time) will be averaged for each day.  Every factor 
or character vector will be reduced to a single (first) observation.
}
\examples{
require(TuktuTools)

data(caribou)

caribou_dailymean <- getDailyMean(caribou)
head(caribou_dailymean)
}
