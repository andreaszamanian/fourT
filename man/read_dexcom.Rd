% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_dexcom.R
\name{read_dexcom}
\alias{read_dexcom}
\title{Reads Dexcom data}
\usage{
read_dexcom(x, col_spec = FALSE)
}
\arguments{
\item{x}{A file (maybe filepath).}
}
\value{
A data frame.
}
\description{
Reads .csv file, deidentifies the data (removes PHI), reformats
}
