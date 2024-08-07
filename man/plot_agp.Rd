% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_agp.R
\name{plot_agp}
\alias{plot_agp}
\title{Canonical bar plot for AGP Thermometers}
\usage{
plot_agp(df_dex, start = "default", end = "default", inter)
}
\arguments{
\item{df_dex}{Data frame outputted by read_dexcom}

\item{start}{Date-time. Computation window start date. Value of "default" means
function ignores this parameter at computes up to the start of the data.}

\item{end}{Date-time. Computation window end date. Value of "default" means
function ignores this parameter at computes up to the start of the data.}

\item{inter}{time interval}
}
\description{
Canonical bar plot for AGP Thermometers
}
