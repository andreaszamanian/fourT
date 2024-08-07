% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_agp.R
\name{compute_agp}
\alias{compute_agp}
\title{Compute AGP Thermometers}
\usage{
compute_agp(
  df_dex,
  start = "default",
  end = "default",
  inter = NULL,
  breaks = NULL,
  include_bv = T
)
}
\arguments{
\item{df_dex}{Data frame outputted by read_dexcom}

\item{start}{Date-time. Computation window start date. Value of "default" means
function ignores this parameter and computes up to the start of the data.}

\item{end}{Date-time. Computation window end date. Value of "default" means
function ignores this parameter at computes up to the start of the data.}

\item{inter}{Time interval for computation; e.g. every 14 days, every month, etc.}

\item{breaks}{Vector. Specify range computations manually, e.g. c(0, 1, 4.5, 7.5, 10.5, 13.5)
would report values at study months 0, 1, 4.5, 7.5, 10.5, and 13.5. EDIT, does not describe well.}

\item{include_bv}{Boolean. If TRUE, "High" and "Low" flags are converted to value 400 and 40
respectively and used in computations. If FALSE, this conversion does not happen;
computations ignore the "High" and "Low" flags}
}
\value{
Data frame with AGP metrics
}
\description{
Compute AGP Thermometers
}
