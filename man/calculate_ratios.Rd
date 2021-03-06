% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculations.R
\name{calculate_ratios}
\alias{calculate_ratios}
\title{Calculate isotope ratios}
\usage{
calculate_ratios(data, ..., name_fun = default_name, quiet = FALSE)
}
\arguments{
\item{data}{a data frame with raw ion counts retrieved from \code{\link{load_LANS_summary}}}

\item{...}{the ratios to calculate, each entry is one ratio with major isotope first, then
minor isotope, e.g. c(\verb{13C}, \verb{12C}), c(\verb{15N12C}, \verb{14C12C}), ...}

\item{name_fun}{the naming function, receives ... from the top level, default concatenates column names with '/'}

\item{quiet}{whether the function should output information messages or be quiet (default is to output)}
}
\value{
the original data frame with the ratio information appended (all ratios have data_type == "ratio")
}
\description{
This function calculates the ratios and resulting counting
statistics error from the raw ion counts. It can be applied to data from both
LANS_summary and LANS_maps loading but can be slow if LANS_maps is combined
from many analyses. It can also be applied to ion_sums generate by calculate_sums
to calculate elemental ratios (careful, ionization efficiencies skew their scaling!)
}
\seealso{
Other calculations: 
\code{\link{calculate_abundances}()},
\code{\link{calculate_sums}()},
\code{\link{calculate}()}
}
\concept{calculations}
