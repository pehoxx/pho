% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read}
\alias{fars_read}
\title{Read FARS files}
\usage{
fars_read(filename)
}
\arguments{
\item{filename}{A string of Path/filename that contains FARS data}
}
\value{
A tibble of FARS data
}
\description{
This function read FARS data (Fatality Analysis Reporting System) from a CSV file and
returns a tibble.  If the path or file is incorrect, the fuction will stop with
an error.
}
\examples{
      \dontrun{data <- fars_read("extdata/accident_2013.csv")}

}
