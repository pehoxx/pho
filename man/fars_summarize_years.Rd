% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Print FARS Summary}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{a list of years}
}
\value{
The summary of FARS info (data frame). Count the accidents per month and year.
}
\description{
This function return a tabular report, which shows the number of accidents per month and year.
}
\examples{
    \dontrun{fars_summarize_years(2013:2015)}

}
