library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(roxygen2)
library(stats)
library(maps)

#'
#' Read FARS files
#'
#' This function read FARS data (Fatality Analysis Reporting System) from a CSV file and
#' returns a tibble.  If the path or file is incorrect, the fuction will stop with
#' an error.
#'
#' @param filename  A string of Path/filename that contains FARS data
#'
#' @return          A tibble of FARS data
#'
#' @export
#'
#' @importFrom     readr read_csv
#' @importFrom     dplyr tbl_df
#'
#' @examples       \dontrun{data <- fars_read("extdata/accident_2013.csv")}
#'
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' Make a filename
#'
#' This function will make a filename based on the input year, which requires a numerical input.
#'
#' @param year   A numeric value of the year for the data file.
#'
#' @return       A string of the file name of the input year
#'
#' @export
#'
#' @examples     \dontrun{make_filename(2013)}
#'
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Read FARS files by a list of years
#'
#' This function will take a list of years as input and read the corresponding FARS files of those years.
#'
#' @param years  a list of years
#'
#' @return       a data frame with 2 values: month and year. Return a warning message and NULL if the file does not exist.
#'
#' @export
#'
#' @importFrom   dplyr %>% mutate select
#'
#' @examples     \dontrun{fars_read_years(2013:2015)}
#'
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Print FARS Summary
#'
#' This function return a tabular report, which shows the number of accidents per month and year.
#'
#' @param years  a list of years
#'
#' @return       The summary of FARS info (data frame). Count the accidents per month and year.
#'
#' @export
#'
#' @importFrom   dplyr %>% bind_rows group_by summarize
#' @importFrom   tidyr spread
#'
#' @examples     \dontrun{fars_summarize_years(2013:2015)}
#'
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
        dplyr::group_by(year, MONTH) %>%
        dplyr::summarize(n = n()) %>%
        tidyr::spread(year, n)
}

#' Plot the FARS data on State map
#'
#' This function plot FARS data on a state map for a specific year.
#'
#' @param state.num  state-number of the state plotted on the map (A numeric value).
#' @param year       the year of FARS data plotted on the map.
#'
#' @return           State map with FARS data of a specific year.  If no data to plot, a message and NULL will return.
#'
#' @export
#'
#' @importFrom       dplyr filter
#' @importFrom       maps map
#' @importFrom       graphics points
#'
#' @examples         \dontrun{fars_map_state(47,2013)}
#'
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
