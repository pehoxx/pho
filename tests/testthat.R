library(testthat)
library(pho)

setwd("C:/Users/peterh/Documents/pho/inst/extdata")

#1 fars_summarize_years()
expect_that(fars_summarize_years(2013), is_a("data.frame"))

#2 fars_map_state(1,2014)
expect_that(fars_map_state(48,2013), is_a("NULL"))

