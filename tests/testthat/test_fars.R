context("Summarize FARS data")
setwd("~/pho/inst/extdata")
test_that("the summarized data is a tbl_df for a year", {
    dat2013 <- fars_summarize_years(2013)
    expect_that(dat2013, is_a("tbl_df"))
})
test_that("the summarized data is a tbl_df for multiple years", {
    dat2013to2015 <- fars_summarize_years(2013:2015)
    expect_that(dat2013to2015, is_a("tbl_df"))
})
