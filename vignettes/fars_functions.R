## ------------------------------------------------------------------------
library(pho)
setwd("~/pho/inst/extdata")
fars_summarize_years(2013)

## ---- fig.show='hold'----------------------------------------------------
library(maps)
setwd("~/pho/inst/extdata")
fars_map_state(48,2013)

