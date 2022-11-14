# Mothers and babies data from 1995 - 2013
library(data.table)
library(magrittr)

## import data
qld_births <- read.csv("data-raw/qld_health_mothers-and-babies-1995-2013.csv")

## tidy up the field names
qld_births <- setNames(
  qld_births,
  c("id", "year", "health_service", "facility_id", "facility_name", "mothers", "babies"))

## turn the data into a tidy dataset
qld_births <- data.table::melt(
  data = data.table::setDT(qld_births),
  id.vars = c("id", "year", "health_service", "facility_id", "facility_name"),
  variable.name = "type",
  value.name = "count"
)  %>%
as.data.frame()

usethis::use_data(qld_births, internal = TRUE, overwrite = TRUE)
