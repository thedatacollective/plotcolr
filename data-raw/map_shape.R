library(sf)

## import map shape
sf_locality <- read_sf("data-raw/1270055003_lga_2020_aust_shp/LGA_2020_AUST.shp")

## filter map shape to Queensland
sf_qld <- sf_locality[sf_locality$STE_NAME16 == "Queensland", ]

usethis::use_data(sf_qld, internal = TRUE, overwrite = TRUE, compress = "xz")
