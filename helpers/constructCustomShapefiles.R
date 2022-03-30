library(dplyr)
library(sf)
library(tmap)

LAUs = read_sf("rawShapeFiles/Local_Authority_Districts_(December_2021)_GB_BFE/LAD_DEC_2021_GB_BFE.shp")

cheshireRegions = c("Cheshire East",
                    "Cheshire West and Chester",
                    "Wirral")

cheshire = LAUs %>% filter(LAD21NM %in% cheshireRegions) %>% st_union()

write_sf(cheshire,"refData/customRegionShapefiles/cheshire.shp")

