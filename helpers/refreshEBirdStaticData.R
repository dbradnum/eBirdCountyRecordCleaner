# NB: Note that this file requires an eBird API key to be stored in the .Renviron file
# See https://docs.ropensci.org/rebird for more details.

library(rebird)
library(readr)

hotspots = ebirdhotspotlist("GB")

write_csv(hotspots,"refData/GBHotspots.csv")

regions = ebirdsubregionlist("subnational2","GB")

write_csv(regions,"refData/eBirdRegions.csv")
