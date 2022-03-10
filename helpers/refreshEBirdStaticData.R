# NB: Note that this file requires an eBird API key to be stored in the .Renviron file
# See https://docs.ropensci.org/rebird for more details.

library(rebird)
library(readr)
library(purrr)

countriesToRetrieve = c("GB","IM")
hotspots = map_dfr(countriesToRetrieve,ebirdhotspotlist)

write_csv(hotspots,"refData/GBHotspots.csv")

regions = map_dfr(countriesToRetrieve,~ebirdsubregionlist("subnational2",.))

write_csv(regions,"refData/eBirdRegions.csv")
