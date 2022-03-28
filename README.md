# eBirdCountyRecordCleaner
R Shiny app to clean and enrich UK eBird data for onward use in local bird reports   

## Wish list for enhancements

### General / Usability
- [ ] Populate instructions tab
- [ ] Add error handling for invalid input data
- [ ] Option to upload observer file in zipped format, to speed things up. (How do eBird Central share this?)

### Filtering records
- [x] Include "Reviewed" column, indicating whether data has passed through local filter
- [ ] Match to list of BBRC species, to output an additional column. (Nearly complete - need to deal with anomalies e.g. Jackdaw, where taxonomy doesn't join cleanly)
- [x] Extend BBRC lookup to also indicate BBRC subspecies
- [ ] Match to eBird barchart data (ie species frequency per week)
- [x] Include eBird taxon type in output (ie species/spuh/hybrid/etc)


### Locations
- [x] Include OS 1km square, as well as full granular detail
- [x] Move nearest hotspot columns to sit next to other location info
- [ ] Option to filter results by shapefile boundary (either from pre-specified list - Steve to share - or upload)
- [ ] Option to filter results by custom boundary (eg rectangular bounding box or custom drawn? Check out mapedit package...)
- [ ] Option to join to user-specified location name mapping
- [ ] Explore possibility of reverse-geocoding from lat-long to nearest named location. Is there a viable free option?


### Summarising data
- [ ] Option to generate summary data (as an alternative or additional output) - e.g. max count per species, per location, per day to reduce duplication.
