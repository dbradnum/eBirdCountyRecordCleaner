

get_header <- function(x, sep = "\t") {
  readLines(x, n = 1) %>%
    # str_trim(side = "right") %>% 
    stringr::str_split(sep) %>%
    `[[`(1) %>%
    trimws()
}

get_col_types <- function(header) {
  # column types based on feb 2017 ebd
  col_types <- c(
    "GLOBAL UNIQUE IDENTIFIER" = "character",
    "LAST EDITED DATE" = "character",
    "TAXONOMIC ORDER" = "numeric",
    "CATEGORY" = "factor",
    "COMMON NAME" = "character",
    "SCIENTIFIC NAME" = "character",
    "SUBSPECIES COMMON NAME" = "character",
    "SUBSPECIES SCIENTIFIC NAME" = "character",
    "OBSERVATION COUNT" = "character",
    "BREEDING BIRD ATLAS CODE" = "character",
    "BREEDING BIRD ATLAS CATEGORY" = "character",
    "AGE/SEX" = "character",
    "COUNTRY" = "factor",
    "COUNTRY CODE" = "factor",
    "STATE" = "factor",
    "STATE CODE" = "factor",
    "COUNTY" = "factor",
    "COUNTY CODE" = "factor",
    "IBA CODE" = "character",
    "BCR CODE" = "integer",
    "USFWS CODE" = "character",
    "ATLAS BLOCK" = "character",
    "LOCALITY" = "character",
    "LOCALITY ID" = "character",
    "LOCALITY TYPE" = "factor",
    "LATITUDE" = "numeric",
    "LONGITUDE" = "numeric",
    "OBSERVATION DATE" = "Date",
    "TIME OBSERVATIONS STARTED" = "character",
    "OBSERVER ID" = "character",
    "FIRST NAME" = "character",
    "LAST NAME" = "character",
    "SAMPLING EVENT IDENTIFIER" = "character",
    "PROTOCOL TYPE" = "character",
    "PROTOCOL CODE" = "character",
    "PROJECT CODE" = "character",
    "DURATION MINUTES" = "integer",
    "EFFORT DISTANCE KM" = "numeric",
    "EFFORT AREA HA" = "numeric",
    "NUMBER OBSERVERS" = "integer",
    "ALL SPECIES REPORTED" = "logical",
    "GROUP IDENTIFIER" = "character",
    "HAS MEDIA" = "logical",
    "APPROVED" = "logical",
    "REVIEWED" = "logical",
    "REASON" = "character",
    "TRIP COMMENTS" = "character",
    "SPECIES COMMENTS" = "character",
    "BREEDING CODE" = "factor",
    "BREEDING CATEGORY" = "factor",
    "BEHAVIOR CODE" = "factor")
  
  # remove any columns not in header
  col_types <- col_types[names(col_types) %in% header]
  
  # make reader specific changes
  col_types <- substr(col_types, 1, 1)
  # add in guesses
  col_types <- col_types[header]
  col_types[is.na(col_types)] <- "?"
  col_types <- paste(col_types, collapse = "")
  col_types
}

colsToKeep <- c(
  "common_name",
  "scientific_name",
  "subspecies_common_name",
  "subspecies_scientific_name",
  "observation_count",
  "locality",
  "locality_type",
  "observation_date",
  "species_comments",
  "breeding_code",
  "breeding_category",
  "behavior_code",
  "age_sex",
  "country",
  "state",
  "county",
  "latitude",
  "longitude",
  "time_observations_started",
  "observer_id",
  "approved",
  "reviewed"
)

readEbirdRawFile <- function(fileName, colsToKeep) {
  header <- get_header(fileName)
  colNames <- make_clean_names(header)
  colTypes <- get_col_types(header)
  
  rowCount = read_tsv(fileName,
                      show_col_types = F,
                      n_max = 2,
                      name_repair = "minimal") %>% nrow()
  
  if (rowCount > 0){
    read_tsv(fileName,
             col_names = colNames,
             col_types = colTypes,
             skip = 1,
             quote = "",
             col_select = all_of(colsToKeep)
    )
  } else {
    tibble()
  }
}

extractDataFromEbirdZip = function(zipFile){
  contents = unzip(zipFile,list = T)
  
  dataFileNamePattern = "^ebd_.*txt$"
  
  # use basename here just in case the files we want are buried in a subdir
  # (eg if user has rezipped up the eBird download)
  toExtract = contents %>% 
    filter(str_detect(basename(Name),dataFileNamePattern)) %>% 
    filter(!str_detect(basename(Name),"sampling")) # exclude sampling files
  
  extracted = unzip(zipFile, files = toExtract$Name, exdir = tempdir())
  
  map_dfr(extracted,readEbirdRawFile,colsToKeep)
  
}


attachNearestHotspots = function(eBirdRecords,hotspots){
  sites = eBirdRecords %>% 
    count(country, state, county,locality, locality_type,latitude, longitude,
          name = "nRecords")
  
  includedCounties = sites %>% distinct(country,state,county)
  
  hotspotsToCheck = hotspots %>% 
    inner_join(includedCounties, by = "county") 
  
  nonHotspots = sites %>% 
    filter(locality_type != "H") %>% 
    mutate(siteId = seq_along(locality)) %>% 
    select(-county,-locality_type)
  
  hotspotsGeo <- st_as_sf(hotspotsToCheck, 
                          coords = c("lng", "lat"),
                          crs = 4326)
  nonHotspotsGeo = st_as_sf(nonHotspots, 
                            coords = c("longitude", "latitude"),
                            crs = 4326)
  
  dist_matrix   <- st_distance(nonHotspotsGeo, hotspotsGeo)          
  
  dist_matrix <- as_tibble(round(dist_matrix)) 
  names(dist_matrix) <- hotspotsToCheck$locId

  # find the 2 nearest hotspots and create new data frame
  nearest = dist_matrix %>% 
    mutate(siteId = nonHotspots$siteId) %>% 
    pivot_longer(-siteId, names_to = "locId",values_to = "dist") %>% 
    filter(!is.na(dist)) %>%  
    group_by(siteId) %>% 
    arrange(dist) %>% 
    slice_head(n = 2) %>%
    mutate(distanceRank = 1:2) %>% 
    ungroup()  %>%
    inner_join(hotspots %>% select(locName,lat,lng,locId),
               by = "locId") %>% 
    pivot_wider(id_cols = siteId, 
                names_from = distanceRank , 
                values_from = c(locName,dist), 
                names_glue = "nearestHotspot_{.value}{distanceRank}")   %>% 
    inner_join(nonHotspots,by = "siteId") %>% 
    select(locality, latitude,longitude,contains("_"))
  
  eBirdRecords %>% left_join(nearest, by = c("locality", "latitude", "longitude"))
  
}
