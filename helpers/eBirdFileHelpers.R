library(readr)
library(dplyr)
library(stringr)
library(janitor)

get_header <- function(x, sep = "\t") {
  readLines(x, n = 1) %>%
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
    "CATEGORY" = "character",
    "COMMON NAME" = "character",
    "SCIENTIFIC NAME" = "character",
    "SUBSPECIES COMMON NAME" = "character",
    "SUBSPECIES SCIENTIFIC NAME" = "character",
    "OBSERVATION COUNT" = "character",
    "BREEDING BIRD ATLAS CODE" = "character",
    "BREEDING BIRD ATLAS CATEGORY" = "character",
    "AGE/SEX" = "character",
    "COUNTRY" = "character",
    "COUNTRY CODE" = "character",
    "STATE" = "character",
    "STATE CODE" = "character",
    "COUNTY" = "character",
    "COUNTY CODE" = "character",
    "IBA CODE" = "character",
    "BCR CODE" = "integer",
    "USFWS CODE" = "character",
    "ATLAS BLOCK" = "character",
    "LOCALITY" = "character",
    "LOCALITY ID" = "character",
    "LOCALITY TYPE" = "character",
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
    "SPECIES COMMENTS" = "character")
  
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
  "last_edited_date",
  "common_name",
  "scientific_name",
  "subspecies_common_name",
  "subspecies_scientific_name",
  "observation_count",
  "locality",
  "observation_date",
  "species_comments",
  "breeding_code",
  "breeding_category",
  "behavior_code",
  "age_sex",
  "county",
  "latitude",
  "longitude",
  "time_observations_started",
  "observer_id",
  "approved"
)

readEbirdRawFile <- function(fileName, colsToKeep) {
  header <- get_header(fileName)
  colNames <- make_clean_names(header)
  colTypes <- get_col_types(header)
  
  
  raw = read_tsv(fileName,
                 col_names = colNames,
                 col_types = colTypes,
                 skip = 1,
                 quote = "",
                 col_select = all_of(colsToKeep)
  )
}

extractDataFromEbirdZip = function(zipFile){
  contents = unzip(zipFile,list = T)
  
  dataFileNamePattern = "^ebd_.*txt$"
  
  toExtract = contents %>% 
    filter(str_detect(Name,dataFileNamePattern))
  
  extracted = unzip(zipFile, files = toExtract$Name, exdir = tempdir())
  
  map_dfr(extracted,readEbirdRawFile,colsToKeep)
  
}

