##########==========##########==========##########==========##########==========

## SET-UP ======================================================================

## meta-information
## Author: Joshua Mendelsohn
## Creation: 2021-01-17
## Version: 4.0.3
## Description: Shape and merge road trip and map data

##  environment set-up
remove(list = objects())
options(digits = 6, scipen = 2) # , expressions = 2^9
library(lintr) #lint("1_data_shaping.R")
library(tidyverse)
library(xml2)

## READ IN DATA ================================================================

travels <- read_xml("A_Inputs/Travels.kml")
wishlist <- read_xml("A_Inputs/Wishlist.kml")

oconus_map <- as_tibble(map_data("world",
  region = c("USA:Alaska", "USA:Hawaii", "Puerto Rico")))
conus_map <- as_tibble(map_data("state"))

## CLEAN AND COMPILE MAP DATA ==================================================

## merge maps and harmonize state coding
us_map <- bind_rows(conus_map, oconus_map) %>%
  mutate("region" = ifelse(region == "USA", subregion, region))
remove(oconus_map, conus_map)

## convert state names with postal codes
postal_codes <- match(
  str_to_title(us_map$region),
  c(state.name, "Puerto Rico", "District Of Columbia"))
postal_codes <- c(state.abb, "PR", "DC")[postal_codes]
us_map <- mutate(us_map, "state" = postal_codes)
remove(postal_codes)

## drop specific islands to improve map aesthetics
us_map <- filter(us_map,
  !((state == "AK") & (group != 78)),
  !((state == "WA") & (group != 60))
  )

## CLEAN AND COMPILE KML CITY DATA =============================================

## write recursive function to extract object names from kml hierarchy
name_extractor <- function(kml_list) {
  if (!is.list(kml_list) | is.null(names(kml_list))) {
    return(kml_list)
  } else{
    i <- any(names(kml_list) %in% c("Folder", "Placemark"))
    j <- sapply(kml_list, function(x) {unlist(x$name)})
    names(kml_list)[i] <- paste(names(kml_list)[i], j, sep = "∆")
    lapply(kml_list, name_extractor)
      }
  }

## convert kml files to a tidy-esque format
travels  <- travels  %>% as_list() %>% name_extractor %>% unlist() %>% enframe()
wishlist <- wishlist %>% as_list() %>% name_extractor %>% unlist() %>% enframe()


remove(name_extractor)
## merge travel and wishlist files
travels$name  <- paste("travels",  travels$name, sep = ".")
wishlist$name <- paste("wishlist", wishlist$name, sep = ".")
travels <- bind_rows(travels, wishlist)
remove(wishlist)

## extract and refine coordinate data
refine_coordinates <- function(raw_xy) {
  raw_xy <- raw_xy %>%
    str_remove_all("[\t\n]") %>%
    trimws() %>%
    str_split(" +") %>%
    lapply(str_split, ",") %>%
    rapply(as.numeric, how = "list") %>%
    lapply(simplify2array) %>%
    lapply(t)

  raw_xy
  }

travels <- travels %>%
  filter(
    str_detect(name, "coordinates"),
    !str_detect(name, "Folder∆Walks"),
    !str_detect(name, "Folder∆Alternates"),
    !str_detect(name, "Folder∆Labels")
    ) %>%
  mutate("value" = refine_coordinates(value)) %>%
  unnest(value)

remove(refine_coordinates)

## parse kml hierarchy
travels$name <- travels$name %>%
  str_split("[.]") %>%
  simplify2array() %>%
  t()

## flatten refined data
travels <- cbind(travels$name, travels$value)
travels <- as_tibble(travels)

colnames(travels) <- c(
  "file", "format", "header",
  "folder1", "folder2", "folder3",
  "placemark", "polygon", "data_type",
  "long", "lat", "altitude"
  )

## get rid of excess directory levels
i <- sapply(travels, function(x) {length(unique(x))})
i <- names(i[i > 1])
travels <- travels[, i] %>% select(-altitude, -file)
remove(i)

## convert coordinates to numeric
travels$long <- as.numeric(travels$long)
travels$lat <- as.numeric(travels$lat)

## simplify directory structure
travels[, c("folder1", "folder2", "folder3")] <- apply(
  travels[, c("folder1", "folder2", "folder3")],
  2,
  str_remove_all,
  pattern = "Folder∆"
  )

travels$placemark <- str_remove_all(travels$placemark, "Placemark∆")

## STANDARDIZE FORMATS =========================================================

## generate polygon groups and orders for travels
travels <- travels %>% mutate(
  "group" = paste(folder1, folder2, folder3, placemark),
  "order"= seq(nrow(travels))
  )
travels$group <- travels$group %>%
  factor(levels = unique(travels$group)) %>%
  as.numeric()
travels$order <- 1 + travels$order - tapply(
  travels$order, travels$group, min)[as.character(travels$group)]

## stanardize organizational schemes
us_map$theme <- "map"
travels$theme <- travels$folder1

us_map$polygon <- "polygon"

us_map$level1 <- us_map$state
travels$level1 <- if_else(
  travels$polygon == "LineString",
  travels$folder3,
  travels$placemark
  )
travels$level2 <- if_else(
  travels$polygon == "LineString",
  travels$placemark,
  as.character(NA)
  )
us_map$level2 <- NA

us_map <- select(us_map, long, lat, group, order,
  theme, level1, level2, polygon)
travels <- select(travels, long, lat, group, order,
  theme, level1, level2, polygon)

## remove travel marks outside project bounds
travels <- filter(travels, !str_detect(travels$level1, ", [A-Z][a-z]+"))

## HARMONIZE CITY DATA =========================================================

## PROJECT COORDINATES - AK/HI/PR ==============================================

## PROJECT COORDINATES - CONTIGUOUS STATES =====================================

## TABULATE PROGRESS STATISTICS ================================================

## EXPORT DATA =================================================================

##########==========##########==========##########==========##########==========
