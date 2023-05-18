library(data.table)
library(dplyr)
library(here)
library(ggplot2)
library(ggspatial)
library(glue)
library(ptaxsim)
library(sf)
library(tidyr)
library(htmlwidgets)
library(plotly)
example_pin10 <- "1536100027"
example_pin14 <- "15361000270000"


setwd("~/ptax-project")
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), here("./ptaxsim.db"))

  




shp_bnd_cook <- read_sf(paste0(
  "https://opendata.arcgis.com/api/v3/datasets/",
  "ea127f9e96b74677892722069c984198_1/downloads/data?",
  "format=geojson&spatialRefId=4326&where=1%3D1"
)) %>%
  select(geometry) %>%
  st_transform(3435)

shp_bnd_municipalities <- read_sf(paste0(
  "https://opendata.arcgis.com/api/v3/datasets/",
  "534226c6b1034985aca1e14a2eb234af_2/downloads/data?",
  "format=geojson&spatialRefId=4326&where=1%3D1"
))

shp_bnd_skokie <- shp_bnd_municipalities %>% 
  filter(AGENCY_DESC == "VILLAGE OF SKOKIE") %>%
  select(agency = AGENCY, name = AGENCY_DESC) %>%
  mutate(agency = "31170000")

shp_bnd_chicago <- shp_bnd_municipalities %>%
  filter(AGENCY_DESC == "CITY OF CHICAGO") %>%
  select(agency = AGENCY, name = AGENCY_DESC)

shp_bnd_example_pin <- read_sf(glue(
  "https://datacatalog.cookcountyil.gov/resource/",
  "77tz-riq7.geojson?pin10={example_pin10}"))



fh_agency_nums <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  "SELECT agency_num, agency_name, minor_type
  FROM agency_info
  WHERE agency_name LIKE '%SKOKIE %'")


fh_tax_codes <- DBI::dbGetQuery(
  ptaxsim_db_conn, "
  SELECT tax_code_num
  FROM tax_code
  WHERE agency_num = '031170000'
  AND year = 2021
  "
)

fh_pins <- DBI::dbGetQuery(
  ptaxsim_db_conn,
  glue_sql("
    SELECT pin, class
    FROM pin
    WHERE tax_code_num IN ({fh_tax_codes$tax_code_num*})
    AND year = 2021
    ",
           .con = ptaxsim_db_conn
  )
)


fh_pin10s <- unique(substr(fh_pins$pin, 1, 10))
fh_pins_geo <- lookup_pin10_geometry(year = 2021, pin10 = fh_pin10s)


fh_pins_geo <- fh_pins_geo %>%
  st_as_sf(wkt = "geometry", crs = 4326)

shp_bnd_skokie_pins <- fh_pins_geo %>%
  left_join(
    fh_pins %>%
      mutate(pin10 = substr(pin, 1, 10)) %>%
      group_by(pin10) %>%
      summarize(class = first(class)),
    by = "pin10"
  ) %>%
  mutate(
    major_class = substr(class, 1, 1))








#shp_bnd_riverside_pins <- read_sf(glue(
#  "https://datacatalog.cookcountyil.gov/resource/",
#  "77tz-riq7.geojson?MUNICIPALITY=Skokie&$limit=100000"
#)) %>%
#  select(pin10, geometry) %>%
#  filter(as.logical(st_intersects(st_centroid(.), shp_bnd_skokie)))

shp_bnd_elem_dist <- read_sf(paste0(
  "https://opendata.arcgis.com/api/v3/datasets/",
  "cbcf6b1c3aaa420d90ccea6af877562b_2/downloads/data?",
  "format=geojson&spatialRefId=4326&where=1%3D1"
)) %>%
  filter(AGENCY_DESCRIPTION == "SCHOOL DISTRICT 67")

#shp_bnd_elem_dist_pins <- read_sf(glue(
#  "https://datacatalog.cookcountyil.gov/resource/",
#  "77tz-riq7.geojson?elemschltaxdist=SCHOOL%20DISTRICT%67",
#  "&$limit=100000"
#)) %>%
#  filter(!pin10 %in% shp_bnd_skokie_pins$pin10) %>%
#  filter(is.na(as.logical(st_intersects(st_centroid(.), shp_bnd_skokie))))

shp_bnd_township <- read_sf(paste0(
  "https://opendata.arcgis.com/api/v3/datasets/",
  "78fe09c5954e41e19b65a4194eed38c7_3/downloads/data?",
  "format=geojson&spatialRefId=4326&where=1%3D1"
)) %>%
  filter(NAME == "SKOKIE")

#shp_bnd_township_pins <- read_sf(glue(
#  "https://datacatalog.cookcountyil.gov/resource/",
#  "77tz-riq7.geojson?politicaltownship=Town%20of%20Riverside",
#  "&$limit=50000"
#)) %>%
#  filter(!pin10 %in% shp_bnd_riverside_pins$pin10) %>%
#  filter(!pin10 %in% shp_bnd_elem_dist_pins$pin10) %>%
#  filter(is.na(as.logical(st_intersects(st_centroid(.), shp_bnd_riverside)))) %>%
#  filter(is.na(as.logical(st_intersects(st_centroid(.), shp_bnd_elem_dist))))

shp_bnd_hs_dist <- read_sf(paste0(
  "https://opendata.arcgis.com/api/v3/datasets/",
  "0657c2831de84e209863eac6c9296081_6/downloads/data?",
  "format=geojson&spatialRefId=4326&where=1%3D1"
)) %>%
  filter(AGENCY_DESC == "COMMUNITY HIGH SCHOOL 219")



#shp_bnd_hs_dist_pins <- read_sf(glue(
#  "https://datacatalog.cookcountyil.gov/resource/",
#  "77tz-riq7.geojson?highschltaxdist=RIVERSIDE%20BROOKFIELD%20HIGH%20SCHOOL%20208",
#  "&$limit=50000"
#)) %>%
#  filter(!pin10 %in% shp_bnd_riverside_pins$pin10) %>%
#  filter(!pin10 %in% shp_bnd_elem_dist_pins$pin10) %>%
#  filter(!pin10 %in% shp_bnd_township_pins$pin10) %>%
#  filter(is.na(as.logical(st_intersects(st_centroid(.), shp_bnd_riverside)))) %>%
#  filter(is.na(as.logical(st_intersects(st_centroid(.), shp_bnd_elem_dist)))) %>%
#  filter(is.na(as.logical(st_intersects(st_centroid(.), shp_bnd_township))))


#shp_bnd_cc_dist <- read_sf(glue(
#  "https://datacatalog.cookcountyil.gov/resource/",
#  "b8q9-dfei.geojson?MAX_AGENCY=TRITON%20COMM%20COLL%20DISTR%20504"
#))



shp_bnd_hs_diff <- shp_bnd_hs_dist %>%
  st_difference(shp_bnd_township)
shp_bnd_township_diff <- shp_bnd_township %>%
  st_difference(shp_bnd_elem_dist)
shp_bnd_elem_diff <- shp_bnd_elem_dist %>%
  st_difference(shp_bnd_riverside)

