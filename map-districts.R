
library(tidyverse)
library(ggplot2)
library(sf)
library(viridis)
library(patchwork)
library(scales)
library(tidygeocoder)
library(readxl)
library(writexl)
library(units)
library(ggsn)
library(arrow)
library(geoarrow)
library(sfarrow)
library(osmdata)
library(viridis)
library(scatterpie)
library(ggrepel)
options(scipen = 999)

## Python Script to prep blocks and parcels
# https://gist.github.com/nmarchio/d65fbfca8e90b82a0b5980ba65c6d906

## Link to data:
# https://uchicago.box.com/s/v8disqrjidjx4svs2plv6ijaakutjwse

## CHANGE PATH
wd_path = '/Users/nm/Downloads/assessor/'

# Read in files -----------------------------------------------------------

county = st_read(fs::dir_ls(paste0(wd_path,'municipal-districts'), regexp = "County_Border")) %>% 
  select(OBJECTID_1) %>%
  mutate(agency = "Cook County",
         agency_description = "Cook County") %>%
  select(agency, agency_description ) %>%
  mutate(district_type = 'County')%>%
  st_transform(4326) 

munis = st_read(fs::dir_ls(paste0(wd_path,'municipal-districts'), regexp = "Municipality")) %>% 
  select(AGENCY, AGENCY_DESC) %>% 
  rename(agency_description = AGENCY_DESC) %>%
  rename_all(list(tolower)) %>%
  mutate(district_type = 'Municipality')%>%
  st_transform(4326)

elementary_school <- st_read(fs::dir_ls(paste0(wd_path,'tax-districts'), regexp = "Elementary_School_Tax_Districts"))  %>% 
  select(AGENCY, AGENCY_DESCRIPTION) %>% 
  rename_all(list(tolower)) %>%
  mutate(district_type = 'Elementary School')%>%
  st_transform(4326)

fire_protection <- st_read(fs::dir_ls(paste0(wd_path,'tax-districts'), regexp = "Fire_Protection_Tax_Districts"))  %>% 
  select(AGENCY, AGENCY_DESCRIPTION) %>% 
  rename_all(list(tolower)) %>%
  mutate(district_type = 'Fire Protection')%>%
  st_transform(4326)

high_school <- st_read(fs::dir_ls(paste0(wd_path,'tax-districts'), regexp = "High_School_Tax_District")) %>% 
  select(AGENCY, AGENCY_DESC) %>% 
  rename(agency_description = AGENCY_DESC) %>%
  rename_all(list(tolower)) %>%
  mutate(district_type = 'High School')%>%
  st_transform(4326)

library <- st_read(fs::dir_ls(paste0(wd_path,'tax-districts'), regexp = "Library_Tax_District")) %>% 
  select(AGENCY, MAX_AGENCY_DESC) %>% 
  rename(agency_description = MAX_AGENCY_DESC) %>%
  rename_all(list(tolower)) %>%
  mutate(district_type = 'Library')%>%
  st_transform(4326)

park <- st_read(fs::dir_ls(paste0(wd_path,'tax-districts'), regexp = "Park_Tax_Districts")) %>% 
  select(AGENCY, AGENCY_DESCRIPTION) %>% 
  rename_all(list(tolower)) %>%
  mutate(district_type = 'Park')%>%
  st_transform(4326)

sanitary <- st_read(fs::dir_ls(paste0(wd_path,'tax-districts'), regexp = "Sanitary_Tax_Districts")) %>% 
  select(AGENCY, MAX_AGENCY_DESC) %>% 
  rename(agency_description = MAX_AGENCY_DESC) %>%
  rename_all(list(tolower)) %>%
  mutate(district_type = 'Sanitary')%>%
  st_transform(4326)

unit_school <- st_read(fs::dir_ls(paste0(wd_path,'tax-districts'), regexp = "Unit_School_Tax_District"))  %>% 
  select(AGENCY, MAX_AGENCY_DESC) %>% 
  rename(agency_description = MAX_AGENCY_DESC) %>%
  rename_all(list(tolower)) %>%
  mutate(district_type = 'Unit School')%>%
  st_transform(4326)

tif <- st_read(fs::dir_ls(paste0(wd_path,'tax-districts'), regexp = "TIF_Districts")) %>%
  select(AGENCY, AGENCY_DESCRIPTION) %>% 
  rename_all(list(tolower)) %>%
  mutate(district_type = 'Tax Increment Finance')%>%
  st_transform(4326)

tax_districts <- rbind(county,munis,elementary_school,fire_protection,high_school,library,park,sanitary,unit_school,tif) %>%
  st_make_valid()

rm(county,munis,elementary_school,fire_protection,high_school,library,park,sanitary,unit_school,tif)

# blocks_2020 <- tidycensus::get_decennial(geography = "block",
#                                          variables = c('P2_001N'),
#                                          summary_var = c('P2_001N'), year = 2020, state = '17', county = '031', 
#                                          geometry = TRUE, cache = TRUE) %>%
#   st_transform(4326)

blocks <- st_read(fs::dir_ls(paste0(wd_path,'parcels-buildings'), regexp = "blocks_2020"))
  
# st_write(blocks_2020, '/Users/nm/Downloads/assessor/parcels-buildings/blocks_2020.geojson', delete_dsn = TRUE)

# Sites -------------------------------------------------------------------

## CHANGE COORDS AND PLACES
sites <- data.frame(place=c('Lincoln Park', 'Winnetka'),
                    longitude=c(-87.647494, -87.732524),
                    latitude=c(41.922973, 42.107828)) %>%
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326, agr = "constant") %>%
  st_transform(3857) %>%
  st_buffer(., 1000) %>%
  st_transform(4326) 

## CHANGE VAR
place_var = 'Winnetka'

# Map layers --------------------------------------------------------------

target_blocks = blocks %>% 
  st_join(x = ., y = sites %>% filter(place == place_var), left = FALSE)

target_tax_districts <- tax_districts %>%
  st_join(x = ., y = sites %>% filter(place == place_var), left = FALSE)

query <- open_dataset(fs::dir_ls(paste0(wd_path,'parcels-buildings'), regexp = "Building_Footprints_Cook")) %>%
  filter(block_fips %in% unique(target_blocks$GEOID))
buildings <- query %>%
  geoarrow_collect_sf() %>%
  st_set_crs(4326)

query <- open_dataset(fs::dir_ls(paste0(wd_path,'parcels-buildings'), regexp = "Parcels_2021")) %>%
  filter(block_fips %in% unique(target_blocks$GEOID))
parcels <- query %>%
  geoarrow_collect_sf() %>%
  st_set_crs(4326)

# Map layers --------------------------------------------------------------

# colorhexes <- colorRampPalette(c('#93328E','#CF3F80','#F7626B','#FF925A','#FFC556','#F9F871'))(length())
# scale_fill_manual(values = c("#009EFA",'#F77552',"#49DEA4","#ffc425",'#845EC2','#FF6F91','#00D2FC','#008F7A','#00C0A3')) + 

# Visualize parcels and buildings
(map_block <- ggplot() +
  geom_sf(data = parcels %>% st_make_valid() %>% 
            st_intersection(., sites %>% filter(place == place_var)), aes(fill = parceltype), color = alpha('white',0), size = .1, alpha = 1) +
  geom_sf(data = buildings %>% st_make_valid() %>% 
            st_intersection(., sites %>% filter(place == place_var)), fill = 'black', color = alpha('white',0), size = .1, alpha = 1) +
  theme_void() +
  theme(legend.position = 'none'))

# Visualize tax districts
(map_districts <- ggplot() +
  geom_sf(data = target_tax_districts %>% filter(district_type != 'County'),
          aes(fill =  district_type,  color = district_type), alpha = .1) +
  scale_fill_manual(name = 'Tax district', values = c("#009EFA",'#F77552',"#49DEA4","#ffc425",'#845EC2','#FF6F91','#00D2FC','#008F7A','#00C0A3'), ) +
  scale_color_manual(name = 'Tax district', values = c("#009EFA",'#F77552',"#49DEA4","#ffc425",'#845EC2','#FF6F91','#00D2FC','#008F7A','#00C0A3')) + 
  geom_sf(data = sites %>% filter(place == place_var), fill = 'white', color = 'black', size = .5, alpha = 1) +
  geom_sf(data = buildings %>% st_make_valid() %>% st_intersection(., sites %>% filter(place == place_var)) , fill = 'black', color = alpha('white',0), size = .1, alpha = 1) +
  theme_void() )

map_block + map_districts 


