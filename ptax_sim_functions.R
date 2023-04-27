# script with all functions
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), here("ptaxsim.db"))
pin_10 <- "1536100028"
year=2021
pin_14 <- "15361000280000"
library(dplyr) 
library(scales)

tax_code = lookup_tax_code(year, pin_14)
agencies = lookup_agency(year, tax_code)
single_bill <- tax_bill(year_vec=2021, pin_vec=pin_14)

create_values_per_district <- function(muni, year, pin14){
  tax_district_levy <- agencies %>%
    filter(agency_name==muni) %>%
    select(agency_total_ext) %>%
    pull() %>%
    as.numeric() %>%
    signif(2)

  tax_district_base <- agencies %>%
    filter(agency_name==muni) %>%
    select(agency_total_eav) %>%
    pull() %>%
    as.numeric() %>%
    signif(2)

  rate = 100 * (tax_district_levy/tax_district_base)

  single_bill <- tax_bill(year_vec=2021, pin_vec=pin_14)
  av = signif(single_bill[agency_name==muni]$av,2)
  eav = signif(single_bill[agency_name==muni]$eav,2)
  property_tax_per_district = signif(single_bill[agency_name==muni]$final_tax,2)
  
  return(data.frame(district=muni,levy=tax_district_levy, base=tax_district_base, rate=rate, av=av,eav=eav,property_tax=property_tax_per_district))
}

populate_df <- function(df, districts, year, pin_14){
  # Create an empty data frame with the column names
  for (d in districts){
    temp <- create_values_per_district(d, year, pin_14)
    df <- rbind(df, temp)
  }
  return(df)
}

format_df <- function(df){
  df$levy <- as.integer(df$levy)
  #df$levy <- signif(df$levy, 1)
  df$base <- round(df$base, 2)
  df$rate <- round(df$rate, 2)
  df$eav <- round(df$eav, 1)
  df$av <- round(df$av, 1)
  
  df$levy <- scales::label_number(prefix = "$", accuracy=0.1, scale_cut=scales::cut_short_scale())(df$levy)
  df$base <- scales::label_number(prefix = "$", accuracy=NULL, scale_cut=scales::cut_short_scale())(as.numeric(df$base))
  df$eav <- scales::label_number(prefix = "$", accuracy=NULL, scale_cut=scales::cut_short_scale())(as.numeric(df$eav))
  df$av <- scales::label_number(prefix = "$", accuracy=NULL, scale_cut=scales::cut_short_scale())(as.numeric(df$av))
  df$property_tax <- scales::label_number(prefix = "$", accuracy=NULL, scale_cut=scales::cut_short_scale())(as.numeric(df$property_tax))
  return(df)
}

get_highest_ext <- function(df){
  df %>%
    group_by(agency_minor_type) %>%
    slice_max(agency_total_ext)
}

create_agency_list <- function(df){
  agency_list <- list()
  for (name in unique(df$agency_major_type)){
    options <- df %>% filter(agency_major_type == name)
    print(options)
    if (name == 'SCHOOL'){
      temp <- options %>%
        arrange(desc(options$agency_total_ext)) %>%
        slice(1:2)
      if (nrow(temp)>0){
        agency_list <- c(agency_list, temp$agency_name[1:2])
      }
    } else {
      temp <- options %>%
        arrange(desc(options$agency_total_ext)) %>%
        slice(1:1)
      print(temp$agency_name[1])
      if (nrow(temp)>0){
        agency_list <- c(agency_list, temp$agency_name[1])
      }
    }
  }
  return (unlist(agency_list))
}