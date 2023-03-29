library(here)
library(ptaxsim)
setwd('/Users/monicanimmagadda/Documents/MSCAPP/Mansueto')
ptaxsim_db_conn <- DBI::dbConnect(RSQLite::SQLite(), here("ptaxsim.db"))

# Select Pin, Municipal
pin_10 <- "1536100028"
pin_14 <- "15361000280000"
muni <- "VILLAGE OF RIVERSIDE"
year = 2021
# Look up tax code for pin
pin_info <- lookup_pin(year, pin_14)
tax_code = lookup_tax_code(year, pin_14)
# assessed value
av = pin_info$av
eav = pin_info$eav
#look up agencies associated with tax_code
agencies = lookup_agency(year, tax_code)

# look up agency levy
tax_district_1 = filter(agencies, agency_name == muni)
tax_district_1_levy = tax_district_1$agency_total_ext

# property type
class = pin_info$class
class_dict = read.csv("class_dict.csv")
class_desc = (filter(class_dict, class_dict$class_code == class))$class_desc
class_desc

# tax rate (levy/base)
tax_rate = (tax_district_1_levy / tax_district_1$agency_total_eav) * 100
property_tax_per_tax_district_1 = (tax_rate/100) * eav

reusable_funct(2021, pin_14, muni)

reusable_funct <- function(year, pin_14, muni){
  pin_info <- lookup_pin(year, pin_14)
  tax_code = lookup_tax_code(year, pin_14)
  av = pin_info$av
  eav = pin_info$eav
  agencies = lookup_agency(year, tax_code)
  tax_district_1 = filter(agencies, agency_name == muni)
  tax_district_1_levy = tax_district_1$agency_total_ext
  class = pin_info$class
  class_dict = read.csv("class_dict.csv")
  class_desc = (filter(class_dict, class_dict$class_code == class))$class_desc
  tax_rate = (tax_district_1_levy / tax_district_1$agency_total_eav) * 100
  property_tax_per_tax_district_1 = (tax_rate/100) * eav
  return(c(muni, tax_district_1_levy, tax_district_1$agency_total_eav, tax_rate, property_tax_per_tax_district_1))
}
print(reusable_funct(2021, pin_14, agencies[0]$agency_name))
for (agency in agencies$agency_name){
  print(reusable_funct(2021, pin_14, agency))
}
