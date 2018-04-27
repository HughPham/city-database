#-----------------------------------------
# latest version : 2018 - 04 -27
#------------------------------------------
# This scrip can clean the orignial data
# which have only ONE variable in the CSV file


#------------------------------------------
# Empty the enviorment
#
rm(list = ls())


#------------------------------------------
# Load the packages
#
library(tidyverse)

file_list <-
  paste(
    "~/Documents/UntreatedData/CEIC/CIEC - city",
    dir("~/Documents/UntreatedData/CEIC/CIEC - city"),
    sep = "/"
  )

file_list_csv <-
  file_list[grepl(".csv", file_list)]


dt_clean <- function(x) {
  require("data.table")
  require("reshape2")
  tidy_data <-
    read_csv(x) %>%
    melt(., na.rm = TRUE)
  names(tidy_data)[1] <- "item"
  temp <- tstrsplit(": ", x  = tidy_data$item)
  num <-
    temp %>% lapply(., unique)   %>% lapply(., length) %>% unlist(.)
  tidy_data <-
    tidy_data %>%
    rename(year = variable) %>%
    mutate(city = temp[[which(num > 270 & num < 280)]],
           province = temp[[which(num > 30 & num < 34)]]) %>%
    mutate(variable =
             tidy_data$item[grepl("Beijing", tidy_data$item)] %>%
             unique(.) %>% gsub(": Beijing", "", .)) %>%
    mutate(item = NULL) %>%
    dcast(., city + province + year ~ variable)
  tidy_data <-
    tidy_data %>%
    mutate(city = ifelse(is.na(city), province, city)) %>%
    mutate(
      city = as.character(city),
      province = as.character(province),
      year = as.character(year)
    ) %>%
    mutate(
      city = gsub(" ", "", city),
      province = gsub(" ", "", province),
      year = gsub(" ", "", year)
    )
}

# Import all data into one List
city_list <- lapply(file_list_csv, dt_clean)
lapply(city_list, head)

clean_data <- data.frame(city_list[[1]])

for (i in 2:length(city_list)) {
  temp <- data.frame(city_list[[i]])
  clean_data <- full_join(clean_data, temp)
}

# Rename the variables 
clean_data <- 
clean_data %>%
  rename(
    gdp = CN..GDP,
    gdp_primary = CN..GDP..Primary.Industry,
    gdp_secondary = CN..GDP..Secondary.Industry,
    gdp_tertiary = CN..GDP..Tertiary.Industry,
    gdp_index = CN..GDP.Index..PY.100
  ) %>%
  rename(
    electricity_consumption = CN..Electricity.Consumption,
    electricity_consumption_industry = CN..Electricity.Consumption..Industry,
    electricity_consumption_residential = CN..Electricity.Consumption..Residential,
    electricity_consumption_per_capita_residential = 
      CN..Electricity.Consumption..per.Capita..Residential
  ) %>%
  rename(
    pop_nonagri = CN..Population..Non.Agricultural,
    pop_census_2000 = CN..Population..Census
  ) %>%
  rename(
    revenue_total = CN..Govt.Revenue,
    revenue_tax = CN..Govt.Revenue..Tax,
    revenue_tax_vat = CN..Govt.Revenue..Tax..Value.Added,
    revenue_tax_eit = CN..Govt.Revenue..Tax..Enterprises.Income,
    revenue_tax_pit = CN..Govt.Revenue..Tax..Individual.Income,
    revenue_tax_ot = CN..Govt.Revenue..Tax..Operation
  ) %>%
  rename(
    expenditure_total = CN..Govt.Expenditure,
    expenditure_educ = CN..Govt.Expenditure..Education,
    expenditure_health_family_planning = 
      CN..Govt.Expenditure..Health.and.Family.Planning,
    expenditure_science = CN..Govt.Expenditure..Science,
    expenditure_social_security_employment = 
      CN..Govt.Expenditure..Social.Security...Employment,
    expenditure_pensions_fund = CN...DC.Govt.Expenditure..Pensions..Social...Relief.Fund
  ) %>% 
  rename(
    num_broadband_internet_subscriber = 
    CN..No.of.Broadband.Internet.Subscriber,
    num_household = 
    CN..No.of.Household,
    num_post_telecom = 
    CN..Post.and.Telecom..Business.Volume,
    num_star_hotel = 
    CN..Star.Rated.Hotel..Number.of.Hotel,
    num_fixed_line_subscriber = 
    CN..Tel..No.of.Subscriber..Fixed.Line,
    num_mobile_subscriber = 
    CN..Tel..No.of.Subscriber..Mobile,
    num_wholesale_retail_enterprise = 
    CN..Wholesale...Retail..No.of.Enterprise,
    wholesale_retail_sales = 
    CN..Wholesale...Retail.Sales
  )
names(clean_data)
