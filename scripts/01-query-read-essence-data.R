# Setup ####
# package libraries
library(here)
library(tidyverse)
library(Rnssp)
library(janitor)
library(lubridate)

# set Rnnsp credentials
myProfile <- Credentials$new(
  username = askme("Enter your username: "),
  password = askme()
)

# data details ####

# url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?endDate=31Jul22&geography=15919&geography=33622&geography=33177&percentParam=noPercent&datasource=va_hosp&admissionTypeCategory=e&startDate=1Apr22&medicalGroupingSystem=essencesyndromes&userId=4887&aqtTarget=TimeSeries&ddAvailable=1&ccddCategory=cdc%20covid-specific%20dd%20v1&ccddCategory=cdc%20influenza%20dd%20v1&ccddCategory=cdc%20respiratory%20syncytial%20virus%20v1&geographySystem=hospital&detector=probrepswitch&removeZeroSeries=false&timeResolution=daily&hasBeenE=1"

# JSON with raw values 
# COVID
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?endDate=31Dec2022&ddInformative=1&geography=15919&geography=33622&geography=33177&percentParam=noPercent&datasource=va_hosp&admissionTypeCategory=e&startDate=1Jan2022&medicalGroupingSystem=essencesyndromes&userId=4887&aqtTarget=DataDetails&ddAvailable=1&ccddCategory=cdc%20covid-specific%20dd%20v1&geographySystem=hospital&detector=probrepswitch&timeResolution=daily&patientLoc=az_coconino&hasBeenE=1"

# # adjust dates to ensure report runs with start date of 12 weeks ago
# start_date <- date(Sys.Date()-weeks(12)) %>%
#   format("%d%b%y")
# 
# # end date for yesterday
# end_date <- date(Sys.Date()-days(1)) %>%
#   format("%d%b%y")
# 
# # update url with new dates 
# url <- str_replace(
#     string = url,
#     pattern = "&startDate=1Apr22&",
#     replacement = str_c(
#       "&startDate=",
#       start_date,
#       "&"
#     )
#   ) %>%
#     str_replace(
#       # string = url,
#       pattern = "endDate=31Jul22&",
#       replacement = str_c(
#         "endDate=",
#         end_date,
#         "&"
#     ))

# query
api_data_details <- get_api_data(url)

## Inspect data object structure
names(api_data_details)

## Get a glimpse of the pulled dataset
glimpse(api_data_details$dataDetails)
