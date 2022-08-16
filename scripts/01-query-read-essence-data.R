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
url <- ""

# adjust dates to ensure report runs with start date of 12 weeks ago
start_date <- date(Sys.Date()-weeks(12)) %>%
  format("%d%b%y")

# end date for yesterday
end_date <- date(Sys.Date()-days(1)) %>%
  format("%d%b%y")

# update url with new dates 
url <- str_replace(
    string = url,
    pattern = "&startDate=1Apr22&",
    replacement = str_c(
      "&startDate=",
      start_date,
      "&"
    )
  ) %>%
    str_replace(
      # string = url,
      pattern = "endDate=31Jul22&",
      replacement = str_c(
        "endDate=",
        end_date,
        "&"
    ))

# query
api_data_details <- get_api_data(url)

## Inspect data object structure
names(api_data_details)

## Get a glimpse of the pulled dataset
glimpse(api_data_details$dataDetails)
