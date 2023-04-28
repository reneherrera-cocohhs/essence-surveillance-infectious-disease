# Setup ####
# package libraries
library(here)
library(tidyverse)
library(Rnssp)
library(janitor)
library(lubridate)
library(visdat)

# set Rnnsp credentials
myProfile <- Credentials$new(
  username = askme("Enter your username: "),
  password = askme()
)

# data details ####
# JSON with raw values
# COVID
url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?endDate=31Dec2022&ddInformative=1&geography=15919&geography=33622&geography=33177&percentParam=noPercent&datasource=va_hosp&admissionTypeCategory=e&startDate=1Jan2022&medicalGroupingSystem=essencesyndromes&userId=4887&aqtTarget=DataDetails&ddAvailable=1&ccddCategory=cdc%20covid-specific%20dd%20v1&geographySystem=hospital&detector=probrepswitch&timeResolution=daily&patientLoc=az_coconino&hasBeenE=1"

# query essence data 
api_data_details <- get_api_data(url)

# tidy and transform 
essence_data <- api_data_details$dataDetails %>%
  clean_names() %>% # make messy variables names neat
  as_tibble() %>%
  distinct( # remove duplicates 
    date,
    hospital_name,
    sex,
    age,
    medical_record_number,
    c_unique_patient_id,
    .keep_all = TRUE
  )

# create character list of date-time variables 
date_time_var <- essence_data %>%
  select(contains("date_time")) %>%
  select(!contains("text")) %>%
  select(!contains("_detection")) %>%
  select(!contains("str_")) %>%
  names()
  
# transform
ed_df <- essence_data %>% 
  mutate_if(is.character, str_to_lower) %>%
  mutate(across(.cols = all_of(date_time_var), ymd_hms)) %>%
  mutate(
    date = mdy(date),
    year = year(date),
    month = month(date),
    week = week(date),
    str_birth_date_time = ymd(str_birth_date_time),
    initial_temp_calc = as.numeric(initial_temp_calc),
    initial_pulse_oximetry_calc = as.numeric(initial_pulse_oximetry_calc),
    height = as.numeric(height),
    weight = as.numeric(weight),
    home_facility_distance = as.numeric(home_facility_distance),
    systolic_blood_pressure = as.numeric(systolic_blood_pressure),
    diastolic_blood_pressure = as.numeric(diastolic_blood_pressure),
    p_zip = str_extract(patient_zip, "[:digit:]{5}"),
    age_group = as.factor(age_group),
    age = if_else(
      condition = c_patient_age_units == "years",
      true = as.double(age),
      false = as.double(age)/12),
    body_mass_index = as.double(body_mass_index)
  ) %>%
  mutate(
    blood_pressure_cat = case_when(
      systolic_blood_pressure < 120 & diastolic_blood_pressure < 80 ~ "Normal",
      systolic_blood_pressure < 130 & diastolic_blood_pressure < 80 ~ "Elevated",
      systolic_blood_pressure < 140 | diastolic_blood_pressure < 90 ~ "Stage 1 high",
      systolic_blood_pressure >= 140 | diastolic_blood_pressure >= 90 ~ "Stage 2 high",
      TRUE ~ str_c(systolic_blood_pressure, "/", diastolic_blood_pressure)
    )
  ) %>%
  mutate(
    bmi = (weight / height / height)*10000,
    bmi_cat = if_else(
      condition = age >= 16,
      true = case_when(
        bmi < 18.5 ~ "Underweight",
        bmi < 25 ~ "Normal weight",
        bmi < 30 ~ "Over weight",
        bmi >= 30 ~ "Obese",
        TRUE ~ NA_character_
      ),
      false = NA_character_),
    body_mass_cat = if_else(
      condition = age >= 16,
      true = case_when(
        body_mass_index < 18.5 ~ "Underweight",
        body_mass_index < 25 ~ "Normal weight",
        body_mass_index < 30 ~ "Over weight",
        body_mass_index >= 30 ~ "Obese",
        TRUE ~ NA_character_
      ),
      false = NA_character_)
  ) 



# code new variables 
ed_df %>%
  mutate(
    
  )


#### TEST text analysis #### 

library(tidytext)
library(stopwords)

# custom functions ####
# function to code words
# tokenize words in character variable, remove stop words,
# then produce a frequency table of words
code_words <- function(.data, x) {
  .data %>%
    mutate(
      text = {{ x }},
      line = row_number()
    ) %>%
    unnest_tokens(
      output = word,
      input = text,
      token = "words"
    ) %>%
    anti_join(get_stopwords()) %>%
    count(word) 
}

ed_df %>%
  code_words(x = chief_complaint_parsed) %>%
  arrange(desc(n))

ed_df %>%
  code_words(x = medication_list) %>%
  arrange(desc(n))

                          


#### TEST text analysis #### 



glimpse(ed_df)

# vis_dat(ed_df)


#### 

ed_df %>%
  tabyl(disposition_category)

ed_df %>%
  group_by(month) %>%
  count(disposition_category) %>%
  ggplot() +
  geom_col(
    mapping = aes(
      x = disposition_category,
      y = n
    )
  ) +
  coord_flip() +
  expand_limits(x = 0, y = c(0,1000)) +
  facet_wrap(~month)

library(gganimate)

ed_df %>%
  group_by(month) %>%
  count(disposition_category) %>%
  ggplot(mapping = aes(
    x = disposition_category,
    y = n
  )) +
  geom_col() +
  labs(title = "{frame_time}") +
  transition_time(month)

ed_df %>%
  group_split(month) %>%
  map(
    ~count(.x$disposition_category)
    # ~ggplot(
    #   .,
    #   mapping = aes(
    #     x = disposition_category,
    #     y = n
    #   )
    # ) +
    #   geom_col() +
    #   coord_flip() +
    #   ylim(0,750)
  )


####


#### TESTING ####

ed_df %>%
  filter(date > date("2022-10-15")) %>%
  group_by(week) %>%
  count() %>%
  ggplot() +
  geom_col(
    mapping = aes(
      x = week,
      y = n
    )
  )
  

## chief complaint text analysis 


## sex
ed_df %>%
  tabyl(sex)

ed_df_sex <- ed_df %>%
  group_by(
    date,
    hospital_name,
    sex
  ) %>%
  count() %>%
  ungroup() %>%
  complete(
    date = seq.Date(date("2022-01-01"), date("2022-12-31"), by = "day"),
    nesting(hospital_name, sex),
    fill = list(
      n = 0
    ),
    explicit = FALSE
  )

ed_df_sex %>%
  ggplot() +
  geom_line(
    mapping = aes(
      x = date,
      y = n,
      color = sex
    )
  ) +
  facet_wrap(~hospital_name) +
  ylim(0,NA)
  
ed_df_sex %>%
  mutate(
    month = month(date),
    week = week(date)
  ) %>%
  group_by(week, hospital_name, sex) %>%
  # group_by(month, hospital_name, sex) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(
    mapping = aes(
      # x = month,
      x = week,
      y = n,
      color = sex
    )
  ) +
  facet_wrap(~hospital_name) +
  ylim(0,NA)

## age & age units;  age_group
ed_df %>%
  tabyl(age_group)

ed_df_age <- ed_df %>%
  group_by(
    date,
    hospital_name,
    age_group
  ) %>%
  count() %>%
  ungroup() %>%
  complete(
    date = seq.Date(date("2022-01-01"), date("2022-12-31"), by = "day"),
    nesting(hospital_name, age_group),
    fill = list(
      n = 0
    ),
    explicit = FALSE
  )

ed_df_age %>%
  ggplot() +
  geom_line(
    mapping = aes(
      x = date,
      y = n,
      color = age_group
    )
  ) +
  facet_wrap(~hospital_name) +
  ylim(0,NA)

ed_df_age %>%
  mutate(
    month = month(date),
    week = week(date)
  ) %>%
  group_by(week, hospital_name, age_group) %>%
  # group_by(month, hospital_name, age_group) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(
    mapping = aes(
      # x = month,
      x = week,
      y = n,
      color = age_group
    )
  ) +
  facet_wrap(~hospital_name) +
  ylim(0,NA)

## ethnicity and race: c_race_c_eth_combined_narrow
ed_df %>%
  tabyl(c_race_c_eth_combined_narrow)

ed_df %>%
  tabyl(c_race_c_eth_combined_broad)

ed_df_eth <- ed_df %>%
  mutate(
    race_eth_code = case_when(
      c_race_c_eth_combined_broad == "american indian or alaska native and non-hispanic" ~ "American Indian or Alaska Native",
      c_race_c_eth_combined_broad == "white and non-hispanic" ~ "Non-Hispanic White",
      c_race_c_eth_combined_broad == "hispanic or latino" ~ "Hispanic",
      str_detect(c_race_c_eth_combined_broad, "and non-hispanic") ~ "Other and Non-Hispanic",
      str_detect(c_race_c_eth_combined_broad, "unknown") ~ "Unknown"
    )
  ) %>%
  group_by(
    date,
    hospital_name,
    race_eth_code
  ) %>%
  count() %>%
  ungroup() %>%
  complete(
    date = seq.Date(date("2022-01-01"), date("2022-12-31"), by = "day"),
    nesting(hospital_name, race_eth_code),
    fill = list(
      n = 0
    ),
    explicit = FALSE
  )

ed_df_eth %>%
  filter(date > date("2022-11-01")) %>%
  ggplot() +
  geom_line(
    mapping = aes(
      x = date,
      y = n,
      color = race_eth_code
    )
  ) +
  facet_wrap(~hospital_name) +
  ylim(0,NA)

ed_df_eth %>%
  mutate(
    month = month(date),
    week = week(date)
  ) %>%
  group_by(week, hospital_name, race_eth_code) %>%
  # group_by(month, hospital_name, race_eth_code) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  ggplot() +
  geom_line(
    mapping = aes(
      # x = month,
      x = week,
      y = n,
      color = race_eth_code
    )
  ) +
  facet_wrap(~hospital_name) +
  ylim(0,NA)

## initial temp calculation; initial_temp_calc                        
ed_df %>%
  mutate(temp_code = cut(x = ed_df$initial_temp_calc, breaks = 5)) %>%
  # cut(x = ed_df$initial_temp_calc, breaks = 5)
  tabyl(temp_code)

class(ed_df$initial_temp_calc)



## ccdd category flat 



## geography, patient_zip, patient_city 
ed_df %>%
  tabyl(patient_city)

ed_df %>%
  tabyl(patient_city, hospital_name)

ed_df %>%
  tabyl(p_zip)

ed_df %>%
  tabyl(p_zip, hospital_zip)


library(tigris)
library(sf)
library(zipcodeR)
library(tidycensus)

options(tigris_use_cache = TRUE)

# query census for zip code denominators 
zip_pop_denominator <- get_acs(
  geography = "zcta",
  survey = "acs5",
  variables = c(
    "total" = "S0101_C01_001"
  ),
  cache_table = TRUE,
  year = 2021
)


# which zip codes exist in essence 
zip_code_list <- unique(ed_df$p_zip)

zip_pop_denominator <- zip_pop_denominator %>%
  filter(GEOID %in% zip_code_list)

# use tigris to get list of zip codes 
zip_code_df <- zctas() %>%
  clean_names() 


# subset national zip code list to zip codes that exist in essence data 
zip_code_df_list <- zip_code_df %>%
  filter(zcta5ce20 %in% zip_code_list)

# geospatial data for coconino county 
coco_sf <- counties(
  state = "AZ"
) %>%
  clean_names() %>%
  filter(name == "Coconino")

# crude rate of cases in each zip code 
zip_code_count <- ed_df %>%
  count(p_zip) %>%
  inner_join(
    zip_pop_denominator,
    by = c("p_zip" = "GEOID")
  ) %>%
  mutate(
    rate = 100000*(n/estimate)
  )

# spatial join between the raw count and zip code spatial data
essence_sf <- geo_join(
  spatial_data = zip_code_df_list,
  data_frame = zip_code_count,
  by_sp = "zcta5ce20",
  by_df = "p_zip"
) %>%
  filter(estimate >= 100)

# plot crude rate for each zip code in county 
ggplot() +
  geom_sf(
    data = coco_sf,
    fill = "grey"
  ) +
  geom_sf(
    data = essence_sf,
    mapping = aes(fill = rate)
  ) +
  scale_fill_viridis_c() +
  labs(
    fill = "Rate per 100,000 population",
    title = "ESSENCE: CDC COVID-Specific DD v1",
    subtitle = "From 1 Jan 2022 to 31 Dec 2022"
  ) +
  theme_void()




# crude rate of cases in each zip code by month
zip_code_count_month <- ed_df %>%
  group_by(month) %>%
  count(p_zip) %>%
  inner_join(
    zip_pop_denominator,
    by = c("p_zip" = "GEOID")
  ) %>%
  mutate(
    rate = 100000*(n/estimate)
  )



# spatial join between the raw count and zip code spatial data
essence_sf_month <- geo_join(
  spatial_data = zip_code_df_list,
  data_frame = zip_code_count_month,
  by_sp = "zcta5ce20",
  by_df = "p_zip",
  how = "inner"
) %>%
  filter(estimate >= 100)

# FUNCTION TESTING ####
plot_funct <- function(x){
  ggplot() +
    geom_sf(
      data = coco_sf,
      fill = "grey"
    ) +
    geom_sf(
      data = essence_sf_month[essence_sf_month$month == x, ],
      mapping = aes(fill = rate)
    ) +
    scale_fill_viridis_c() +
    labs(
      fill = "Rate per 100,000 population"
      # title = "ESSENCE: CDC COVID-Specific DD v1",
      # subtitle = "By month from 1 Jan 2022 to 31 Dec 2022"
    ) +
    theme_void()
}



# FUNCTION TESTING ####

# plot crude rate for each zip code in county 
ggplot() +
  geom_sf(
    data = coco_sf,
    fill = "grey"
  ) +
  geom_sf(
    data = essence_sf_month,
    mapping = aes(fill = rate)
  ) +
  facet_wrap(~month) +
  scale_fill_viridis_c() +
  labs(
    fill = "Rate per 100,000 population",
    title = "ESSENCE: CDC COVID-Specific DD v1",
    subtitle = "By month from 1 Jan 2022 to 31 Dec 2022"
  ) +
  theme_void()

#### FUNCTION to make plot for each month 

data_months <- unique(essence_sf_month$month)


test_function <- function(x){

ggplot() +
  geom_sf(
    data = coco_sf,
    fill = "grey",
    color = "black"
  ) +
  geom_sf(
    data = essence_sf_month[essence_sf_month$month == x, ],
    mapping = aes(fill = rate)
  ) +
  scale_fill_viridis_c() +
  labs(
    fill = "Rate per 100,000 population",
    title = "ESSENCE: CDC COVID-Specific DD v1",
    subtitle = str_c("Month: ", as.character(x))
  ) +
  theme_void()
  
  ggsave(
    filename = str_c(
      "figures-and-data-viz/covid-by-month-",
      x,
      ".png"
    )
  )
}

map(
  .x = data_months,
  .f = ~test_function(.x)
)

library(magick)

image_read(
  path = "figures-and-data-viz/covid-by-month-1.png"
)

list.files(
    path = "figures-and-data-viz/",
    pattern = "covid-by-month-"
  ) %>%
  map(
    .f = ~image_read(
    path = str_c(
      "figures-and-data-viz/",
      as.character(.)
    )
  )) %>%
  image_join() %>%
  image_animate(fps =2) %>%
  image_write("figures-and-data-viz/covid-by-month.gif")


# crude rate of cases in each zip code by week
zip_code_count_week <- ed_df %>%
  group_by(week_year) %>%
  count(p_zip) %>%
  inner_join(
    zip_pop_denominator,
    by = c("p_zip" = "GEOID")
  ) %>%
  mutate(
    rate = 100000*(n/estimate)
  )

# spatial join between the raw count and zip code spatial data
essence_sf_week <- geo_join(
  spatial_data = zip_code_df_list,
  data_frame = zip_code_count_week,
  by_sp = "zcta5ce20",
  by_df = "p_zip",
  how = "inner"
) %>%
  filter(estimate >= 100)

# plot crude rate for each zip code in county 
ggplot() +
  geom_sf(
    data = coco_sf,
    fill = "grey"
  ) +
  geom_sf(
    data = essence_sf_week,
    mapping = aes(fill = rate)
  ) +
  facet_wrap(~week_year) +
  scale_fill_viridis_c() +
  labs(
    fill = "Rate per 100,000 population",
    title = "ESSENCE: CDC COVID-Specific DD v1",
    subtitle = "By week from 1 Jan 2022 to 31 Dec 2022"
  ) +
  theme_void()

library(transformr)

ggplot() +
  geom_sf(
    data = coco_sf,
    fill = NA,
    color = "black"
  ) +
  geom_sf(
    data = essence_sf_week,
    mapping = aes(fill = rate)
  ) +
  # facet_wrap(~week_year) +
  scale_fill_viridis_c() +
  labs(
    fill = "Rate per 100,000 population",
    title = "ESSENCE: CDC COVID-Specific DD v1",
    subtitle = "By week from 1 Jan 2022 to 31 Dec 2022"
  ) +
  theme_void() +
  transition_states(week_year)

anim_save(filename = "figures-and-data-viz/covid-by-week.gif")




coco_map <- get_stamenmap(
  bbox = c(-113.3542, 34.25841, -110.7507, 37.0031),
  maptype = "watercolor",
  zoom = 10
)

class(coco_map)
st_crs(coco_map)

class(coco_sf)
st_crs(coco_sf)

plot(st_transform(coco_sf, crs = 3857)[1], bgMap = coco_map)

ggmap(coco_map) +
  geom_sf(
    data = coco_sf,
    fill = "grey",
    inherit.aes = FALSE
  )

ed_df_map_fnct <- function(x){
  
}

unique(ed_df$week_year) %>%
  map_df(
    .f = ed_df_map_fnct
  )


# compare age and weight 
ed_df %>%
  ggplot() +
  geom_point(
    mapping = aes(
      x = as.numeric(age),
      y = weight
    )
  ) +
  facet_wrap(~hospital_name)


###################################################
# total
(ed_count_total <- ed_df %>%
  group_by(date) %>%
  summarise(n = n()) %>%
  ungroup() %>%
    complete(
      date = seq.Date(date("2022-04-01"), max(date), by = "day"),
      fill = list(n = 0),
      explicit = FALSE
    ) %>%
  alert_ewma(
    t = date,
    y = n
  ))

distinct(ed_df, hospital_name)
distinct(ed_df, ccdd_category)
glimpse(ed_df)

# total by hospital

apply_alert_to_count <- function(., x, y){
  ed_df %>%
    filter(x == y) %>%
    group_by(date) %>%
    count() %>%
    ungroup() %>%
    complete(
      date = seq.Date(min(date), max(date), by = "day"),
      fill = list(n = 0),
      explicit = FALSE
    ) %>%
    alert_ewma(
      t = date,
      y = n
    )
}

# tuba city
ed_count_total_tcrhcc <- apply_alert_to_count(
  x = ed_df$hospital_name,
  y = "AZ-Tuba City Regional Health Care Corporation"
)

ed_count_total_tcrhcc

# Banner Page
ed_count_total_bannerpage <- apply_alert_to_count(
  x = ed_df$hospital_name,
  y = "AZ-Banner Page Hospital"
)

ed_count_total_bannerpage

# FMC
ed_count_total_fmc <- apply_alert_to_count(
  x = ed_df$hospital_name,
  y = "AZ-Flagstaff Medical Center"
)

ed_count_total_fmc


# cc dd by hospital


ed_count_fmc_by_cccdd <- ed_df %>%
  filter(hospital_name == "AZ-Flagstaff Medical Center") %>%
  group_by(date, ccdd_category) %>%
  count() %>%
  ungroup() %>%
  complete(
    date = seq.Date(min(date), max(date), by = "day"),
    nesting(ccdd_category),
    fill = list(n = 0),
    explicit = FALSE
  )

count_hosp_by_ccdd <- function(., y){
  ed_df %>%
    filter(hospital_name == y) %>%
    group_by(date, ccdd_category) %>%
    count() %>%
    ungroup() %>%
    complete(
      date = seq.Date(min(date), max(date), by = "day"),
      nesting(ccdd_category),
      fill = list(n = 0),
      explicit = FALSE
    )
}

ed_count_fmc_by_cccdd <- count_hosp_by_ccdd(y = "AZ-Flagstaff Medical Center")

ed_count_tuba_by_cccdd <- count_hosp_by_ccdd(y = "AZ-Tuba City Regional Health Care Corporation")

ed_count_banner_by_cccdd <- count_hosp_by_ccdd(y = "AZ-Banner Page Hospital")

count_alert_fmc_flu <- ed_df %>%
  filter(hospital_name == "AZ-Flagstaff Medical Center" & ccdd_category == "CDC Influenza DD v1") %>%
  group_by(date) %>%
  count() %>%
  ungroup() %>%
  complete(
    date = seq.Date(date("2022-04-01"), max(date), by = "day"),
    fill = list(n = 0),
    explicit = FALSE
  ) %>%
  alert_ewma(
    t = date,
    y = n
  )

count_alert_fmc_rsv <- ed_df %>%
  filter(hospital_name == "AZ-Flagstaff Medical Center" & ccdd_category == "CDC Respiratory Syncytial Virus v1") %>%
  group_by(date) %>%
  count() %>%
  ungroup() %>%
  complete(
    date = seq.Date(date("2022-04-01"), max(date), by = "day"),
    fill = list(n = 0),
    explicit = FALSE
  ) %>%
  alert_ewma(
    t = date,
    y = n
  )

count_alert_fmc_covid <- ed_df %>%
  filter(hospital_name == "AZ-Flagstaff Medical Center" & ccdd_category == "CDC COVID-Specific DD v1") %>%
  group_by(date) %>%
  count() %>%
  ungroup() %>%
  complete(
    date = seq.Date(date("2022-04-01"), max(date), by = "day"),
    fill = list(n = 0),
    explicit = FALSE
  ) %>%
  alert_ewma(
    t = date,
    y = n
  )


ggplot(
  mapping = aes(
    x = date,
    y = n
  )
) +
  geom_line(
    data = count_alert_fmc_flu,
    color = "grey"
  ) +
  geom_line(
    data = subset(count_alert_fmc_flu, alert != "grey"),
    mapping = aes(x = date, y = n),
    color = "navy"
  ) +

  geom_line(data = subset(df_ewma_region, alert != "grey"), aes(x = date, y = dataCount), color = "navy") +
  geom_point(data = subset(df_ewma_region, alert == "blue"), aes(x = date, y = dataCount), color = "navy") +
  geom_point(data = subset(df_ewma_region, alert == "yellow"), aes(x = date, y = dataCount), color = "yellow") +
  geom_point(data = subset(df_ewma_region, alert == "red"), aes(x = date, y = dataCount), color = "red") +
  theme_bw() +
  labs(x = "Date", y = "Count")


ed_df %>%
  filter(hospital_name == "AZ-Flagstaff Medical Center") %>%
  group_by(date, ccdd_category) %>%
  count() %>%
  ungroup() %>%
  pivot_wider(
    names_from = ccdd_category,
    values_from = n
  ) %>%
  pivot_longer(
    cols = c("CDC Influenza DD v1",
           "CDC Respiratory Syncytial Virus v1",
           "CDC COVID-Specific DD v1"),
    names_to = "ccdd_category",
    values_to = "n"
  ) %>%
  replace_na(
    replace = list(n = 0)
  ) %>%
  alert_ewma(
    t = date,
    y = n
  )


# RSV by hospital


# COVID by hospital
ed_count_alert <- alert_ewma(
  df = ed_count,
  t = date,
  y = n
)

df_ewma_region %>%
  ggplot() +
  geom_line(aes(x = date, y = dataCount), color = "grey70") +
  geom_line(data = subset(df_ewma_region, alert != "grey"), aes(x = date, y = dataCount), color = "navy") +
  geom_point(data = subset(df_ewma_region, alert == "blue"), aes(x = date, y = dataCount), color = "navy") +
  geom_point(data = subset(df_ewma_region, alert == "yellow"), aes(x = date, y = dataCount), color = "yellow") +
  geom_point(data = subset(df_ewma_region, alert == "red"), aes(x = date, y = dataCount), color = "red") +
  theme_bw() +
  labs(x = "Date", y = "Count")

# test_count <- ed_df %>%
#   filter(hospital_name == "AZ-Banner Page Hospital") %>%
#   group_by(date) %>%
#   summarise(n = n()) %>%
#   ungroup() %>%
#   complete(
#     date = seq.Date(min(date), max(date), by = "day"),
#     fill = list(n = 0),
#     explicit = FALSE
#   ) %>%
#   alert_ewma(
#     t = date,
#     y = n
#   )
#
# test_count %>%
#   ggplot(
#     mapping = aes(
#       x = date,
#       y = n
#     )
#   ) +
#   geom_line() +
#   ylim(0,NA)
#
