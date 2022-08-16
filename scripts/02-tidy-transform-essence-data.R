source("scripts/01-query-read-essence-data.R", echo = TRUE)

# tidy
essence_data <- api_data_details$dataDetails %>%
  clean_names() %>%
  as_tibble() %>%
  mutate(
    date = mdy(date)
  )

glimpse(essence_data)

# transform 
essence_data <- essence_data %>%
  distinct(
    date,
    hospital_name,
    sex,
    age,
    medical_record_number,
    c_unique_patient_id,
    .keep_all = TRUE
  ) %>%
  select(
    date,
    week_year,
    month_year,
    year,
    hospital_name,
    region,
    chief_complaint_orig,
    chief_complaint_parsed,
    category_flat,
    sub_category_flat,
    discharge_diagnosis,
    c_patient_county,
    patient_state,
    patient_country,
    sex,
    age,
    age_group,
    c_ethnicity,
    c_race,
    c_race_c_eth_combined_narrow,
    c_race_c_eth_combined_broad,
    admit_date_time,
    admit_reason_code,
    admit_reason_combo,
    ccdd_category_flat,
    ccdd_category
  ) 


# library(zoo)
# 
# date_time_series <- zoo(
#   order.by = seq.Date(min((essence_data$date)), max((essence_data$date)),
#                       by = "days"
#   ))
# 
# date_time_series <- as.POSIXct(date_time_series)
# 
# date_time_series <- tibble(date_time_series) %>%
#   transmute(
#     date = date(date_time_series)
#   )
# 
# test_data <- full_join(
#   x = date_time_series,
#   y = essence_data,
#   keep = TRUE
# )
