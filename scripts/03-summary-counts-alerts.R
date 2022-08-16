source("scripts/02-tidy-transform-essence-data.R", echo = TRUE)

# total 
(ed_count_total <- essence_data %>%
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

distinct(essence_data, hospital_name)
distinct(essence_data, ccdd_category)
glimpse(essence_data)

# total by hospital 

apply_alert_to_count <- function(., x, y){
  essence_data %>%
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
  x = essence_data$hospital_name,
  y = "AZ-Tuba City Regional Health Care Corporation"
)

ed_count_total_tcrhcc

# Banner Page 
ed_count_total_bannerpage <- apply_alert_to_count(
  x = essence_data$hospital_name,
  y = "AZ-Banner Page Hospital"
)

ed_count_total_bannerpage

# FMC
ed_count_total_fmc <- apply_alert_to_count(
  x = essence_data$hospital_name,
  y = "AZ-Flagstaff Medical Center"
)

ed_count_total_fmc


# cc dd by hospital 


ed_count_fmc_by_cccdd <- essence_data %>%
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
  essence_data %>%
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

count_alert_fmc_flu <- essence_data %>%
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

count_alert_fmc_rsv <- essence_data %>%
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

count_alert_fmc_covid <- essence_data %>%
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


essence_data %>%
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

# test_count <- essence_data %>%
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
