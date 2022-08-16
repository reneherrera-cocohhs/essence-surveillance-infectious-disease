source("scripts/03-summary-counts-alerts.R")


# Total for all hospitals and all ccdd categories combined 
ed_count_total %>%
  ggplot(
    mapping = aes(
      x = date,
      y = n
    )
  ) +
  geom_line(color = "grey70") +
  geom_point(
    data = subset(ed_count_alert, alert != "grey"),
    color = "navy"
  ) +
  geom_point(
    data = subset(ed_count_alert, alert == "blue"),
    color = "navy"
  ) +
  geom_line(
    data = subset(ed_count_alert, alert != "grey"),
    color = "navy"
  ) +
  geom_point(
    data = subset(ed_count_alert, alert == "yellow"),
    color = "yellow"
  ) +
  geom_point(
    data = subset(ed_count_alert, alert == "red"),
    color = "red"
  ) +
  ylim(0,NA) +
  labs(
    x = "Date",
    y = "Count",
    title = "Infectious Disease ED Visits in Coconino County (2022)",
    subtitle = "Including: Influenza, COVID-19, RSV",
    caption = "Hospital: AZ Banner Page, FMC, & Tuba City. CC DD Category: CDC COVID-Specific DD v1, CDC Influenza DDv1, CDC Respiratory Synctial Virus v1."
  ) +
  theme_classic()

# Tuba City
ed_count_total_tcrhcc %>%
  ggplot(
    mapping = aes(
      x = date,
      y = n
    )
  ) +
  geom_line(color = "grey70") +
  geom_line(
    data = subset(ed_count_total_tcrhcc, alert != "grey"),
    color = "navy"
  ) +
  geom_point(
    data = subset(ed_count_total_tcrhcc, alert == "blue"),
    color = "navy"
  ) +
  geom_point(
    data = subset(ed_count_total_tcrhcc, alert == "yellow"),
    color = "yellow"
  ) +
  geom_point(
    data = subset(ed_count_total_tcrhcc, alert == "red"),
    color = "red"
  ) +
  ylim(0,NA) +
  labs(
    x = "Date",
    y = "Count",
    title = "Infectious Disease ED Visits at Tuba City RHCC (2022)",
    subtitle = "Including: Influenza, COVID-19, RSV",
    caption = "Source: Essence API; Where CC DD Category: CDC COVID-Specific DD v1, CDC Influenza DDv1, CDC Respiratory Synctial Virus v1."
  ) +
  theme_classic()

# Tuba City by CC DD Category 
ed_count_tuba_by_cccdd %>%
  ggplot(
    mapping = aes(
      x = date,
      y = n,
      group = ccdd_category,
      color = ccdd_category
    )
  ) +
  geom_line() +
  ylim(0,NA) +
  labs(
    x = "Date",
    y = "Count",
    title = "Infectious Disease ED Visits at Tuba City RHCC (2022)",
    subtitle = "Including: Influenza, COVID-19, RSV",
    caption = "Source: Essence API; Where CC DD Category: CDC COVID-Specific DD v1, CDC Influenza DDv1, CDC Respiratory Synctial Virus v1.",
    color = "CC DD Category"
  ) +
  theme_classic()


# Banner Page
ed_count_total_bannerpage %>%
  ggplot(
    mapping = aes(
      x = date,
      y = n
    )
  ) +
  geom_line(color = "grey70") +
  geom_line(
    data = subset(ed_count_total_bannerpage, alert != "grey"),
    color = "navy"
  ) +
  geom_point(
    data = subset(ed_count_total_bannerpage, alert == "blue"),
    color = "navy"
  ) +
  geom_point(
    data = subset(ed_count_total_bannerpage, alert == "yellow"),
    color = "yellow"
  ) +
  geom_point(
    data = subset(ed_count_total_bannerpage, alert == "red"),
    color = "red"
  ) +
  ylim(0,NA) +
  labs(
    x = "Date",
    y = "Count",
    title = "Infectious Disease ED Visits at Banner Page (2022)",
    subtitle = "Including: Influenza, COVID-19, RSV",
    caption = "Source: Essence API; Where CC DD Category: CDC COVID-Specific DD v1, CDC Influenza DDv1, CDC Respiratory Synctial Virus v1."
  ) +
  theme_classic()

# Banner Page by CC DD Category 
ed_count_banner_by_cccdd %>%
  ggplot(
    mapping = aes(
      x = date,
      y = n,
      group = ccdd_category,
      color = ccdd_category
    )
  ) +
  geom_line() +
  ylim(0,NA) +
  labs(
    x = "Date",
    y = "Count",
    title = "Infectious Disease ED Visits at Banner Page (2022)",
    subtitle = "Including: Influenza, COVID-19, RSV",
    caption = "Source: Essence API; Where CC DD Category: CDC COVID-Specific DD v1, CDC Influenza DDv1, CDC Respiratory Synctial Virus v1.",
    color = "CC DD Category"
  ) +
  theme_classic()



# FMC
ed_count_total_fmc %>%
  ggplot(
    mapping = aes(
      x = date,
      y = n
    )
  ) +
  geom_line(color = "grey70") +
  geom_line(
    data = subset(ed_count_total_fmc, alert != "grey"),
    color = "navy"
  ) +
  geom_point(
    data = subset(ed_count_total_fmc, alert == "blue"),
    color = "navy"
  ) +
  geom_point(
    data = subset(ed_count_total_fmc, alert == "yellow"),
    color = "yellow"
  ) +
  geom_point(
    data = subset(ed_count_total_fmc, alert == "red"),
    color = "red"
  ) +
  ylim(0,NA) +
  labs(
    x = "Date",
    y = "Count",
    title = "Infectious Disease ED Visits at Flagstaff Medical (2022)",
    subtitle = "Including: Influenza, COVID-19, RSV",
    caption = "Source: Essence API; Where CC DD Category: CDC COVID-Specific DD v1, CDC Influenza DDv1, CDC Respiratory Synctial Virus v1."
  ) +
  theme_classic()

# FMC by CC DD Category 
ed_count_fmc_by_cccdd %>%
  ggplot(
    mapping = aes(
      x = date,
      y = n,
      group = ccdd_category,
      color = ccdd_category
    )
  ) +
  geom_line() +
  ylim(0,NA) +
  labs(
    x = "Date",
    y = "Count",
    title = "Infectious Disease ED Visits at Flagstaff Medical (2022)",
    subtitle = "Including: Influenza, COVID-19, RSV",
    caption = "Source: Essence API; Where CC DD Category: CDC COVID-Specific DD v1, CDC Influenza DDv1, CDC Respiratory Synctial Virus v1.",
    color = "CC DD Category"
  ) +
  theme_classic()

