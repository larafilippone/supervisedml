# Data

data <- data %>%
  filter(between(area, 20,300) | is.na(area)) 


data <- data %>%
  mutate(rent_per_sqm = rent_full / area) %>%
  # delete rows with unusually low values
  filter(between(rent_per_sqm, 8, 60) | is.na(rent_per_sqm)) %>%
  select(-rent_per_sqm)

