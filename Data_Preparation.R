## Data preparation


library(tidyverse)
library(readr)

data <- read_csv("training_data.csv")

# Excluding all variables with only na's

data <- data %>%
  select(-appartments, -bath_tube, -garden_m2, -ceiling,  -minergie, -pets, -shared_flat, -shopping, -size_land, 
         -wheelchair) %>% # Excluding all columns with only NA`s
  select(-bath, -gardenshed, -heating_air, -heating_earth, -heating_electro, -heating_far, 
  -heating_gas, -heating_oil, -heating_pellets,  -manlift, -middle_house, -oven,
  -pool, -quiet, -shower, -sunny, -terrace, -veranda, -water) %>% # less than 100 observations without NA`s
  select(-toilets) # nonsense variables

# Dealing with NA`s

data %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_share") %>%
  filter(na_share>0) %>%
  view()

# area : create avarages for room numbers and then fill up for all NA'S and values below 5 sqm

area_means = data %>% 
  group_by(rooms) %>%
  summarize(mean = mean(area, na.rm=T)) 

data <- left_join(data, area_means, by="rooms")

for (i in 1:nrow(data)) {
  if (is.na(data[i,"area"]) == TRUE & (is.na(data[i,"rooms"]) == TRUE) ) {
    data[i,"area"] <- mean(data$area, na.rm=T)
  }
  if (!is.na(data[i, "area"]) && data[i,"area"] < 5) {
    data[i,"area"] <- mean(data$area, na.rm=T)
  }
  if (is.na(data[i,"area"]) == TRUE) {
    data[i,"area"] <- data[i,"mean"]
  }
}

data <- data %>%
          select(-mean)

# Area_usable: replace NA's with area

for (i in 1:nrow(data)) {
  if (is.na(data[i,"area_useable"]) == TRUE) {
    data[i,"area_useable"] <- data[i,"area"]
  }
}

# balcony: replace NA's with 0

data[is.na(data$balcony),"balcony"] <- 0

# basement: replace NA's with 0

data[is.na(data$basement),"basement"] <- 0

# cabletv: replace NA's with 0

data[is.na(data$cabletv),"cabletv"] <- 0

# cheminee: replace NA's with 0

data[is.na(data$cheminee),"cheminee"] <- 0

# descr: replace NA's with ""

data[is.na(data$descr),"descr"] <- ""

# dishwasher: replace NA's with 0

data[is.na(data$dishwasher),"dishwasher"] <- 0

# dryer: replace NA's with 0

data[is.na(data$dryer),"dryer"] <- 0

# elevator: replace NA's with 0

data[is.na(data$elevator),"elevator"] <- 0

#floors: fill up with mean

data[is.na(data$floors),"floors"] <- mean(data$floors, na.rm=T)

# furnished: replace NA's with 0

data[is.na(data$furnished),"furnished"] <- 0

# kids_friendly: replace NA's with 0

data[is.na(data$kids_friendly),"kids_friendly"] <- 0

# laundry: replace NA's with 0

data[is.na(data$laundry),"laundry"] <- 0

# oldbuilding: fill up with 0

data[is.na(data$oldbuilding),"oldbuilding"] <- 0

# parking_indoor: fill up with 0

data[is.na(data$parking_indoor),"parking_indoor"] <- 0

# parking_outside: fill up with 0

data[is.na(data$parking_outside),"parking_outside"] <- 0

# playground: fill up with 0

data[is.na(data$playground),"playground"] <- 0

# quarter_general and quarter_specific: checking that there aren't any meaningless values.

unique(data$quarter_general)
unique(data$quarter_specific)

# raised_groundfloor: fill up with 0.

data[is.na(data$raised_groundfloor),"raised_groundfloor"] <- 0

# rent_full: checking for NAs.

na_rent <- sum(is.na(data$rent_full))
print(na_rent)

# rooms: replacing NAs with the average household size.

data$rooms <- ifelse(is.na(data$rooms), data$Avg_size_household, data$rooms)

# topstorage: fill up with 0.

data[is.na(data$topstorage),"topstorage"]<-0

# year: checking that everything refers to year 2019.

unique(data$year)

# year_built: checking for unusually old houses.

range(data$year_built, na.rm=T)
summary(data$year_built)
sorted_year_built <- data[order(data$year_built), ]
head(sorted_year_built$year_built, 20)
head(sorted_year_built$descr, 20)

# replacing NAs with averages of the neighborhood (avg_bauperiode).

data$year_built <- ifelse(is.na(data$year_built), data$avg_bauperiode, data$year_built)
summary(data$year_built)

# Some descriptions seem to make sense, we could leave this variable as it is.

# Micro_rating: checking the range (should be between 0 and 10).

range(data$Micro_rating, na.rm=T)

# Micro_rating_NoiseAndEmission: checking the range (should be between 0 and 10).

range(data$Micro_rating_NoiseAndEmission, na.rm=T)

# Micro_rating_Accessibility: checking the range (should be between 0 and 10).

range(data$Micro_rating_Accessibility, na.rm=T)

# Micro_rating_DistrictAndArea: checking the range (should be between 0 and 10).

range(data$Micro_rating_DistrictAndArea, na.rm=T)

# Micro_rating_SunAndView: checking the range (should be between 0 and 10).

range(data$Micro_rating_SunAndView, na.rm=T)

# Micro_Rating_ServicesAndNature: checking the range (should be between 0 and 10).

range(data$Micro_rating_ServicesAndNature, na.rm=T)

# wgh_avg_sonnenklasse_per_egid: checking the range. It should refer to solar energy (?).

range(data$wgh_avg_sonnenklasse_per_egid, na.rm=T)

# Anteil_auslaend: checking the range.

range(data$Anteil_auslaend, na.rm=T)

# Avg_age: checking the range.

range(data$Avg_age, na.rm=T)

# Avg_size_household: checking the range. It refers to rooms.

range(data$Avg_size_household, na.rm=T)

# Noise_max: checking the range.

range(data$Noise_max, na.rm=T)

# anteil_efh: checking the range. It refers to percentage of detatched houses.

range(data$anteil_efh, na.rm=T)

# apoth_pix_count_2km: checking the range.

range(data$apoth_pix_count_km2, na.rm=T)

# avg_anzhl_geschosse: checking the range.

range(data$avg_anzhl_geschosse, na.rm=T)

# avg_bauperiode: range is ok, useful when age of construction is missing.

range(data$avg_bauperiode, na.rm=T)

# dist_to_4G: this might be relevant. I am not sure about the unit of measure, but as expected 0 has the highest observed frequency.

range(data$dist_to_4G)
summary(data$dist_to_4G)
hist(data$dist_to_4G)

# dist_to_5G: I wouldn't include this var because I don't think 5G is relevant.

range(data$dist_to_5G)

# dist_to_halst: not sure what this variable means.

range(data$dist_to_haltst, na.rm=T)
summary(data$dist_to_haltst)
hist(data$dist_to_haltst)

# dist_to_highway: I think these distances are all expressed in meters.

range(data$dist_to_highway)

# dist_to_lake: too many NAs, maybe substitute according to the city. Also, max values aren't accurate.

range(data$dist_to_lake, na.rm = T)
summary(data$dist_to_lake)

# dist_to_main_stat: not sure if max values are plausible.

range(data$dist_to_main_stat, na.rm = T)
summary(data$dist_to_main_stat)
hist(data$dist_to_main_stat)

# dist_to_school_1: this should be ok.

range(data$dist_to_school_1, na.rm=T)
summary(data$dist_to_school_1)

# dist_to_train_stat: checking range, seems fine.

range(data$dist_to_train_stat)
summary(data$dist_to_train_stat)
hist(data$dist_to_train_stat)

# restaur_pix_count_km2: checking range and distribution.

range(data$restaur_pix_count_km2)
summary(data$restaur_pix_count_km2)
hist(data$restaur_pix_count_km2)

# superm_pix_count_km2: checking range.

range(data$superm_pix_count_km2)

# dist_to_river: checking range and distribution.

range(data$dist_to_river)
summary(data$dist_to_river)
hist(data$dist_to_river)

# Checking for duplicates: there aren't any.
duplicates <- duplicated(data)
print(data[duplicates, ])
