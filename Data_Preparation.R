## Data preparation


library(tidyverse)
library(readr)
library(stargazer)

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

# playground: fill up with 0. Not significant. Not included in models.
data[is.na(data$playground),"playground"] <- 0
lmplay<-lm(data$rent_full~data$playground)
stargazer(lmplay, type="text")

# quarter_general and quarter_specific: checking that there aren't any meaningless values. We should only include one of them, because they are equal.
unique(data$quarter_general)
unique(data$quarter_specific)
lmquarter<-lm(data$rent_full~data$quarter_general)
stargazer(lmquarter, type="text")

# raised_groundfloor: fill up with 0. Significant, we could include it.
data[is.na(data$raised_groundfloor),"raised_groundfloor"] <- 0
lmfloor<-lm(data$rent_full~data$raised_groundfloor)
stargazer(lmfloor, type="text")

# rent_full: checking for NAs and that the range is reasonable.
na_rent <- sum(is.na(data$rent_full))
print(na_rent)
summary(data$rent_full)
sorted_rent <- data[order(data$rent_full), ]
head(sorted_rent$rent_full, 20)
head(sorted_rent$descr, 20) #descriptions seem to match

# rooms: replacing NAs with the average household size. Include this variable in models.
data$rooms <- ifelse(is.na(data$rooms), data$Avg_size_household, data$rooms)
lmrooms<-lm(data$rent_full~data$rooms)
stargazer(lmrooms, type="text")

# topstorage: fill up with 0.
data[is.na(data$topstorage),"topstorage"]<-0

# year: checking that everything refers to year 2019.
unique(data$year)

# year_built: checking for unusually old houses.
# Some descriptions seem to make sense, we could leave this variable.
range(data$year_built, na.rm=T)
summary(data$year_built)
sorted_year_built <- data[order(data$year_built), ]
head(sorted_year_built$year_built, 20)
head(sorted_year_built$descr, 20)

# replacing NAs with averages of the neighborhood (avg_bauperiode).
data$year_built <- ifelse(is.na(data$year_built), data$avg_bauperiode, data$year_built)
summary(data$year_built)
#significant impact.
lmyear<-lm(data$rent_full~data$year_built)
stargazer(lmyear, type="text")

# Micro_rating: checking the range (should be between 0 and 10). We can just include this one. It is significant.
range(data$Micro_rating)
lmrating<-lm(data$rent_full~data$Micro_rating)
stargazer(lmrating, type="text")

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

# wgh_avg_sonnenklasse_per_egid: checking the range. It should refer to solar energy (?). Could include it because it has a significant impact. Probabily 4=worst class.
range(data$wgh_avg_sonnenklasse_per_egid, na.rm=T)
lmsonnenklasse<-lm(data$rent_full~data$wgh_avg_sonnenklasse_per_egid)
stargazer(lmsonnenklasse, type="text")

# Anteil_auslaend: checking the range. Include this variable, it has a significant impact.
range(data$Anteil_auslaend, na.rm=T)
lmauslaend<-lm(data$rent_full~data$Anteil_auslaend)
stargazer(lmauslaend, type="text")

# Avg_age: checking the range. Would not include it, no significant impact.
range(data$Avg_age, na.rm=T)
lmage<-lm(data$rent_full~data$Avg_age)
stargazer(lmage, type="text")

# Avg_size_household: checking the range. It refers to rooms. I would rather include the actual size for each house.
range(data$Avg_size_household, na.rm=T)

# Noise_max: checking the range. Would not include it, doesn't have a sinificant impact on rent.
range(data$Noise_max)
lmnoise<-lm(data$rent_full~data$Noise_max)
stargazer(lmnoise, type="text")

# anteil_efh: checking the range. It refers to percentage of detatched houses. I wouldn't include this variable.
range(data$anteil_efh, na.rm=T)
summary(data$anteil_efh)

# apoth_pix_count_2km: checking the range.
range(data$apoth_pix_count_km2)
#significant impact on rent-->include this variable.
lmapo<-lm(data=data, rent_full~apoth_pix_count_km2)
stargazer(lmapo, type="text")

# avg_anzhl_geschosse: checking the range. I am not sure what this variable is.
range(data$avg_anzhl_geschosse, na.rm=T)

# avg_bauperiode: range is ok, useful when age of construction is missing.
range(data$avg_bauperiode, na.rm=T)

# dist_to_4G: this might be relevant. I am not sure about the unit of measure, but as expected 0 has the highest observed frequency.
range(data$dist_to_4G)
summary(data$dist_to_4G)
hist(data$dist_to_4G)
lm4g<-lm(data=data, rent_full~dist_to_4G)
stargazer(lm4g, type="text")

# dist_to_5G: I wouldn't include this var because I don't think 5G is relevant.
range(data$dist_to_5G)

# dist_to_halst: not sure what this variable means. But it has a significant impact, we could include it.
range(data$dist_to_haltst, na.rm=T)
summary(data$dist_to_haltst)
hist(data$dist_to_haltst)
lmhaltst<-lm(data=data, rent_full~dist_to_haltst)
stargazer(lmhaltst, type="text")

# dist_to_highway: I think these distances are all expressed in meters.
range(data$dist_to_highway)
summary(data$dist_to_highway)
#significant impact on rent-->include this variable.
lmhigh<-lm(data=data, rent_full~dist_to_highway)
stargazer(lmhigh, type="text")

# dist_to_lake: too many NAs, maybe substitute according to the city. Also, max values aren't accurate.
range(data$dist_to_lake, na.rm = T)
summary(data$dist_to_lake)

# dist_to_main_stat: not sure if max values are plausible. Not sure if NAs are too many.
range(data$dist_to_main_stat, na.rm = T)
summary(data$dist_to_main_stat)
hist(data$dist_to_main_stat)
#significant impact on rent-->include this variable.
lmmain<-lm(data=data, rent_full~dist_to_main_stat)
stargazer(lmmain, type="text")

# dist_to_school_1: this should be ok.
range(data$dist_to_school_1, na.rm=T)
summary(data$dist_to_school_1)
#significant impact on rent-->we should include it.
lmschool<-lm(data=data, rent_full~dist_to_school_1)
stargazer(lmschool, type="text")

# dist_to_train_stat: checking range, seems fine.
range(data$dist_to_train_stat)
summary(data$dist_to_train_stat)
hist(data$dist_to_train_stat)
#significant impact on rent-->we should include this variable.
lmtrain<-lm(data=data, rent_full~dist_to_train_stat)
stargazer(lmtrain, type="text")

# restaur_pix_count_km2: checking range and distribution.
range(data$restaur_pix_count_km2)
summary(data$restaur_pix_count_km2)
hist(data$restaur_pix_count_km2)
#significant impact on rent-->we should include this variable.
lmrest<-lm(data=data, rent_full~restaur_pix_count_km2)
stargazer(lmrest, type="text")

# superm_pix_count_km2: checking range.
range(data$superm_pix_count_km2)
#significant impact on rent-->include this variable.
lmsupermarket<-lm(data = data, rent_full~superm_pix_count_km2)
stargazer(lmsupermarket, type="text")

# dist_to_river: checking range and distribution.
range(data$dist_to_river)
summary(data$dist_to_river)
hist(data$dist_to_river)

# It has a significant impact on the price-->we should include this var.
lmriver<-lm(data = data, rent_full~dist_to_river)
stargazer(lmriver, type="text")

# Checking for duplicates: there aren't any.
duplicates <- duplicated(data)
print(data[duplicates, ])