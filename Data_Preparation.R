## Data preparation


library(tidyverse)
library(readr)
library(tidyverse)
library(stringr)

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





# save data as dataset called "training_data_cleaned.csv" as csv file

#write_csv(data, "training_data_cleaned.csv")
