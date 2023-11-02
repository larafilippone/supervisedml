## Data preparation


library(tidyverse)


# Excluding all variables with only na's

data <- data %>%
  select(-appartments, -bath_tube, -garden_m2, -ceiling,  -minergie, -pets, -shared_flat, -shopping, -size_land, 
         -wheelchair) %>% # Excluding all columns with only NA`s
  select(-bath, -gardenshed, -heating_air, -heating_earth, -heating_electro, -heating_far, 
  -heating_gas, -heating_oil, -heating_pellets,  -manlift, -middle_house, -oven,
  -pool, -quiet, -shower, -sunny, -terrace, -veranda, -water) %>% # less than 100 observations without NA`s
  select(-toilets) # nonsense variables

# Dealing with NA`s

# area : create avarages for room numbers and then fill up for all NA'S and values below 5 sqm

# adding features to break down the date (eg. published at the start of a month, etc.)

# year build: figure out featurs (eg Build before 1800)

# additional step: where necessary, change numerical variables to categorical ones

# Checking for duplicates

# 

