## Data preparation

library(rstatix)
library(moments)
library(tidyverse)
library(readr)
library(stargazer)
library(caret)

data <- read_csv("training_data.csv")

# Excluding all variables with only na's

data <- data %>%
  select(-address, -...1, -GDENR, -appartments, -bath_tube, -garden_m2, -ceiling,  -minergie, -pets, -shared_flat, -shopping, -size_land, 
         -wheelchair) %>% # Excluding all columns with only NA`s
  select(-bath, -gardenshed, -heating_air, -heating_earth, -heating_electro, -heating_far, 
  -heating_gas, -heating_oil, -heating_pellets,  -manlift, -middle_house, -oven,
  -pool, -quiet, -shower, -sunny, -terrace, -veranda, -water) %>% # less than 100 observations without NA`s
  select(-toilets) # nonsense variables

#descriptives
summary(data$rent_full)
hist(data$rent_full, main="Distribution of rent_full", xlab="rent", col="lightblue", breaks=30)
summary(data$rent_full)
skewness(data$rent_full)

summary(data$area)
hist(data$area, main="Distribution of area", xlab="area (squared meters)", col="lightblue", breaks=45)
summary(data$area)
skewness(data$area)

df_general <- data %>%
  select(rent_full, area, month, year_built, rooms, Micro_rating, apoth_pix_count_km2,restaur_pix_count_km2,superm_pix_count_km2,
         wgh_avg_sonnenklasse_per_egid,dist_to_4G,dist_to_highway, dist_to_lake, dist_to_main_stat)

#Pearson's correlation
#df_general %>%
  #cor_mat() %>%
  #gather(-rowname, key = cor_var, value = r) %>%
  #ggplot(aes(rowname, cor_var, fill = r)) +
  #geom_tile() +
  #geom_text(aes(label = round(r, 2)), color = "black", size = 3) +
  #theme_minimal() +
  #scale_fill_gradient(low = "lightblue", high = "blue") +
  #labs(title = "Correlation matrix of the main variables")+ylab("")+xlab("")+
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.4))  # Adjust angle for slant

#Spearman's correlation
df_general %>%
  cor(method = "spearman", use = "complete.obs") %>%  # Use complete observations
  as.data.frame() %>%
  rownames_to_column(var = "rowname") %>%
  gather(-rowname, key = cor_var, value = r) %>%
  ggplot(aes(rowname, cor_var, fill = r)) +
  geom_tile() +
  geom_text(aes(label = ifelse(is.na(r), "", round(r, 2))), color = "black", size = 3) +  # Handle NA values in labels
  theme_minimal() +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  labs(title = "Correlation matrix of the main variables", y = "", x = "") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.4))

ggplot(data, aes(x = rent_full, y = area)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(x = "Rent", y = "Area")+ggtitle("Relationship between rent and area")

# Dealing with NA`s

data %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_share") %>%
  filter(na_share>0) %>%
  view()

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

# topstorage: fill up with 0. Not significant.
data[is.na(data$topstorage),"topstorage"]<-0
lmstorage<-lm(data$rent_full~data$topstorage)
stargazer(lmstorage, type="text")

# year: checking that everything refers to year 2019.
unique(data$year)

# year_built: checking for unusually old houses.
# Some descriptions seem to make sense, we could leave this variable.
range(data$year_built, na.rm=T)
summary(data$year_built)
sorted_year_built <- data[order(data$year_built), ]
head(sorted_year_built$year_built, 20)
head(sorted_year_built$descr, 20)

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

# Noise_max: checking the range. Would not include it, doesn't have a significant impact on rent.
range(data$Noise_max)
lmnoise<-lm(data$rent_full~data$Noise_max)
stargazer(lmnoise, type="text")

# apoth_pix_count_2km: checking the range.
range(data$apoth_pix_count_km2)
#significant impact on rent-->include this variable.
lmapo<-lm(data=data, rent_full~apoth_pix_count_km2)
stargazer(lmapo, type="text")

# avg_bauperiode: range is ok, useful when age of construction is missing.
range(data$avg_bauperiode, na.rm=T)
#imputing missing values using the mean.
data$avg_bauperiode <- ifelse(is.na(data$avg_bauperiode), mean(data$avg_bauperiode,na.rm=T), data$avg_bauperiode)
summary(data$avg_bauperiode)

# dist_to_4G: this might be relevant. I am not sure about the unit of measure, but as expected 0 has the highest observed frequency.
range(data$dist_to_4G)
summary(data$dist_to_4G)
hist(data$dist_to_4G)
lm4g<-lm(data=data, rent_full~dist_to_4G)
stargazer(lm4g, type="text")

# dist_to_5G: I wouldn't include this var because I don't think 5G is relevant.
range(data$dist_to_5G)

# dist_to_highway: I think these distances are all expressed in meters.
range(data$dist_to_highway)
summary(data$dist_to_highway)
#significant impact on rent-->include this variable.
lmhigh<-lm(data=data, rent_full~dist_to_highway)
stargazer(lmhigh, type="text")

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

# feature engeneering frederik
#rm(list=ls())
# load dataset training_data_cleaned.csv

#data <- read.csv("training_data_cleaned.csv", sep = ",", header = TRUE)

# add feature area/ rooms

data$area_per_room <- data$area / data$rooms


# datatype conversions

# convert date from chr to date

data$date <- as.Date(data$date, format = "%d.%m.%Y")

# add feature on whether date at the beginning or end of the month
data$date <- as.Date(data$date, format = "%Y-%m-%d")

data$day <- as.numeric(format(data$date, "%d"))
data$month <- as.numeric(format(data$date, "%m"))
# add feature on the weekday and assign levels from 0 to 7
data$weekday <- as.numeric(format(data$date, "%u"))

#dropping old date var
data <- data %>%
  select(-date)

#creating dummies for description
# List of words to check for

words_to_check <- c("renoviert", "saniert", "renovated", "rinnovat", "ristrutturat", "rénové", "renové", "rénove", "renove",
                    
                    "sanierung", "renovierung", "renovation", "rénovation", "ristrutturazione")



# Create a single dummy variable
data$renovated <- ifelse(rowSums(sapply(words_to_check, function(word) grepl(word, data$descr, ignore.case = TRUE))) > 0, 1, 0)

# Check how many rows have been assigned to 1
words_to_check <- c("sonnig", "sunny", "soleggiat", "ensoleillé", "ensoleille", "luminos", "lumineux", "lumineuse")

# Create a single dummy variable
data$is_sunny <- ifelse(rowSums(sapply(words_to_check, function(word) grepl(word, data$descr, ignore.case = TRUE))) > 0, 1, 0)

#dropping old descr var
data <- data %>%
  select(-descr)

# include a variable city_50k with one for all rows where GDNMK is in the vector citylist
city_list <- c("Zürich", "Genf", "Basel", "Lausanne", "Bern", "Winterthur", "Luzern", "St. Gallen", "Lugano", "Biel/Bienne")

data$city_50k <- ifelse(data$GDENAMK %in% city_list, 1, 0)
rm(city_list)

# remove near zero variance variables
nzv <- nearZeroVar(data)
data <- data[, -nzv]

#knn imputation

# remove correlated predictors
data_numeric <- data[, -which(names(data) %in% c("GDENAMK", "KTKZ"))]
correlated_predictors <- findCorrelation(cor(data_numeric), cutoff = 0.85)
print(correlated_predictors)
#of course months are correlated with the quarter, I wouldn't worry about it.

# checking for linear dependencies: this means that these variables can be expressed as a linear combination of each other.
linear_dependencies <- findLinearCombos(data_numeric)
print(linear_dependencies)
#It makes sense, because the micro_rating is based on the other subratings. But I don't think this is an issue.
#If we want, we can run models without the micro_rating but the substance won't change, this is more related to efficiency.

#Dropping quarter_specific var
data <- data %>%
  select(-quarter_specific)

#FOR THE MOMENT, I ALSO DROP "msregion" and "GDENAMK".
data <- data %>%
  select(-GDENAMK)

#transforming KTKZ in dummy variables.
# Create dummy variables for KTKZ column
dummies <- model.matrix(~ KTKZ - 1, data = data)
dummies_df <- as.data.frame(dummies)

# Add the dummy variables to the data dataframe
data <- bind_cols(data, dummies_df)

# Remove the column
data <- select(data, -KTKZ)

#transforming msregion in dummy variables.
# Create dummy variables for msregion column
dummiesms <- model.matrix(~ msregion - 1, data = data)
dummiesms_df <- as.data.frame(dummiesms)

# Add the dummy variables to the data dataframe
data <- bind_cols(data, dummiesms_df)

# Remove the column
data <- select(data, -msregion)

# normalization of the predictors: this function is for z-score normalization.
#data <- preProcess(data, method = c("center", "scale")) # eg. Normalizing and scaling (means what?

#write_csv(data,"training_data_preprocessed.csv")
