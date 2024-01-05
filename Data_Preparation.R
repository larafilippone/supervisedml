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

skewness(data$Avg_size_household)

# area : create averages for room numbers and then fill up for all NA'S and values below 5 sqm
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

#check
table(data$area<data$area_useable)
#area cannot be smaller than the usable area, so we set the usable area for those observations as equal to the area.
data <- within(data, {
  area_useable[area < area_useable] <- area[area < area_useable]
})

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

data$floors <- ifelse(is.na(data$floors), median(data$floors,na.rm = T), data$floors)
summary(data$floors)
unique(data$floors)

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

# rooms: replacing NAs with mean. Include this variable in models.
summary(data$rooms)
data$rooms <- ifelse(is.na(data$rooms), mean(data$rooms,na.rm=T), data$rooms)
lmrooms<-lm(data$rent_full~data$rooms)
stargazer(lmrooms, type="text")

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

# replacing NAs with averages of the neighborhood (avg_bauperiode). If the average size of the neighborhood is not available, replace it with the mean of that variable.
data$year_built <- ifelse(is.na(data$year_built), data$avg_bauperiode, data$year_built)
data <- data %>%
  mutate(
    year_built = ifelse(is.na(year_built) & is.na(avg_bauperiode), mean(avg_bauperiode, na.rm=T), year_built)
  )
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
#imputing NAs through mean.
hist(data$wgh_avg_sonnenklasse_per_egid)
data$wgh_avg_sonnenklasse_per_egid <- ifelse(is.na(data$wgh_avg_sonnenklasse_per_egid), mean(data$wgh_avg_sonnenklasse_per_egid,na.rm=T), data$wgh_avg_sonnenklasse_per_egid)

# Anteil_auslaend: checking the range. Include this variable, it has a significant impact. Imputing NAs using mean.
range(data$Anteil_auslaend, na.rm=T)
data$Anteil_auslaend <- ifelse(is.na(data$Anteil_auslaend), mean(data$Anteil_auslaend,na.rm=T), data$Anteil_auslaend)
lmauslaend<-lm(data$rent_full~data$Anteil_auslaend)
stargazer(lmauslaend, type="text")

# Avg_age: checking the range. Would not include it, no significant impact. Replacing NAs with mean.
range(data$Avg_age, na.rm=T)
summary(data$Avg_age)
data$Avg_age <- ifelse(is.na(data$Avg_age), mean(data$Avg_age,na.rm=T), data$Avg_age)
lmage<-lm(data$rent_full~data$Avg_age)
stargazer(lmage, type="text")

# Avg_size_household: checking the range. It refers to people per household.
range(data$Avg_size_household, na.rm=T)
summary(data$Avg_size_household)
hist(data$Avg_size_household)
#substituting NAs with median.
data$Avg_size_household <- ifelse(is.na(data$Avg_size_household), median(data$Avg_size_household,na.rm=T), data$Avg_size_household)

# Noise_max: checking the range. Would not include it, doesn't have a significant impact on rent.
range(data$Noise_max)
lmnoise<-lm(data$rent_full~data$Noise_max)
stargazer(lmnoise, type="text")

# anteil_efh: checking the range. It refers to percentage of detatched houses. I wouldn't include this variable.Imputing NAs using median.
range(data$anteil_efh, na.rm=T)
summary(data$anteil_efh)
hist(data$anteil_efh)
data$anteil_efh <- ifelse(is.na(data$anteil_efh), median(data$anteil_efh,na.rm=T), data$anteil_efh)

# apoth_pix_count_2km: checking the range.
range(data$apoth_pix_count_km2)
#significant impact on rent-->include this variable.
lmapo<-lm(data=data, rent_full~apoth_pix_count_km2)
stargazer(lmapo, type="text")

# avg_anzhl_geschosse: checking the range. I am not sure what this variable is.
range(data$avg_anzhl_geschosse, na.rm=T)
summary(data$avg_anzhl_geschosse)
hist(data$avg_anzhl_geschosse)
data$avg_anzhl_geschosse <- ifelse(is.na(data$avg_anzhl_geschosse), median(data$avg_anzhl_geschosse,na.rm=T), data$avg_anzhl_geschosse)

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

# dist_to_halst: not sure what this variable means. But it has a significant impact, we could include it. NAs substituted with median.
range(data$dist_to_haltst, na.rm=T)
summary(data$dist_to_haltst)
hist(data$dist_to_haltst)
data$dist_to_haltst<- ifelse(is.na(data$dist_to_haltst), median(data$dist_to_haltst,na.rm=T), data$dist_to_haltst)
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
data$dist_to_lake <- ifelse(is.na(data$dist_to_lake), mean(data$dist_to_lake, na.rm=T), data$dist_to_lake)
summary(data$dist_to_lake)
# dist_to_main_stat: not sure if NAs are too many.
range(data$dist_to_main_stat, na.rm = T)
summary(data$dist_to_main_stat)
hist(data$dist_to_main_stat)
data$dist_to_main_stat<-ifelse(is.na(data$dist_to_main_stat), mean(data$dist_to_main_stat, na.rm=T)-mean(data$dist_to_train_stat), data$dist_to_main_stat)
#significant impact on rent-->include this variable.
lmmain<-lm(data=data, rent_full~dist_to_main_stat)
stargazer(lmmain, type="text")

# dist_to_school_1: this should be ok. NAs substituted with median.
range(data$dist_to_school_1, na.rm=T)
summary(data$dist_to_school_1)
hist(data$dist_to_school_1)
data$dist_to_school_1<- ifelse(is.na(data$dist_to_school_1), median(data$dist_to_school_1,na.rm=T), data$dist_to_school_1)

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

#outlier detection: area.
boxplot(data$area)
summary(data$area)
#capping the unusually high values.
#Function that detects and outputs outliers:
find_outliers_IQR <- function(df) {
  q1 <- quantile(df, 0.25, na.rm = TRUE)
  q3 <- quantile(df, 0.75, na.rm = TRUE)
  IQR <- q3 - q1
  outliers <- df[(df < (q1 - 1.5 * IQR)) | (df > (q3 + 1.5 * IQR))]
  return(outliers)
}

#let's apply it to area
outliers_area <- find_outliers_IQR(data$area)
cat('number of outliers:', length(outliers_area), '\n')
cat('max outlier value:', max(outliers_area, na.rm = TRUE), '\n')
cat('min outlier value:', min(outliers_area, na.rm = TRUE), '\n')

#we cap unusually high values, setting as limit the mean of the variable + 3 standard deviations.
area_mean <- mean(data$area, na.rm = TRUE)
area_std <- sd(data$area, na.rm = TRUE)
area_cap <- area_mean + 3 * area_std
data$area <- ifelse(data$area > area_cap, area_cap, data$area)

#outliers in area_usable.
boxplot(data$area_useable)
summary(data$area_useable)
outliers_area_useable <- find_outliers_IQR(data$area_useable)
cat('number of outliers:', length(outliers_area_useable), '\n')
cat('max outlier value:', max(outliers_area_useable, na.rm = TRUE), '\n')
cat('min outlier value:', min(outliers_area_useable, na.rm = TRUE), '\n')
#setting a lower and upper limit this time.
upper_area_useable <- mean(data$area_useable, na.rm = TRUE) + 3 * sd(data$area_useable, na.rm = TRUE)
lower_area_useable <- mean(data$area_useable, na.rm = TRUE) - 3 * sd(data$area_useable, na.rm = TRUE)

data$area_useable <- ifelse(data$area_useable > upper_area_useable, upper_area_useable,
                                     ifelse(data$area_useable < lower_area_useable, lower_area_useable, data$area_useable))

#outliers in rent_full.
boxplot(data$rent_full)
summary(data$rent_full)
outliers_rent <- find_outliers_IQR(data$rent_full)
cat('number of outliers:', length(outliers_rent), '\n')
cat('max outlier value:', max(outliers_rent, na.rm = TRUE), '\n')
cat('min outlier value:', min(outliers_rent, na.rm = TRUE), '\n')
#setting a lower and upper limit this time.
upper_rent <- mean(data$rent_full, na.rm = TRUE) + 3 * sd(data$rent_full, na.rm = TRUE)
lower_rent <- mean(data$rent_full, na.rm = TRUE) - 3 * sd(data$rent_full, na.rm = TRUE)

data$rent_full <- ifelse(data$rent_full > upper_rent, upper_rent,
                            ifelse(data$rent_full < lower_rent, lower_rent, data$rent_full))

#outliers in rooms.
boxplot(data$rooms)
table((data$rooms))
outliers_rooms <- find_outliers_IQR(data$rooms)
cat('number of outliers:', length(outliers_rooms), '\n')
cat('max outlier value:', max(outliers_rooms, na.rm = TRUE), '\n')
cat('min outlier value:', min(outliers_rooms, na.rm = TRUE), '\n')
upper_rooms <- mean(data$rooms, na.rm = TRUE) + 3 * sd(data$rooms, na.rm = TRUE)
lower_rooms <- mean(data$rooms, na.rm = TRUE) - 3 * sd(data$rooms, na.rm = TRUE)

data$rooms <- ifelse(data$rooms > upper_rooms, upper_rooms,
                         ifelse(data$rooms < lower_rooms, lower_rooms, data$rooms))

#outliers in average household size.
summary(data$Avg_size_household)
boxplot(data$Avg_size_household)
#unusually high values. 13???
outliers_household <- find_outliers_IQR(data$Avg_size_household)
cat('number of outliers:', length(outliers_household), '\n')
cat('max outlier value:', max(outliers_household, na.rm = TRUE), '\n')
cat('min outlier value:', min(outliers_household, na.rm = TRUE), '\n')
#we cap unusually high values, setting as limit the mean of the variable + 3 standard deviations.
household_mean <- mean(data$Avg_size_household, na.rm = TRUE)
household_std <- sd(data$Avg_size_household, na.rm = TRUE)
household_cap <- household_mean + 3 * household_std
data$Avg_size_household <- ifelse(data$Avg_size_household > household_cap, household_cap, data$Avg_size_household)

#outliers in dist_to_4G
boxplot((data$dist_to_4G))
summary(data$dist_to_4G)
fourg_mean <- mean(data$dist_to_4G, na.rm = TRUE)
fourg_std <- sd(data$dist_to_4G, na.rm = TRUE)
fourg_cap <- fourg_mean + 3 * fourg_std
data$dist_to_4G <- ifelse(data$dist_to_4G > fourg_cap, fourg_cap, data$dist_to_4G)

#outliers in dist_to_5G
boxplot((data$dist_to_5G))
summary(data$dist_to_5G)
fiveg_mean <- mean(data$dist_to_5G, na.rm = TRUE)
fiveg_std <- sd(data$dist_to_5G, na.rm = TRUE)
fiveg_cap <- fiveg_mean + 3 * fiveg_std
data$dist_to_5G <- ifelse(data$dist_to_5G > fiveg_cap, fiveg_cap, data$dist_to_5G)

#outliers in dist_to_haltst
boxplot(data$dist_to_haltst)
summary(data$dist_to_haltst)
upper_halt <- mean(data$dist_to_haltst, na.rm = TRUE) + 3 * sd(data$dist_to_haltst, na.rm = TRUE)
lower_halt <- mean(data$dist_to_haltst, na.rm = TRUE) - 3 * sd(data$dist_to_haltst, na.rm = TRUE)

data$dist_to_haltst <- ifelse(data$dist_to_haltst > upper_halt, upper_halt,
                         ifelse(data$dist_to_haltst < lower_halt, lower_halt, data$dist_to_haltst))

#outliers in dist_to_highway
boxplot(data$dist_to_highway)
summary(data$dist_to_highway)
upper_highway <- mean(data$dist_to_highway, na.rm = TRUE) + 3 * sd(data$dist_to_highway, na.rm = TRUE)
lower_highway <- mean(data$dist_to_highway, na.rm = TRUE) - 3 * sd(data$dist_to_highway, na.rm = TRUE)

data$dist_to_highway<- ifelse(data$dist_to_highway > upper_highway, upper_highway,
                              ifelse(data$dist_to_highway < lower_highway, lower_highway, data$dist_to_highway))

#outliers in dist_to_lake
boxplot(data$dist_to_lake)
summary(data$dist_to_lake)
upper_lake<- mean(data$dist_to_lake, na.rm = TRUE) + 3 * sd(data$dist_to_lake, na.rm = TRUE)
lower_lake <- mean(data$dist_to_lake, na.rm = TRUE) - 3 * sd(data$dist_to_lake, na.rm = TRUE)

data$dist_to_lake<- ifelse(data$dist_to_lake > upper_lake, upper_lake,
                              ifelse(data$dist_to_lake < lower_lake, lower_lake, data$dist_to_lake))

#outliers in dist_to_main_stat
boxplot(data$dist_to_main_stat)
summary(data$dist_to_main_stat)
upper_main<- mean(data$dist_to_main_stat, na.rm = TRUE) + 3 * sd(data$dist_to_main_stat, na.rm = TRUE)
lower_main <- mean(data$dist_to_main_stat, na.rm = TRUE) - 3 * sd(data$dist_to_main_stat, na.rm = TRUE)

data$dist_to_main_stat<- ifelse(data$dist_to_main_stat > upper_main, upper_main,
                           ifelse(data$dist_to_main_stat < lower_main, lower_main, data$dist_to_main_stat))

#outliers in dist_to_school_1
boxplot(data$dist_to_school_1)
summary(data$dist_to_school_1)
upper_school<- mean(data$dist_to_school_1, na.rm = TRUE) + 3 * sd(data$dist_to_school_1, na.rm = TRUE)
lower_school <- mean(data$dist_to_school_1, na.rm = TRUE) - 3 * sd(data$dist_to_school_1, na.rm = TRUE)

data$dist_to_school_1<- ifelse(data$dist_to_school_1 > upper_school, upper_school,
                                ifelse(data$dist_to_school_1 < lower_school, lower_school, data$dist_to_school_1))

#outliers in dist_to_train_stat
boxplot(data$dist_to_train_stat)
summary(data$dist_to_train_stat)
upper_train<- mean(data$dist_to_train_stat, na.rm = TRUE) + 3 * sd(data$dist_to_train_stat, na.rm = TRUE)
lower_train <- mean(data$dist_to_train_stat, na.rm = TRUE) - 3 * sd(data$dist_to_train_stat, na.rm = TRUE)

data$dist_to_train_stat<- ifelse(data$dist_to_train_stat > upper_train, upper_train,
                                ifelse(data$dist_to_train_stat < lower_train, lower_train, data$dist_to_train_stat))

#outliers in dist_to_river
boxplot(data$dist_to_river)
summary(data$dist_to_river)
upper_river<- mean(data$dist_to_river, na.rm = TRUE) + 3 * sd(data$dist_to_river, na.rm = TRUE)
lower_river <- mean(data$dist_to_river, na.rm = TRUE) - 3 * sd(data$dist_to_river, na.rm = TRUE)

data$dist_to_river<- ifelse(data$dist_to_river > upper_river, upper_river,
                                ifelse(data$dist_to_river < lower_river, lower_river, data$dist_to_river))

#outliers in area_per_room
boxplot(data$area_per_room)
summary(data$area_per_room)
upper_apr<- mean(data$area_per_room, na.rm = TRUE) + 3 * sd(data$area_per_room, na.rm = TRUE)
lower_apr <- mean(data$area_per_room, na.rm = TRUE) - 3 * sd(data$area_per_room, na.rm = TRUE)

data$area_per_room<- ifelse(data$area_per_room > upper_apr, upper_apr,
                                ifelse(data$area_per_room < lower_apr, lower_apr, data$area_per_room))

# remove near zero variance variables
nzv <- nearZeroVar(data)
data <- data[, -nzv]

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
  select(-msregion)
data <- data %>%
  select(-GDENAMK)

#transforming KTKZ in dummy variables.
# Create dummy variables for KTKZ column
dummies <- model.matrix(~ KTKZ - 1, data = data)
dummies_df <- as.data.frame(dummies)

# Add the dummy variables to the data dataframe
data <- bind_cols(data, dummies_df)

# Remove the opensea_slug column
data <- select(data, -KTKZ)

# normalization of the predictors: this function is for z-score normalization.
#data <- preProcess(data, method = c("center", "scale")) # eg. Normalizing and scaling (means what?

#write_csv(data,"training_data_preprocessed.csv")
