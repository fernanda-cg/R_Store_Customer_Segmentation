#' Author: Fernanda Cortes
#' Date: Mar 27, 2023
#' Purpose: A1 - Direct Mail EDA
#' 

# Libraries
options(scipen = 999)
library(powerjoin)
library(dplyr)
library(scales)
library(stringr)
library(dplyr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(viridis)
library(radiant.data)
library(DataExplorer)
library(tidyverse)
library(sf)
library(mapview)
library(ggmap)
library(plotly)

# Using Joining Supplemental, written by professor Ted Kwartler, to get one csv

# List all the files paths of interest from your local machine
allFiles <- list.files(path = '~/Documents/Visualizing & Analyzing Data with R/hult_r_class/personalFiles/A1 - EDA/main_tables',
                       pattern = '*.csv',
                       full.names = T)

# Read in a list, list apply a function to an object ie read.csv to the file paths
allDF <- lapply(allFiles, read.csv)

# Using data.table rbindlist put all the data together
households <- power_left_join(allDF, by = "tmpID")

# Since the "external" data has additional households not part of the internal 
# BBY loyalty program, you can use complete.cases() to obtain the best records 
# for EDA analysis
bbyLoyalty <- households[complete.cases(households),]

# You can save a copy of the joined data with this.  
# It will save to your local working directory.
#write.csv(bbyLoyalty, 'bbyLoyalty.csv', row.names = F)

# Copy bbyLoyalty data frame to drop columns and fill nulls where needed
bbyLoyalty_clean <- data.frame(bbyLoyalty)

# Creating a first version of the list of columns to drop, chose based on 
# the description of the column, most were dropped for ethical reasons
dropCol <- list('BroadEthnicGroupings', 'ReligiousContributorInHome',
                'PoliticalContributerInHome', 'DonatestoHealthcare', 
                'DonatestoInternationalAidCauses', 'DonatestoVeteransCauses',
                'DonatestoHealthcare1', 'DonatestoInternationalAidCauses1',
                'TelephonesFullPhone', 'ReligiousMagazineInHome',
                'FinancialMagazineInHome', 'InterestinCurrentAffairsPoliticsInHousehold',
                'PartiesDescription', 'ReligionsDescription', 'LikelyUnionMember',
                'GunOwner', 'Veteran', 'supportsAffordableCareAct', 'supportsGayMarriage',
                'supportsGunControl', 'supportsTaxesRaise', 'overallsocialviews',
                'DonatestoConservativeCauses', 'DonatestoLiberalCauses', 
                'MosaicZ4')

# Drop selected columns
bbyLoyalty_clean <- bbyLoyalty_clean[, !names(bbyLoyalty_clean) %in% dropCol]

#### HomePurchasePrice, LandValue and EstHomeValue mean imputation by county
# Make it numeric in a new columns
bbyLoyalty_clean$housePrice <- as.numeric(gsub('[$]','',bbyLoyalty_clean$HomePurchasePrice))
bbyLoyalty_clean$LandVal <- as.numeric(gsub('[$]','',bbyLoyalty_clean$LandValue))
bbyLoyalty_clean$EstHomeVal <- as.numeric(gsub('[$]','',bbyLoyalty_clean$EstHomeValue))

# Calculate mean by county and impute in NA's. 
# Impute state mean if there is no information for county
# Impute total mean if there is no information for state
houseInfo <- c('housePrice', 'LandVal', 'EstHomeVal')

for (col_name in houseInfo) {
  bbyLoyalty_clean <- bbyLoyalty_clean %>%
    group_by(county) %>%
    mutate(!!sym(col_name) := ifelse(is.na(!!sym(col_name)), 
                                     mean(!!sym(col_name), 
                                          na.rm = TRUE), 
                                     !!sym(col_name)))
  bbyLoyalty_clean <- bbyLoyalty_clean %>%
    group_by(state) %>%
    mutate(!!sym(col_name) := ifelse(is.na(!!sym(col_name)), 
                                     mean(!!sym(col_name), 
                                          na.rm = TRUE), 
                                     !!sym(col_name)))
  bbyLoyalty_clean$housePrice[is.na(bbyLoyalty_clean$housePrice)] <- mean(bbyLoyalty_clean$housePrice, na.rm=TRUE)
}

# Drop old columns
bbyLoyalty_clean <- bbyLoyalty_clean[, !names(bbyLoyalty_clean) %in% list('HomePurchasePrice',
                                                                          'LandValue', 
                                                                          'EstHomeValue')]


# Find how many empty values each column has to decide what to do with each
bbyLoyalty_clean[bbyLoyalty_clean == ""] <- NA
countNulls <- data.frame(nulls = colSums(is.na(bbyLoyalty_clean)))
countNulls$percentage <- countNulls$nulls / 9678

# Now, we will focus on the columns that have a high % of missing values. Before dropping, 
# we'll check the unique values to determine if empty values can be filled
countNullsOver <- subset(countNulls, percentage > 0.5)

sapply(bbyLoyalty_clean[, row.names(countNullsOver)], unique)

# Drop empty column
bbyLoyalty_clean <- bbyLoyalty_clean[, !names(bbyLoyalty_clean) %in% 'BuyerofAntiquesinHousehold']

# After checking the unique values of the columns, we can determine that for some columns
# the empty values could mean "No". The "Yes" will be changed to 1 and then input 0 to empty values
colsToBool <- c('DonatestoAnimalWelfare', 'DonatestoArtsandCulture', 
                'DonatestoChildrensCauses', 'DonatestoWildlifePreservation',
                'DonatestoLocalCommunity', 'Investor', 'BusinessOwner', 
                'HorseOwner', 'CatOwner', 'DogOwner', 'OtherPetOwner',
                'HomeOffice', 'BuyerofArtinHousehold', 'GeneralCollectorinHousehold',
                'BooksAudioReadinginHousehold')

for (col_name in colsToBool) {
  bbyLoyalty_clean[[col_name]] <- ifelse(is.na(bbyLoyalty_clean[[col_name]]), 0, 
                                         ifelse(bbyLoyalty_clean[[col_name]] == "Yes", 1, 
                                                bbyLoyalty_clean[[col_name]]))
}

bbyLoyalty_clean$ComputerOwnerInHome <- ifelse(bbyLoyalty_clean$ComputerOwnerInHome == "Yes", 1, 0)

# Only keep the numeric part of the following columns and input 0 to NA's
numericSplit <- c("BookBuyerInHome", "UpscaleBuyerInHome", "FamilyMagazineInHome",
                  "FemaleOrientedMagazineInHome", "GardeningMagazineInHome", 
                  "CulinaryInterestMagazineInHome", "HealthFitnessMagazineInHome",
                  "DoItYourselfMagazineInHome")

for (col_name in numericSplit) {
  bbyLoyalty_clean[[col_name]] <- str_extract(bbyLoyalty_clean[[col_name]], "[[:digit:]]+")
  bbyLoyalty_clean[[col_name]] <- replace(bbyLoyalty_clean[[col_name]], is.na(bbyLoyalty_clean[[col_name]]), 0)
  bbyLoyalty_clean[[col_name]] <- as.numeric(bbyLoyalty_clean[[col_name]])
}


#### Calculate the ethnic mode by county, then city, then state to fill nulls
# Define Mode function to calculate the mode of a vector
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Group data by county and impute mode for ethnicity
bbyLoyalty_clean <- bbyLoyalty_clean %>% 
  group_by(county) %>%
  mutate(EthnicDescription = ifelse(is.na(EthnicDescription), Mode(EthnicDescription), EthnicDescription))

# Group data by city and impute mode for ethnicity
bbyLoyalty_clean <- bbyLoyalty_clean %>% 
  group_by(city) %>%
  mutate(EthnicDescription = ifelse(is.na(EthnicDescription), Mode(EthnicDescription), EthnicDescription))

# Group data by state and impute mode for remaining ethnicity nulls
bbyLoyalty_clean <- bbyLoyalty_clean %>% 
  group_by(state) %>%
  mutate(EthnicDescription = ifelse(is.na(EthnicDescription), Mode(EthnicDescription), EthnicDescription))

# Impute general mode for remaining ethnicity nulls
bbyLoyalty_clean$EthnicDescription[is.na(bbyLoyalty_clean$EthnicDescription)] <- Mode(bbyLoyalty_clean$EthnicDescription)


#bbyLoyalty_clean$FirstName[is.na(bbyLoyalty_clean$Gender)] = "Berta" -> missing gender is Female
bbyLoyalty_clean$Gender[is.na(bbyLoyalty_clean$Gender)] <- "F"


# Add a column for categorical value of NetWorth
bbyLoyalty_clean$NetWorthCat <- factor(bbyLoyalty_clean$NetWorth, 
                                       levels = c("$1-4999",
                                                  "$5000-9999",
                                                  "$10000-24999",
                                                  "$25000-49999",
                                                  "$50000-99999",
                                                  "$100000-249999",
                                                  "$250000-499999",
                                                  "$499999+"),
                                       labels = c(1,2,3,4,5,6,7,8))
bbyLoyalty_clean$NetWorthCat <- as.numeric(bbyLoyalty_clean$NetWorthCat)

# Run correlations to fill NetWorth NAs with most relatable column                                          
cor(bbyLoyalty_clean[, sapply(bbyLoyalty_clean, is.numeric)], 
    bbyLoyalty_clean$NetWorthCat, use = "complete.obs")


# Since EstHomeVal has the highest absolute correlation, we will use that to impute NAs
# using a simple linear regression
data_subset <- bbyLoyalty_clean[, c("NetWorthCat", "EstHomeVal")]
data_with_nulls <- data_subset[is.na(data_subset$NetWorthCat), ]
data_without_nulls <- data_subset[!is.na(data_subset$NetWorthCat), ]
model <- lm(NetWorthCat ~ EstHomeVal, data = data_without_nulls)
predicted_values <- predict(model, newdata = data_with_nulls)
bbyLoyalty_clean$NetWorthCat[is.na(bbyLoyalty_clean$NetWorthCat)] <- predicted_values
bbyLoyalty_clean$NetWorthCat[bbyLoyalty_clean$NetWorthCat > 8] <- 8
bbyLoyalty_clean$NetWorthCat <- as.integer(bbyLoyalty_clean$NetWorthCat)

# Now we will fill the NetWorth column using the categorical value
bbyLoyalty_clean$NetWorth <- ifelse(is.na(bbyLoyalty_clean$NetWorth) & bbyLoyalty_clean$NetWorthCat == 1, "$1-4999", 
                             ifelse(is.na(bbyLoyalty_clean$NetWorth) & bbyLoyalty_clean$NetWorthCat == 2, "$5000-9999",
                             ifelse(is.na(bbyLoyalty_clean$NetWorth) & bbyLoyalty_clean$NetWorthCat == 3, "$10000-24999", 
                             ifelse(is.na(bbyLoyalty_clean$NetWorth) & bbyLoyalty_clean$NetWorthCat == 4, "$25000-49999",
                             ifelse(is.na(bbyLoyalty_clean$NetWorth) & bbyLoyalty_clean$NetWorthCat == 5, "$50000-99999",
                             ifelse(is.na(bbyLoyalty_clean$NetWorth) & bbyLoyalty_clean$NetWorthCat == 6, "$100000-249999",
                             ifelse(is.na(bbyLoyalty_clean$NetWorth) & bbyLoyalty_clean$NetWorthCat == 7, "$250000-499999",
                             ifelse(is.na(bbyLoyalty_clean$NetWorth) & bbyLoyalty_clean$NetWorthCat == 8, "$499999+",
                                    bbyLoyalty_clean$NetWorth))))))))

# Imputing the remaining columns using the most frequent value
nullCols <- c('PresenceOfChildrenCode', 'HomeOwnerRenter', 'DwellingUnitSize')

for (col_name in nullCols) {
  bbyLoyalty_clean[[col_name]] <- ifelse(is.na(bbyLoyalty_clean[[col_name]]), Mode(bbyLoyalty_clean[[col_name]]), 
                                         bbyLoyalty_clean[[col_name]])
}

# Creating new column for PresenceOfChildrenCode as 1 or 0
bbyLoyalty_clean$children <- ifelse(bbyLoyalty_clean$PresenceOfChildrenCode == 'Modeled Likely to have a child' | 
                                      bbyLoyalty_clean$PresenceOfChildrenCode == 'Likely to have a child', 1, 0)

# New list of columns to drop
dropCol <- list('DonatesEnvironmentCauseInHome', 'DonatesToCharityInHome',
                'DonatestoAnimalWelfare', 'DonatestoArtsandCulture',
                'DonatestoChildrensCauses', 'DonatestoWildlifePreservation', 
                'FirstName', 'LastName', 'county', 'fips',
                'PresenceOfChildrenCode')

# Drop selected columns
bbyLoyalty_clean <- bbyLoyalty_clean[, !names(bbyLoyalty_clean) %in% dropCol]

# Apply log to houseInfo to normalize variables
for (col_name in houseInfo) {
  bbyLoyalty_clean[[paste0("log", col_name)]] <- log(bbyLoyalty_clean[[col_name]])
}

# To check which column is more useful for education info, we checked the mean 
# by education level
bbyLoyalty_clean %>%
  group_by(Education) %>%
  summarise(mean_MedianEducationYears = mean(MedianEducationYears, na.rm = TRUE))

# Since we are keeping the Education column, we'll simplify it
bbyLoyalty_clean$Education <- ifelse(bbyLoyalty_clean$Education == "Grad Degree - Likely" | 
                                       bbyLoyalty_clean$Education == "Grad Degree - Extremely Likely", "Undergrad", 
                                     ifelse(bbyLoyalty_clean$Education == "Bach Degree - Likely" | 
                                              bbyLoyalty_clean$Education == "Bach Degree - Extremely Likely", "Bachelors",
                                            ifelse(bbyLoyalty_clean$Education == "Some College - Likely" | 
                                              bbyLoyalty_clean$Education == "Some College -Extremely Likely" | 
                                              bbyLoyalty_clean$Education == "Vocational Technical Degree - Extremely Likely", "College/Tech",
                                                   ifelse(bbyLoyalty_clean$Education == "HS Diploma - Likely" | 
                                                            bbyLoyalty_clean$Education == "HS Diploma - Extremely Likely", "High School",
                                                          ifelse(bbyLoyalty_clean$Education == "Less than HS Diploma - Likely" | 
                                                                   bbyLoyalty_clean$Education == "Less than HS Diploma - Ex Like", "Basic",
                                                                 bbyLoyalty_clean$Education)))))

bbyLoyalty_clean$EducationCat <- factor(bbyLoyalty_clean$Education, 
                                        levels = c( "Unknown", "Basic", 
                                                    "High School", "College/Tech", 
                                                    "Bachelors","Undergrad"),
                                        labels = c(0,1,2,3,4,5))
bbyLoyalty_clean$EducationCat <- as.numeric(bbyLoyalty_clean$EducationCat)

# Create a new column to obtain spend per visit
bbyLoyalty_clean$spendPerVisit <- bbyLoyalty_clean$y_householdSpend / bbyLoyalty_clean$storeVisitFrequency

# Turn Gender to Boolean
bbyLoyalty_clean$isFemale <- ifelse(bbyLoyalty_clean$Gender == "F", 1, 0)
bbyLoyalty_clean <- bbyLoyalty_clean[, !names(bbyLoyalty_clean) %in% 'Gender']

# create a correlation matrix
cor_matrix <- cor(bbyLoyalty_clean[, sapply(bbyLoyalty_clean, is.numeric)])
cor_df <- as.data.frame(cor_matrix)

# After going through the correlation df, there's a high negative correlation
# between spend per visit and the number of magazines each househould has
bbyLoyalty_clean <- bbyLoyalty_clean %>% 
  mutate(totalMagazines = FamilyMagazineInHome + FemaleOrientedMagazineInHome +
           GardeningMagazineInHome + CulinaryInterestMagazineInHome +
           HealthFitnessMagazineInHome + DoItYourselfMagazineInHome)

### Creating bins for age analysis
# create custom breaks for age bins
custom_breaks <- c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)

# create age bins using custom breaks
bbyLoyalty_clean$age_bin <- cut(bbyLoyalty_clean$Age, breaks = custom_breaks)

# view the resulting age groups
table(bbyLoyalty_clean$age_bin)


#plot_histogram(bbyLoyalty_clean)

# Dumbbell plot to compare extremes by magazine count
# Define the list of magazine count columns to loop over
magazine_cols <- c('FamilyMagazineInHome', 'FemaleOrientedMagazineInHome',
                   'GardeningMagazineInHome', 'CulinaryInterestMagazineInHome',
                   'HealthFitnessMagazineInHome', 'DoItYourselfMagazineInHome', 
                   'totalMagazines')

# Loop over the column names
for (col_name in magazine_cols) {
  # Construct the formula dynamically based on the current column name
  formula <- as.formula(paste('spendPerVisit ~', col_name))
  
  # Aggregate the data by the current magazine count column
  minSpend <- aggregate(formula, data = bbyLoyalty_clean, FUN = min)
  maxSpend <- aggregate(formula, data = bbyLoyalty_clean, FUN = max)
  spend <- left_join(minSpend, maxSpend, by = col_name)
  names(spend) <- c('magazines','minSpend','maxSpend')
  
  # Create the dumbbell plot for the current magazine count column
  ggplot(spend) + 
    geom_segment(aes(x=minSpend, xend=maxSpend,
                     y=factor(magazines), yend=factor(magazines)),
                 size=2.5,  color = "#aeb6bf", alpha = 0.5) +
    geom_point(data = spend, aes(x=minSpend, y=factor(magazines)), color = '#d0e11c', size = 5) + 
    geom_point(data = spend, aes(x=maxSpend, y=factor(magazines)), color = '#440154', size = 5) +
    geom_vline(xintercept = 0, linetype = 'dotted', color = 'darkgrey', size = 0.5) +
    theme_few() + theme(legend.position="none") +
    labs(x="Spend per Visit", y = "# of Magazines", 
         title= paste("Min/Max Spend by", col_name))
  
  # Save the plot with a unique file name for each magazine count column
  ggsave(paste0('spendvs', col_name, '.jpg'))
}

##### Heatmap

# Create a subset with only the information necessary
spendAndLocation <- subset(bbyLoyalty_clean, select = c('state', 'lat', 'lon', 'spendPerVisit'))

#mapview(spendAndLocation, xcol = "lon", ycol = "lat", crs = 4269, grid = FALSE)

# Start map
# Boundaries for the USA
map_bounds <- c(left = -130, bottom = 24, right = -60, top = 50)
# Set map type
coords.map <- get_stamenmap(map_bounds, zoom = 4, maptype = 'toner-lite')
coords.map <- ggmap(coords.map, extent="device", legend="none")
# Add data frame to get info
coords.map <- coords.map + 
  stat_density2d(data=spendAndLocation,
                 aes(x=lon, y=lat, 
                     fill=..density..), 
                 geom="tile",
                 contour = F,
                 alpha = 0.5) +
  scale_fill_viridis(direction = -1)

# Source: https://axelhodler.medium.com/creating-a-heat-map-from-coordinates-using-r-780db4901075

coords.map
ggsave('heatmap.jpg')

# Double check the states with most loyalty members
stateCount <- as.data.frame(table(bbyLoyalty_clean$state))
stateCount[order(-stateCount$Freq), ]


# plot bar chart with average total spend per household and networth
bbyLoyalty_clean %>%
  group_by(NetWorth) %>%
  summarise(ave_spend = mean(y_householdSpend)) %>%
  ggplot(aes(x = factor(NetWorth, 
                        levels = c("$1-4999",
                                   "$5000-9999",
                                   "$10000-24999",
                                   "$25000-49999",
                                   "$50000-99999",
                                   "$100000-249999",
                                   "$250000-499999",
                                   "$499999+")), 
             y = ave_spend, 
             fill = factor(NetWorth, 
                           levels = c("$1-4999",
                                      "$5000-9999",
                                      "$10000-24999",
                                      "$25000-49999",
                                      "$50000-99999",
                                      "$100000-249999",
                                      "$250000-499999",
                                      "$499999+")))) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(direction = -1, discrete = T) + 
  labs(x = "Net Worth", y = "Average of Total Household Spend") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(fill = F)
ggsave('householdSpendVSNetWorth.jpg')

bbyLoyalty_clean %>%
  group_by(NetWorth, isFemale) %>%
  summarise(ave_spend = mean(y_householdSpend)) %>%
  ggplot(aes(x = factor(NetWorth, 
                        levels = c("$1-4999",
                                   "$5000-9999",
                                   "$10000-24999",
                                   "$25000-49999",
                                   "$50000-99999",
                                   "$100000-249999",
                                   "$250000-499999",
                                   "$499999+")), 
             y = ave_spend, 
             fill = factor(isFemale))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("#d0e11c", '#440154'), 
                    name = "Gender", 
                    labels = c('M', 'F')) +
  labs(x = "Net Worth", y = "Average of Total Household Spend") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  guides(fill = guide_legend(title = "Gender", ncol = 2, override.aes = list(alpha = 1)))
ggsave('householdSpendVSNetWorthGender.jpg')


# plot bar chart with average total spend per household and ethnicity
bbyLoyalty_clean %>%
  group_by(EthnicDescription) %>%
  summarise(ave_spend = mean(y_householdSpend)) %>%
  ggplot(aes(x = reorder(factor(EthnicDescription), -ave_spend), y = ave_spend, fill = ave_spend)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(name = "Average Spend") + 
  labs(x = "Ethnic Description", y = "Average of Total Household Spend") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave('householdSpendVSEthnicity.jpg')

# plot bar chart with average total spend per household and property type
bbyLoyalty_clean %>%
  group_by(PropertyType) %>%
  summarise(ave_spend = mean(y_householdSpend)) %>%
  ggplot(aes(x = reorder(factor(PropertyType), -ave_spend), y = ave_spend, fill = ave_spend)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(name = 'Average Spend') + 
  labs(x = "Property Type", y = "Average of Total Household Spend") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave('householdSpendVSPropertyType.jpg')

ethnicityCount <- as.data.frame(table(bbyLoyalty_clean$EthnicDescription))
ethnicityCount[order(-ethnicityCount$Freq), ]

#bbyLoyalty_clean %>%
#  group_by(EthnicDescription) %>%
#  summarise(ave_spend = mean(y_householdSpend), ethnicityCount = n()) %>%
#  filter(ethnicityCount > 95) %>%
#  ggplot(aes(x = reorder(factor(EthnicDescription), -ave_spend), y = ave_spend, fill = ave_spend)) +
#  geom_bar(stat = "identity") +
#  scale_fill_viridis(option = "A") + 
#  labs(x = "Ethnic Description", y = "Average of Total Household Spend") +
#  theme_classic() +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# scatterplot for logEstHomeVal and householdSpend to see if there is any
# relation in spending habits using home value, since we don't have a continuous
# variable for income
ggplot(bbyLoyalty_clean, aes(x = logEstHomeVal, y = y_householdSpend)) +
  stat_binhex(aes(fill = ..count..), alpha = 0.7) +
  scale_fill_viridis(name = "Count") +
  labs(x = "Log of Estimated Home Value", y = "Total Spend") +
  theme_classic()
ggsave('totalSpendVSEstHomeVal.jpg')

ggplot(bbyLoyalty_clean, aes(x = logEstHomeVal, 
                             y = spendPerVisit, 
                             color = HomeOwnerRenter, 
                             size = storeVisitFrequency)) + 
  geom_point(alpha = 0.5) +
  labs(x = "Log of Estimated Home Value", 
       y = "Spend Per Visit", 
       color = "Home Ownership", 
       size = "Store Visit Frequency") +
  facet_wrap(~ NetWorth) +
  scale_color_manual(values = c("#440154", "#d0e11c")) +
  theme_classic()

# Getting a table with some descriptive data for the states with the most 
# loyalty members
bbyLoyalty_clean %>%
  filter(state %in% c("Texas", "California", "Pennsylvania", 
                      "Illinois", "New York", "Ohio", "New Mexico", 
                      "Missouri", "Oregon", "Kentucky", "Michigan", 
                      "Colorado", "Wisconsin", "Washington", "Indiana", 
                      "Minnesota")) %>%
  group_by(state) %>%
  summarize(mean_spend = mean(y_householdSpend),
            median_spend = median(y_householdSpend),
            sd_spend = sd(y_householdSpend),
            count = n())


selected_states <- c("Texas", "California", "Pennsylvania", 
                     "Illinois", "New York", "Ohio", "New Mexico", 
                     "Missouri", "Oregon", "Kentucky", "Michigan", 
                     "Colorado", "Wisconsin", "Washington", "Indiana", 
                     "Minnesota")

selected_data <- bbyLoyalty_clean[bbyLoyalty_clean$state %in% selected_states, ]

#  Graphing spend per visit boxplots for the states with the most 
# loyalty members
ggplot(selected_data, aes(x = state, y = spendPerVisit)) + 
  geom_boxplot(fill = "#d0e11c", color = "#440154", alpha = 0.5, outlier.size = 2) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(x = "State", y = "Spend per Visit") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text = element_text(size=8)) +
  theme_classic()

# Filter out NA values for next graph
spend_by_age <- bbyLoyalty_clean %>% 
  filter(!is.na(age_bin)) %>% 
  group_by(age_bin) %>% 
  summarise(mean_spend_per_visit = mean(spendPerVisit))

# Create spend per visit bar graph segmented by age bin
ggplot(spend_by_age, aes(x = age_bin, y = mean_spend_per_visit, fill = age_bin)) + 
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(name = 'Age Bins') +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(x = "Age Bin", y = "Mean Spend per Visit") +
  theme_classic()
ggsave('spendpervisit_by_age2.jpg')

# Filter out NA values for next graph
spend_by_age <- bbyLoyalty_clean %>% 
  filter(!is.na(age_bin)) %>% 
  group_by(age_bin) %>% 
  summarise(mean_total_spend = mean(y_householdSpend))

# Create total spend bar graph segmented by age bin
ggplot(spend_by_age, aes(x = age_bin, y = mean_total_spend, fill = age_bin)) + 
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(name = 'Age Bins') +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(x = "Age Bin", y = "Mean Spend per Visit") +
  theme_classic()
ggsave('totalSpend_by_age.jpg')

# Create total spend bar graph segmented by age bin and gender
ggplot(drop_na(bbyLoyalty_clean, age_bin), aes(x = age_bin, y = y_householdSpend , fill = factor(isFemale))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  scale_fill_manual(values = c("#d0e11c", '#440154'), 
                    name = 'Gender', labels= c('M', 'F')) +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(x = "Age Bin", y = "Mean Total Spend", fill = "Gender") +
  theme_classic()
ggsave('totalSpend_by_age_and_gender.jpg')

# Boxplot for Total Household Spend by Education level
ggplot(data = subset(bbyLoyalty_clean, Education != "Unknown"), 
       aes(x = Education, y = y_householdSpend)) +
  geom_boxplot(fill = '#440154', color = "#21918c") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(x = "Education", y = "Total Household Spend") +
  theme_classic() +
  scale_fill_viridis_d() +
  scale_color_viridis_d()
ggsave('householdSpendbyEducation.jpg')

# Boxplot for Total Household Spend for Basic Education level
ggplot(data = subset(bbyLoyalty_clean, Education == "Basic") %>% 
         drop_na(age_bin), 
       aes(x = age_bin, y = y_householdSpend, fill = Education)) +
  geom_boxplot(fill = '#440154', color = "#21918c") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(x = "Age Bins", y = "Total Household Spend") +
  theme_classic() +
  scale_fill_viridis_d() +
  scale_color_viridis_d()
ggsave('householdSpendbyEducation_Basic.jpg')

# Boxplot for Total Household Spend for Basic Education level 
# filtering only the age bins that seem important
ggplot(data = subset(bbyLoyalty_clean, Education == "Basic" & 
                       age_bin %in% c("(30,35]", "(35,40]", "(40,45]", "(45,50]", "(50,55]", "(55,60]")), 
       aes(x = age_bin, y = y_householdSpend, fill = factor(isFemale))) +
  geom_boxplot(color = "#21918c") +
  scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
  labs(x = "Age Bins", y = "Total Household Spend", fill = "Gender") +
  theme_classic() +
  scale_fill_manual(values = c("#d0e11c", "#440154"), labels = c("Male", "Female")) +
  scale_color_manual(values = c("#d0e11c", "#440154"), labels = c("Male", "Female"))
ggsave('householdSpendbyEducation_Basic_targetAgeBins.jpg')

# histogram for logEstHomeVal
ggplot(bbyLoyalty_clean, aes(x = logEstHomeVal)) +
  geom_histogram(binwidth = 0.1, fill = "#21918c", color = "#440154") +
  geom_vline(xintercept = mean(bbyLoyalty_clean$logEstHomeVal), 
             linetype = "dashed", color = "#d0e11c", size = 1) +
  scale_x_continuous(breaks = seq(10, 16, by = 1)) +
  labs(x = "Log Estimated Home Value", y = "Count") +
  theme_classic()
ggsave('logEstHomeValHistogram.jpg')

ggplot(bbyLoyalty_clean, aes(x = logEstHomeVal, fill = factor(isFemale))) +
  geom_histogram(binwidth = 0.1, color = "#21918c", position = "identity",
                 alpha = 0.5) +
  geom_vline(xintercept = mean(bbyLoyalty_clean$logEstHomeVal), 
             linetype = "dashed", color = "#d0e11c", size = 1) +
  scale_x_continuous(breaks = seq(10, 16, by = 1)) +
  labs(x = "Log Estimated Home Value", y = "Count", fill = "Gender") +
  scale_fill_manual(values = c("#d0e11c", "#440154"), labels = c("M", "F")) +
  theme_classic()
ggsave('logEstHomeValHistogram_byGender.jpg')

# End
