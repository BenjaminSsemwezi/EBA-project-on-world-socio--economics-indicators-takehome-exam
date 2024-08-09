#loading libraries
library(readxl)
library(dplyr)
#loading the dataset
World_bank <- read_excel("C:/Users/Ssemwezi/Desktop/WorldBank.xlsx")
View(World_bank)

#inspecting the data
head(World_bank)

#summary of statistics
summary(World_bank)

#checking for missing values
colSums(is.na(World_bank))

#Let's remove rows with any missing values
worldbank_cleaned <- na.omit(World_bank)

#Check for and removing duplicates
worldbank_cleaned <- data_cleaned %>% distinct()


# After snapshot
head(worldbank_cleaned)
View(worldbank_cleaned)


# Load necessary libraries
library(readxl)
library(writexl)
install.packages("writexl")
library(writexl)

#saving cleaned dataset
write_xlsx(worldbank_cleaned,"C:/Users/Ssemwezi/Desktop/worldbank_cleaned.xlsx")

# Creating a smaller dataset
worldbank_smaller <- data.frame(
  `Country Name` = c('Afghanistan', 'Afghanistan', 'Afghanistan', 'Afghanistan', 'Afghanistan'),
  Year = c(2018, 2017, 2016, 2015, 2014),
  `Education expenditure (% of GDP)` = c(3.2, 3.1, 3.0, 2.9, 2.8),
  `Healthcare expenditure (% of GDP)` = c(10.0, 9.8, 9.6, 9.5, 9.3)
)
View(worldbank_smaller)

# Save the smaller dataset to an Excel file
write_xlsx(worldbank_smaller, "C:/Users/Ssemwezi/Desktop/WorldBank_Smaller.xlsx")

# Load the original dataset
df_original <- read_excel("C:/Users/Ssemwezi/Desktop/worldbank_cleaned.xlsx")

# Load the smaller dataset
df_smaller <- read_excel("C:/Users/Ssemwezi/Desktop/WorldBank_Smaller.xlsx")

# Merge the datasets on 'Country Name' and 'Year'
df_merged <- merge(df_original, df_smaller, by = c("Country Name", "Year"), all.x = TRUE)

# Display the first few rows of the merged dataset
head(worldbank_cleaned)

#Summary of statistics and data visualisation
# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(cluster)

# Summary statistics
summary(worldbank_cleaned)
View(worldbank_cleaned)
df <- read_excel("C:/Users/Ssemwezi/Desktop/worldbank_cleaned.xlsx")

# Visualisations
library(ggplot2)

#Number of countries by income group and region
ggplot(worldbank_cleaned, aes(x = Region, fill = IncomeGroup)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of Countries by Income Group and Region") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)
library(dplyr)

#number of countries per income group per region for the years 1995, 2005, and 2014
# Filter the dataset for the required years
filtered_data <- worldbank_cleaned %>% filter(Year %in% c(1995,2005,2014))

# Create the bar plot
ggplot(filtered_data, aes(x = Region, fill = IncomeGroup)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Year) +
  labs(title = "Number of Countries by Income Group and Region for Selected Years",
       x = "Region",
       y = "Number of Countries") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Birthrate by region and income group
library(ggplot2)
library(dplyr)

ggplot(worldbank_cleaned, aes(x = Region, y = `Birth rate, crude (per 1,000 people)`, fill = IncomeGroup)) +
  geom_boxplot() +
  labs(title = "Birthrate by Region and Income Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#INCOME MORTALITY RATE

install.packages("reshape2")
library(reshape2)
library(ggcorrplot)
library(tidyr)
install.packages("ggcorrplot")
library(ggcorrplot)

#birthrates and death rates per region
ggplot(worldbank_cleaned, aes(x = `Birth rate, crude (per 1,000 people)`, y = `Death rate, crude (per 1,000 people)`, color = Region)) +
  geom_point() +
  labs(title = "Birth Rates and Death Rates per Region")

#GDP per region and income group
ggplot(worldbank_cleaned, aes(x = Region, y = `GDP (USD)`, fill = IncomeGroup)) +
  geom_boxplot() +
  scale_y_log10() +  # Log scale for better visualization
  labs(title = "GDP per Region and Income Group") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Infant mortality rate and life expectancy per region
ggplot(worldbank_cleaned, aes(x = `Infant mortality rate (per 1,000 live births)`, y = `Life expectancy at birth (years)`, color = Region, shape = IncomeGroup)) +
  geom_point() +
  labs(title = "Infant Mortality Rate and Life Expectancy per Region and Income Group")

#unemployment rate per region after every 5 years
# Filter the dataset for the years of interest
filtered_data <- worldbank_cleaned %>%
  filter(Year %in% c(1995, 2000, 2005, 2010, 2014)) %>%
  select(Year, Region,`Unemployment (% of total labor force) (modeled ILO estimate)`)

# Rename the unemployment column for easier access
colnames(filtered_data)[which(names(filtered_data) == "Unemployment (% of total labor force) (modeled ILO estimate)")] <- "Unemployment_Rate"

# Plot the grouped bar chart
ggplot(filtered_data, aes(x = Year, y = Unemployment_Rate, fill = Region)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Unemployment Rate by Region (1995, 2000, 2005, 2010, 2014)",
       x = "Year",
       y = "Unemployment Rate (%)",
       fill = "Region") +
  theme_minimal()


install.packages("scales")
library(scales)
library(ggplot2)

#population density per region
ggplot(worldbank_cleaned, aes(x = Region, y = `Population density (people per sq. km of land area)`)) +
  geom_boxplot() +
  labs(title = "Population Density per Region") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Correlation Heatmap
install.packages("corrplot")
library(reshape2)
library(ggcorrplot)
library(corrplot)

numeric_data <- worldbank_cleaned[, sapply(worldbank_cleaned, is.numeric)]
cor_matrix <- cor(numeric_data, use = "complete.obs")

ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, 
           method="circle", colors = c("red", "white", "blue"), 
           title="Correlation Heatmap of Socioeconomic Indicators", 
           ggtheme=theme_bw)








