#Section 1: Load Required Libraries & Install Packages.
#Install all necessary packages at once to have prepare for all steps.
packages <- c("tidyverse", "readxl", "janitor", "effsize")
install.packages(packages[!packages %in% installed.packages()])

#call and Load all required libraries for this project.
library(tidyverse)  
library(readxl)
library(janitor)
library(effsize)

#Section 2: Data Import & Initial Exploration
#Load File from CBS website data as this site provides reliable and coherent statistical information.
#Skip metadata rows that exist in our data to have clean and vital data for better analyse.
df <- read_excel("C:/Users/aliho/Downloads/big data for business/Directory for my files in R/Data/Final Assignment(Bank)/pc5_2021_vol.xlsx",
                 skip = 5) 

#First, explore the data.
#Shows structure and first few entries of each column to understand about data.
glimpse(df) 
#First few rows of dataset.
head(df)     

#Section 3: Data Cleaning & Preprocessing
#Clean column names and remove empty columns to prepare for analyse.
df <- df %>%
  clean_names() %>%
  select_if(~!all(is.na(.)))  

#Rename Dutch columns to English for better readability.
#Remove remaining x* columns.
df <- df %>%
  rename(
    postal_code = x1,             
    population = inwoners,        
    households = huishouden,      
    housing = woning,              
    social_security = sociale_zekerheid, 
    income = inkomen_gedurende_2021,
    density = dichtheid,             
    facilities_37 = voorzieningen_37, 
    facilities_49 = voorzieningen_49,
    facilities_65 = voorzieningen_65,
    facilities_73 = voorzieningen_73,
    facilities_77 = voorzieningen_77,
    facilities_99 = voorzieningen_99,
    facilities_115 = voorzieningen_115
  ) %>%
  select(-starts_with("x"))    

#Section 4: Data Transformation & Handling Missing Values.
#Remove rows where postal_code is NA or contains "Totaal".
df <- df %>%
  filter(!is.na(postal_code) & !grepl("Totaal", postal_code))

#Convert all relevant columns to numeric and handle missing/negative values as calculations such as mean, correlation, regression analysis, and visualizations require proper numeric data without missing or invalid values for accurate results.
df <- df %>%
  mutate(across(c(population, households, housing, income, 
                  social_security, starts_with("facilities_"), 
                  density),
                ~{
                  x <- as.numeric(.)
                  x[x < 0] <- NA
                  replace_na(x, mean(x, na.rm = TRUE))
                }))

#Check the results of the data cleaning process.
summary(df)

#Section 5: Data Visualization
#Histogram for Population Distribution.
ggplot(df, aes(x = population)) + 
  geom_histogram(bins = 30, fill = "skyblue", color = "black") + 
  labs(title = "Population Distribution", x = "Population", y = "Count") +
  theme_minimal()

#Boxplot for Income Distribution.
ggplot(df, aes(y = income)) + 
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  labs(title = "Boxplot of Income") +
  theme_minimal()

#Scatter Plot: Population vs Social Security Benefits.
ggplot(df, aes(x = population, y = social_security)) + 
  geom_point(color = "red", alpha = 0.5) + 
  geom_smooth(method = "lm", color = "blue", se = FALSE) +  
  labs(title = "Population vs Social Security Benefits", 
       x = "Population", 
       y = "Social Security Benefits") +
  theme_minimal()

#Section 6: Statistical Analysis
#correlation analysis; We need to measure the strength and direction of the linear relationship.  
cor_value <- cor(df$population, df$social_security)
print(paste("Correlation between Population and Social Security:", round(cor_value, 3)))

#Linear Regression Analysis; Linear Regression Analysis helps us predict social security benefit levels based on population size. It also shows us exactly how these variables are related mathematically. Finally, it tells us how much social security benefits change for each unit increase in population through the regression coefficient.
model <- lm(social_security ~ population, data = df)

#Model diagnostics; We need to check if our linear regression assumptions are met. This includes making sure that our data follows a normal distribution. 
#Q-Q Plot for residuals (Normality Check); We can use Q-Q plots since helps us make sure that our statistical inferences and predictions are valid and reliable.
qqnorm(model$residuals)
qqline(model$residuals, col = "red")

#Model summary and confidence intervals
summary(model)
confint(model, level = 0.95)

#Effect Size Analysis using Cohen's_d
#Compare social security benefits between high and low population areas
high_pop <- df$social_security[df$population > median(df$population)]
low_pop <- df$social_security[df$population <= median(df$population)]
cohen_d <- cohen.d(high_pop, low_pop)
#see the result of cohen_d
print(cohen_d)
