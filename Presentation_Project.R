#Project/ Presentation file

#First step in R is to download data file and save it into a Dataframe
#commonly referred to as df

#My Question will be if these different factors have a relationship with people renting
#and that their rent is 50% or more of their income in the state of Illinois 
#compared to the national median

# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)
library(lmtest) # more advanced statistical testing
library(ggplot2) # for plotting
library(corrplot) # for correlation plots


# Step 1: Load data from the Excel file
# Specify the correct sheet containing the data
DATAFORPROJECT <- read_excel("/Users/ash/Desktop/DATAFORPROJECT.xlsx", sheet = "Data", col_types = "text")  


# View the dataset to confirm it loaded correctly
View(DATAFORPROJECT)

# Step 2: Print column names to confirm structure
# This helps us identify which variables are available in the dataset.
names(DATAFORPROJECT)

# Step 3: Filter out the variables we will not be looking at
# We are creating a new table focusing on variables relevant to our analysis.
dataframe <- select(DATAFORPROJECT, 
                    YEAR, 
                    COUNTYFIPS, 
                    STATEFIPS, 
                    STATE, 
                    COUNTY, 
                    REGION, 
                    ACS_TOT_CIVIL_EMPLOY_POP, 
                    ACS_TOT_CIVILIAN_LABOR, 
                    ACS_PCT_RENTER_HU_COST_50PCT, 
                    SAIPE_MEDIAN_HH_INCOME)

# View the filtered dataframe
View(dataframe)

# Step 4: Rename columns for clarity
colnames(dataframe)[7] <- "Employed"
colnames(dataframe)[8] <- "Civilian_Labor_Force"
colnames(dataframe)[9] <- "Renter_Housing_Cost_50pct"
colnames(dataframe)[10] <- "Median_Household_Income"

# 5. Create a bar plot for the 5 largest counties by Civilian Labor Force
# Visualization helps highlight key trends. This bar plot shows the top 5 counties 
# by Civilian Labor Force, giving us insight into where the labor force is concentrated.
top_5_counties <- dataframe %>%
  arrange(desc(Civilian_Labor_Force)) %>%
  head(5)

ggplot(top_5_counties, aes(x = reorder(COUNTY, Civilian_Labor_Force), y = Civilian_Labor_Force)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 5 Counties by Civilian Labor Force", x = "County", y = "Civilian Labor Force") +
  theme_minimal()

# 8. Summary statistics for key variables
summary(dataframe %>% 
          select(Employed, Civilian_Labor_Force, Renter_Housing_Cost_50pct, Median_Household_Income))

# Step 9: Narrow our focus down to one county : Your choice
# For demonstration, we focus on Adams County.
Adams_County <- filter(dataframe, COUNTY == "Adams County")

# View the filtered county data
View(Adams_County)

# Step 10: Convert variables to numeric
# Statistical tests require numeric data types. Here, we ensure our variables
# are properly formatted for analysis.
Adams_County <- Adams_County %>%
  mutate(
    Employed = as.numeric(Employed),
    Renter_Housing_Cost_50pct = as.numeric(Renter_Housing_Cost_50pct),
    Median_Household_Income = as.numeric(Median_Household_Income)
  )

# Check the structure of the data to confirm changes
str(Adams_County)

# Step 11: Build a regression model
# We aim to predict the number of employed individuals in Adams County based on:
# - Percentage of renters spending 50% or more on housing costs
# - Median household income
Reg <- lm(Employed ~ Renter_Housing_Cost_50pct + Median_Household_Income, data = Adams_County)

# Step 12: Plot regression line: Predicted vs Actual 2nd plot
# This scatter plot compares the predicted employment values with actual values.
# The red line represents a perfect prediction, helping us visualize model accuracy.
predicted <- predict(Reg, newdata = Adams_County)
actual <- Adams_County$Employed

plot(predicted, actual, 
     xlab = "Predicted", 
     ylab = "Actual", 
     main = "Predicted vs. Actual for Employed")
abline(a = 0, b = 1, col = "red")

# Step 13: Regression diagnostics plots
# These plots help evaluate model assumptions (e.g., normality of residuals, homoscedasticity).
par(mfrow = c(2, 2)) # Set up a 2x2 grid for plots
plot(Reg)

# Step 14: Summarize the regression model
# This summary provides important statistics about the regression model, 
# such as coefficients, R-squared value, and significance levels.
summary(Reg)

# Step 15: Further Refinement
Reg1 <- lm(log(Employed) ~ Renter_Housing_Cost_50pct + log(Median_Household_Income), data = Adams_County)
summary(Reg1)
par(mfrow = c(2, 2)) 
plot(Reg1)


# After running the statistical analysis a few things to focus on are R squared and P Values 
