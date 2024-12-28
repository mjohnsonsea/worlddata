#Programming 1 - Assignment 2
#Your name(s): Group 9 - Mike Johnson | Ryan Mathis


# Preparation: Load the required packages.
library(tidyverse)

#Part 1:

#Q1:
# Load the World values survey data "WorldValues3.csv" and save it as WorldData. 
# Load the country code data "CountryCodes3.csv" and save it as CountryData.
WorldData = read.csv("WorldValues3.csv")
CountryData = read.csv("CountryCodes3.csv")


#Q2:
# Join the country name from the CountryCode data into WorldData. 
# Note that country code is called *ID* in CountryData and *CountryCode* in WorldData. 
# Return the first 10 unique countries. 

# Join the country name from the CountryCode data into WorldData.
WorldData = left_join(WorldData,
                      CountryData,
                      by = c('CountryCode' = 'ID'))

# Return the first 10 unique countries. 
UniqueCountries = WorldData %>% 
  distinct(Country) %>% 
  head(10)

UniqueCountries


#Q3:
# Update WorldData by dropping income, education, and lifeexpect variables.
# What is the dimension of the updated WorldData?

# Update WorldData by dropping income, education, and lifeexpect variables.
WorldData = WorldData %>% 
  select(
    -c(
      Income,
      Education,
      lifeexpect
    )
  )

# What is the dimension of the updated WorldData?
dimensions = WorldData %>% 
  dim_desc()

dimensions


#Q4:
# Using pipes, calculate the average age and percent immigrant (i.e., average ratio of immigrants) for every country.
# Display the top 5 countries in terms of average age. 
# Hint: For displaying the top 5, refer to the help for slice( ) to find the right form of the function to use.
# Calculate percent immigrant for every country in dataset.

top_5_countries = WorldData %>% 
  mutate(Age = SurveyYear - BirthYear) %>% 
  group_by(Country) %>% 
  summarise(
    `Average Age` = mean(Age, na.rm = TRUE),
    `Percent Immigrant` = sum(Immigrant, na.rm = TRUE) / n()
  ) %>% 
  slice_max(order_by = `Average Age`, n=5)

top_5_countries


#Q5:
# Group WorldData by country and marital status and show the percent of people in each category who are immigrants.
# Show the results for China. (the coding for the *Marital* variable is 1 = Married, 2 = Living together, 3 = Divorced, 
# 4 = Separated, 5 = Widowed, and 6 = Single).

# Create marital status data frame.
d_marital_status = data.frame(Marital = c(1, 2, 3, 4, 5, 6),
                              Status = c('Married', 'Living Together', 'Divorced', 'Separated', 'Widowed', 'Single'))

# Summarize Marital Status in China
china_marital_status = WorldData %>% 
  group_by(Country, Marital) %>% 
  summarize(`Percent Immigrant` = sum(Immigrant, na.rm = TRUE) / n()) %>% 
  filter(Country == 'China') %>% 
  left_join (d_marital_status, by = 'Marital') %>% 
  select(
    Marital,
    Status,
    `Percent Immigrant`
  )

china_marital_status


#Q6: 
# Using pipes, create a function that takes a country name and calculates its average life satisfaction ("Satisfied") 
# broken down by "immigrant". Name this function meanSat. Show the results for China.

meanSat = function(CountryName) {
  WorldData %>% 
    filter(Country == CountryName) %>% 
    summarise(`Avg Life Satisfaction` = mean(Satisfied, na.rm = TRUE))
}

meanSat("China")

#Part 2: 
# Load the web analytics data "WebAnalytics3.csv" and save it as analytics.
analytics = read.csv("WebAnalytics3.csv")


#Q7:
# Use base R to draw the scatterplot of TimeOnPage (x axis) versus PageViews (y axis).
# Make sure to change the shape, color, and size of the points. 
# Also, set your own labels for both axes and add a title to the plot. 
# In addition, set a limit on the x axis to be between 0 and 300,000.

base_plot = plot(analytics$TimeOnPage, analytics$PageViews,
                 pch = 1, # Circle
                 col = "red", # Color selection
                 cex = 1.5, # Set size
                 xlab = "Time on Page", # x axis label
                 ylab = "Page Views", # y axis label
                 main = "Scatterplot of Time on Page vs Page Views", # Title
                 xlim = c(0, 300000) # Set limit on x axis to be between 0 and 300,000
                 )

base_plot


#Q8:
# Recreate the same plot using ggplot. 
# (You can refer to ggplot cheatsheet or its reference page online for an overview of all the arguments.)
# (For example, here are some examples of geom_point() options: 
# https://ggplot2.tidyverse.org/reference/geom_point.html

ggplot_scatter = ggplot(analytics, aes(TimeOnPage, PageViews)) + 
  geom_point(pch = 1, # Circle
             col = "red", # Color selection
             size = 1.5, # Set size
             ) + 
  
  # Set Labels
  labs(x = "Time on Page", # x axis label
       y = "Page Views", # y axis label
       title = "Scatterplot of Time on Page vs Page Views") + 
       xlim(0, 300000) # Set limit on x axis to be between 0 and 300,000

ggplot_scatter


#Q9:
# a. Use ggplot to draw the scatterplot of TotalDownloads (x axis) versus ConversionRate (y axis).
# b. Draw a similar plot but add jitters. Use a theme and a color.
# (For both parts, read TotalDownloads as a factor for visual clarity on the x axis.)

# a. Use ggplot to draw the scatterplot of TotalDownloads (x axis) versus ConversionRate (y axis).
plot_cr = ggplot(analytics, aes(factor(TotalDownloads), ConversionRate)) +
  geom_point(pch = 1, # Circle
             col = "red", # Color selection
             size = 1.5, # Set size
             ) +
  
  # Set labels
  labs(x = "Total Downloads",
       y = "Conversion Rate",
       title = "Scatterplot of Total Downloads vs Conversion Rate") +
  
  # Set theme
  theme_classic()

plot_cr

# b. Draw a similar plot but add jitters. Use a theme and a color.
jitters_cr = ggplot(analytics, aes(factor(TotalDownloads), ConversionRate)) +
  geom_jitter(pch = 1, # Circle
             col = "red", # Color selection
             size = 1.5, # Set size
  ) +
  
  # Set labels
  labs(x = "Total Downloads", 
       y = "Conversion Rate",
       title = "Jitters of Total Downloads vs Conversion Rate") +
  
  # Set theme
  theme_classic()

jitters_cr

#Q10:
# Create a boxplot with Region as categories (x-axis) and ConversionRate as the outcome of interest (y-axis).
# Change the (border/points) color of the boxplots.
# In addition, add a title, a subtitle, and a blank x axis label. 
# In so doing, adjust the location, size, and the color of the title and the subtitle.

boxplot_cr = ggplot(analytics, aes(Region, ConversionRate)) +
  geom_boxplot(color = "green") + 
  
  # Set labels
  labs(x = "",
      y = "Conversion Rate",
      title = "Boxplot of Conversion Rates",
      subtitle = "By Region") +
  
  # Adjust title and subtitle formats and position
  theme(plot.title = element_text(hjust = 0.5, size = 16, color = "blue"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "blue"))

boxplot_cr

#Q11:
# Create the same box plot of Q10 but use facet wrap to separate by Source.
# Also, fill each boxplot with different colors based on Source and 
# add a color legend to the bottom of the plot.

facetwrap_cr = ggplot(analytics, aes(Region, ConversionRate, fill = Source)) +
  geom_boxplot() + 
  
  # Set Labels
  labs(x = "",
       y = "Conversion Rate",
       title = "Boxplot of Conversion Rates",
       subtitle = "By Region") +
  
  # Adjust title, subtitle, and legend formats and potion
  theme(plot.title = element_text(hjust = 0.5, size = 16, color = "blue"),
        plot.subtitle = element_text(hjust = 0.5, size = 12, color = "blue"),
        legend.position = "bottom") + 
  
  # Facet wrap by source
  facet_wrap(~Source) 

facetwrap_cr


