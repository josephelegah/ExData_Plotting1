# Load necessary libraries
library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)

# 1. Download and Load the Data
# Assuming the data is in a file named "household_power_consumption.txt"
# and is in the same directory as your R script.
# **You might need to adjust the file path.**
data <- fread("household_power_consumption.txt", na.strings="?")

# 2. Data Cleaning and Transformation

# Convert Date and Time variables
data$Time <- strptime(data$Time, format="%H:%M:%S")
data$Date <- as.Date(data$Date, format="%d/%m/%Y")
data <- mutate(data, DateTime = as.POSIXct(paste(Date, format(Time,"%H:%M:%S")), format="%Y-%m-%d %H:%M:%S"))


# Subset data to the specified date range (if required for the project)
data_filtered <- subset(data, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))

# 3. Exploratory Data Analysis (EDA)

# Basic Summary Statistics
summary(data_filtered)

# Check for missing values
sapply(data_filtered, function(x) sum(is.na(x)))

# Histograms of numerical variables
hist(data_filtered$Global_active_power, main="Global Active Power", xlab="Global Active Power (kilowatts)")
hist(data_filtered$Voltage, main="Voltage", xlab="Voltage (volt)")
hist(data_filtered$Global_reactive_power, main="Global Reactive Power", xlab="Global Reactive Power (kilowatts)")

# Time series plots
# Global active power over time
ggplot(data_filtered, aes(x=DateTime, y=Global_active_power)) +
  geom_line() +
  xlab("Date and Time") +
  ylab("Global Active Power (kilowatts)") +
  ggtitle("Global Active Power Over Time (2007-02-01 to 2007-02-02)")

# 4.  Further Analysis (as per project requirements)
#  This is where you would add code specific to the questions
#  being asked in the assignment.  For example, you might
#  need to calculate averages, create other types of plots,
#  or perform statistical tests.

# Example: Calculate daily average active power
daily_avg <- data_filtered %>%
  group_by(Date) %>%
  summarise(Mean_Global_active_power = mean(Global_active_power, na.rm=TRUE))

print(daily_avg)
