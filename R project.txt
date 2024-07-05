# Install necessary libraries (if not already installed)
install.packages("ggplot2")
install.packages("lubridate")
install.packages("plyr")
install.packages("corrplot")
install.packages("gridExtra")
install.packages("ggthemes")
install.packages("caret")
install.packages("MASS")
install.packages("randomForest")
install.packages("party")
install.packages("readr")
install.packages("vroom")
# Load the libraries
library(ggplot2)
library(lubridate)
library(plyr)
library(corrplot)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
library(party)


# Load 
library(readr) 
comcast_fcc_complaints_2015 <- read_csv("C:/Users/28015509/Downloads/comcast_fcc_complaints_2015.csv")
#Or load
comcast_fcc_complaints_2015 <- read.csv("C:/Users/28015509/Downloads/comcast_fcc_complaints_2015.csv", stringsAsFactors = FALSE)

# Now convert the 'Date' column to a Date type
comcast_fcc_complaints_2015$Date <- mdy(comcast_fcc_complaints_2015$Date)
# Aggregate data to get the number of complaints per day
daily_complaints <- aggregate(`Ticket #` ~ Date, comcast_fcc_complaints_2015, length)
# Plot the daily complaint count over time
ggplot(daily_complaints, aes(x=Date, y=`Ticket #`)) +
  geom_line() +
  labs(title="Trend of Daily Complaints", x="Date", y="Number of Complaints") +
  theme_minimal()

#Monthly complaints trend
#Extract the month from the 'Date' column
comcast_fcc_complaints_2015$Month <- format(comcast_fcc_complaints_2015$Date, "%m-%Y")
#Aggregate data to get the number of complaints per month
monthly_complaints <- aggregate(`Ticket #` ~ Month, comcast_fcc_complaints_2015, length)
#Plot the monthly complaint count:
library(ggplot2)
ggplot(monthly_complaints, aes(x=Month, y=`Ticket #`)) +
  geom_line(group=1) + 
  geom_point() +
  labs(title="Trend of Monthly Complaints", x="Month", y="Number of Complaints") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Table of frequency types
# Compute the frequency of complaint types
complaint_freq <- table(comcast_fcc_complaints_2015$`Customer Complaint`)
# Convert it to a data frame for better visualization and ordering
complaint_freq_df <- as.data.frame(complaint_freq)
colnames(complaint_freq_df) <- c("Complaint_Type", "Frequency")
# Order the data frame by frequency (optional, but helps in identifying top complaint types)
complaint_freq_df <- complaint_freq_df[order(-complaint_freq_df$Frequency), ]
# Display the table
head(complaint_freq_df)

# Load the ggplot2 library
library(ggplot2)
# Plotting the top N complaint types for clarity. Let's say N=10 for this example.
N <- 10
top_complaints <- head(complaint_freq_df, n=50)
# Create the bar graph
ggplot(top_complaints, aes(x=reorder(Complaint_Type, -Frequency), y=Frequency)) +
  geom_bar(stat="identity") +
  coord_flip() +  # This makes the graph horizontal for better readability
  labs(title="Top 50 Complaint Types", x="Complaint Type", y="Frequency") +
  theme_minimal()

# Load necessary libraries
install.packages(c("tm", "stringr"))
library(tm)
library(stringr)
# Text preprocessing
complaints <- as.character(comcast_fcc_complaints_2015$`Customer Complaint`)
complaints_clean <- tolower(complaints)
complaints_clean <- removePunctuation(complaints_clean)
complaints_clean <- removeNumbers(complaints_clean)
complaints_clean <- removeWords(complaints_clean, stopwords("en"))
# Group by keywords (basic approach)
complaints_clean <- ifelse(str_detect(complaints_clean, "bill"), "billing issues",
                           ifelse(str_detect(complaints_clean, "service"), "service issues",
                                  ifelse(str_detect(complaints_clean, "internet"), "internet issues",
                                         "others")))
# Compute the frequency of grouped complaints
complaint_freq_grouped <- table(complaints_clean)
# Convert to data frame for plotting
complaint_freq_grouped_df <- as.data.frame(complaint_freq_grouped)
# Plotting the grouped complaints
ggplot(complaint_freq_grouped_df, aes(x=complaints_clean, y=Freq)) +
  geom_bar(stat="identity") +
  labs(title="Grouped Complaint Types", x="Complaint Type", y="Frequency") +
  theme_minimal()

# Filter original complaints that were categorized as "others"
others_complaints <- complaints[complaints_clean == "others"]
# Count unique complaints under "others"
unique_others_count <- length(unique(others_complaints))
unique_others_count

# Extract frequencies of 'others' complaints
others_freq <- table(others_complaints)
# Convert to data frame and sort by frequency
others_freq_df <- as.data.frame(others_freq)
others_freq_df <- others_freq_df[order(-others_freq_df$Freq),]
# Display top N frequent 'others' complaints. Let's say N=10 for this example.
N <- 10
head(others_freq_df, n=20)

# Combine initial and refined grouping logic
complaints_grouped <- ifelse(str_detect(complaints, "bill"), "billing issues",
                             ifelse(str_detect(complaints, "service"), "service issues",
                                    ifelse(str_detect(complaints, "internet"), "internet issues",
                                           ifelse(str_detect(complaints, "speed"), "speed issues",
                                                  ifelse(str_detect(complaints, "outage"), "outage issues",
                                                         ifelse(str_detect(complaints, "price|charge"), "pricing issues",
                                                                ifelse(str_detect(complaints, "customer"), "customer service issues",
                                                                       "others")))))))

# Compute the frequency of the consolidated grouped complaints
complaint_freq_combined <- table(complaints_grouped)
# Convert to data frame for plotting
complaint_freq_combined_df <- as.data.frame(complaint_freq_combined)
# Plotting the consolidated grouped complaints
ggplot(complaint_freq_combined_df, aes(x=complaints_grouped, y=Freq)) +
  geom_bar(stat="identity") +
  labs(title="Consolidated Grouped Complaint Types", x="Complaint Type", y="Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#clean
#remove duplicates
comcast_fcc_complaints_2015 <- unique(comcast_fcc_complaints_2015)

#aggregate the data
daily_complaints <- aggregate(`Ticket #` ~ Date, comcast_fcc_complaints_2015, length)

#
install.packages(c("forecast","tseries"))
library(forecast)
library(ggplot2)
library(tseries)
# Assuming a weekly seasonality
ts_data <- ts(daily_complaints$'Ticket #', frequency=7)  
adf.test(ts_data)
fit <- auto.arima(ts_data)
summary(fit)
future_points = 30  # Forecast the next 30 days, for example
forecast_results <- forecast(fit, h=future_points)
plot(forecast_results)
future_points = 30  # Forecast the next 30 days, for example
forecast_results <- forecast(fit, h=future_points)
plot(forecast_results)

#check column names
names(comcast_fcc_complaints_2015)

#use base R plotting functions to label the plot.
plot(forecast_results, main="Forecast of Comcast FCC Complaints", xlab="Date", ylab="Number of Complaints")
# Annotate a specific point, replace `your_date` and `your_value` with the actual date and value you want to annotate.
# Sample annotation
sample_date = as.Date("2023-11-01") # Replace this with the desired date.
sample_value = 25  # Replace this with the desired forecasted complaint count.
plot(forecast_results, main="Forecast of Comcast FCC Complaints", xlab="Date", ylab="Number of Complaints")
text(x=sample_date, y=sample_value, labels="Annotation", pos=3)

#
# Extract the last date from your original dataset
#last_date <- max(comcast_fcc_complaints_2015$Date)

# Create an extended sequence of dates for the forecasted periods
#forecast_dates <- seq(from = last_date + 1, 
                      #by = "day", 
                      #length.out = length(forecast_results$mean))

# Combine dates from the original and forecasted periods
all_dates <- c(comcast_fcc_complaints_2015$Date, forecast_dates)


# Extract the dates corresponding to the original time series data
original_dates <- comcast_fcc_complaints_2015$Date[1:length(forecast_results$x)]

# Combine the original dates and the forecasted dates
all_dates <- c(original_dates, forecast_dates)

# Construct the forecast_data dataframe
forecast_data <- data.frame(
  Date = all_dates,
  Forecast = c(forecast_results$x, forecast_results$mean),
  Lower80 = c(rep(NA, length(forecast_results$x)), forecast_results$lower[,1]),
  Upper80 = c(rep(NA, length(forecast_results$x)), forecast_results$upper[,1]),
  Lower95 = c(rep(NA, length(forecast_results$x)), forecast_results$lower[,2]),
  Upper95 = c(rep(NA, length(forecast_results$x)), forecast_results$upper[,2])
)

library(ggplot2)

# Plotting
ggplot(data = forecast_data, aes(x = Date, y = Forecast)) +
  geom_line(color = "blue", aes(y = Forecast)) +  # Original and forecasted values
  geom_ribbon(aes(ymin = Lower80, ymax = Upper80), fill = "blue", alpha = 0.2) +  # 80% confidence interval
  geom_ribbon(aes(ymin = Lower95, ymax = Upper95), fill = "blue", alpha = 0.1) +  # 95% confidence interval
  labs(title = "Time Series Forecast of Comcast FCC Complaints",
       x = "Date",
       y = "Number of Complaints") +
  theme_minimal()



