
#### F O R E C A S T I N G   M O D E L ####
setwd("C:/Users/Juan/OneDrive - Georgia State University/APPROVAL VS MACHINE LEARNING")

library(academictwitteR)
library(dplyr)
library(tidyverse)

# 
names_list <- c("ALBANESE", "NEHAMMER", "DECROO", "LULA", "TRUDEAU", "FIALA",
        "MACRON", "SCHOLZ", "MODI", "VARADKAR", "MELONI", "KISHIDA",
        "AMLO", "RUTTE", "GAHRSTORE", "MORAWIECKI", "SEOKYOUL", "SANCHEZ", "BERSET",
        "SUNAK", "BIDEN")  

username_list <- c("AlboMP", "karlnehammer", "alexanderdecroo", "LulaOficial", "JustinTrudeau", "P_Fiala",
           "EmmanuelMacron", "OlafScholz", "narendramodi","LeoVaradkar", "GiorgiaMeloni", "kishida230",
           "lopezobrador_", "MinPres", "jonasgahrstore", "MorawieckiM", "moonriver365", "sanchezcastejon", "alain_berset",
           "RishiSunak", "JoeBiden")

library(rtweet)

create_token(
  app = "tajamar",
  consumer_key = "gzuTkqjgnjb5KHvWsWv4Zh30v",
  consumer_secret = "V9iuXYQOdmCnGuhNoXXaORHnV74hGsmvnpgTC5IAW669LmQ6Bm",
  access_token = "3221574271-adiHqim4ZJt7peaShEK8DXtCkMHo0D1DKIAACRd",
  access_secret = "U3Mpv2nsUjSowavG1ZQaUTSAnI7pXXmzWFPdaXFnKtgpC")

library(academictwitteR)
Bidentweets <- get_all_tweets(users = c("JoeBiden"),
                         start_tweets = "2021-01-01T00:00:00Z",
                         end_tweets = "2021-02-01T00:00:00Z",
                         bearer_token = get_bearer(),
                         n = 15000)


# Get the current date
current_date <- format(Sys.Date(), "%Y-%m-%dT00:00:00Z")

# Loop through the names list
for (i in 1:length(names_list)) {
  # Get tweets for the current name and username
  tweets <- get_all_tweets(users = c(username_list[i]),
           start_tweets = "2018-01-01T00:00:00Z",
           end_tweets = current_date,
           bearer_token = get_bearer(),
           n = 15000000)
  
  # Unnest the tweets
  tweets <- tidyr::unnest(tweets)
  
  # Select the desired columns
  tweets <- subset(tweets, select = c(text, id, lang, created_at, retweet_count, reply_count, like_count, quote_count, possibly_sensitive, in_reply_to_user_id, type), row.names = FALSE)
  
  # Write the tweets to CSV file
  write.csv(tweets, paste0("tweets", names_list[i], ".csv"))
}



########## Database Tweets Clean Up ##############
########## Database Tweets Clean Up ##############
########## Database Tweets Clean Up ##############

# Load required library
library(dplyr)



###### LOOP #########

# Define the list of executive names
names_list <- c("ALBANESE", "NEHAMMER", "DECROO", "LULA", "TRUDEAU", "FIALA", "MACRON", "SCHOLZ", "MODI", "VARADKAR", "MELONI", "KISHIDA", "AMLO", "RUTTE", "GAHRSTORE", "MORAWIECKI", "SEOKYOUL", "SANCHEZ", "BERSET", "SUNAK", "BIDEN")

# Define the list of corresponding country names
country_list <- c("AUSTRALIA", "AUSTRIA", "BELGIUM", "BRAZIL", "CANADA", "CZECHREP", "FRANCE", "GERMANY", "INDIA", "IRELAND", "ITALY", "JAPON", "MEXICO", "NEATHERLANDS", "NORWAY",  "POLAND", "SOUTHKOREA", "SPAIN", "SWITZERLAND", "UK", "USA")


# Define empty data frames to store the results
daily <- data.frame()
weekly <- data.frame()

# Loop through the names list
for (i in 1:length(names_list)) {
  # Read the CSV file for each name
  data <- read.csv(paste0("tweets", names_list[i], ".csv"))
  
  # Convert the 'date' column to the Date format
  data$date <- as.Date(data$created_at)
  
  # Group by date and calculate the sum of values for each day
  daily_temp <- data %>%
    group_by(date) %>%
    summarise(retweet_count = sum(retweet_count),
      reply_count = sum(reply_count),
      like_count = sum(like_count),
      quote_count = sum(quote_count),
      number_of_tweets = n())
  
  # Add the corresponding country name and executive name
  daily_temp$country <- country_list[i]
  daily_temp$executive <- names_list[i]
  
  # Calculate the average counts
  daily_temp$avgRTcount <- daily_temp$retweet_count / daily_temp$number_of_tweets
  daily_temp$avgREPLYcount <- daily_temp$reply_count / daily_temp$number_of_tweets
  daily_temp$avgLIKEcount <- daily_temp$like_count / daily_temp$number_of_tweets
  daily_temp$avgQUOTEcount <- daily_temp$quote_count / daily_temp$number_of_tweets
  
  # Append the current name's daily data to the overall daily data
  daily <- rbind(daily, daily_temp)
  
  # Group by consecutive weeks and calculate the sum of values for each week and count of entries
  weekly_temp <- daily_temp %>%
    mutate(week_start = floor_date(date, "week"),
   week_end = ceiling_date(date, "week") - days(1)) %>%
    group_by(week_start, week_end) %>%
    summarise(retweet_count = sum(retweet_count),
      reply_count = sum(reply_count),
      like_count = sum(like_count),
      quote_count = sum(quote_count),
      number_of_tweets = sum(number_of_tweets),
      count_entries = n())
  
  # Add the corresponding country name and executive name
  weekly_temp$country <- country_list[i]
  weekly_temp$executive <- names_list[i]
  
  # Calculate the average counts
  weekly_temp$avgRTcount <- weekly_temp$retweet_count / weekly_temp$number_of_tweets
  weekly_temp$avgREPLYcount <- weekly_temp$reply_count / weekly_temp$number_of_tweets
  weekly_temp$avgLIKEcount <- weekly_temp$like_count / weekly_temp$number_of_tweets
  weekly_temp$avgQUOTEcount <- weekly_temp$quote_count / weekly_temp$number_of_tweets
  
  # Append the current name's weekly data to the overall weekly data
  weekly <- rbind(weekly, weekly_temp)
  
  # Write the dataframes to CSV files
  write.csv(daily_temp, paste0(names_list[i], "daily.csv"))
  write.csv(weekly_temp, paste0(names_list[i], "weekly.csv"))
}


####### MODEL ##########
####### MODEL ##########
####### MODEL ##########

Approval = read.csv("output.csv")

# Define the list of executive names
names_list <- c("ALBANESE", "NEHAMMER", "DECROO", "LULA", "TRUDEAU", "FIALA", "MACRON", "SCHOLZ", "MODI", "VARADKAR", "MELONI", "KISHIDA", "AMLO", "RUTTE", "GAHRSTORE", "MORAWIECKI", "SEOKYOUL", "SANCHEZ", "BERSET", "SUNAK", "BIDEN")

# Create an empty dataframe to store the combined data
combined_data <- data.frame()

# Loop through each name
for (name in names_list) {
  # Generate the file name
  file_name <- paste0(name, "daily.csv")
  
  # Read the file
  data <- read.csv(file_name)
  
  # Append the data to the combined dataframe
  combined_data <- bind_rows(combined_data, data)
}

# Write the combined data to a new CSV file
write.csv(combined_data, "combined_data.csv", row.names = FALSE)


## Get weekly approval avg 

# Convert date string to date object
Approval$date <- mdy(Approval$date)

# Format date object to YYYY-MM-DD
Approval$date <- format(Approval$date, "%Y-%m-%d")

Approval$date  = as.Date(Approval$date)



# Group by consecutive weeks and calculate the sum of values for each week and count of entries
#Approvaldaily <- Approval %>%
#  group_by(Executive) %>%
#  mutate(week_start = floor_date(date, "day"),
#         week_end = ceiling_date(date, "day") - days(1)) %>%
#  group_by(Country, week_start, week_end) %>%
 # summarise(Approval = sum(Approval),
  #          Disapproval = sum(Disapproval),
   ##         Country = Country,
     #       Executive = Executive,
      #      count_entries = n()) %>%
  #distinct()

#Approvalweekly$approvalAVG = Approvalweekly$Approval/Approvalweekly$count_entries
#Approvalweekly$disapprovalAVG = Approvalweekly$Disapproval/Approvalweekly$count_entries

# Change the name of a specific column
# colnames(Approvaldaily)[1] <- "country"
# colnames(Approvaldaily)[6] <- "executive"
# 
# 
# #0 Group by consecutive weeks and calculate the sum of values for each week and count of entries
# Approvalweekly <- Approval %>%
#   group_by(Executive) %>%
#   mutate(week_start = floor_date(date, "week"),
#          week_end = ceiling_date(date, "week") - days(1)) %>%
#   group_by(Country, week_start, week_end) %>%
#   summarise(Approval = sum(Approval),
#     Disapproval = sum(Disapproval),
#     Country = Country,
#     Executive = Executive,
#     count_entries = n()) %>%
#   distinct()

# Approvalweekly$approvalAVG = Approvalweekly$Approval/Approvalweekly$count_entries
# Approvalweekly$disapprovalAVG = Approvalweekly$Disapproval/Approvalweekly$count_entries



# Change the name of a specific column
colnames(Approval)[2] <- "country"
colnames(Approval)[1] <- "executive"


# Merge data frames by country and date
# Merge dataframes based on matching columns

# Perform inner join based on executive and week_start
merged_df <- inner_join(daily, Approval, by = c("executive", "date"))


########################
#Descriptive Statistics#
########################

# Create a list to store the separate data frames
executive_dfs <- list()

# Loop through each executive
for (executive in unique(merged_df$executive)) {
  # Subset the data for the current executive
  executive_data <- merged_df[merged_df$executive == executive, ]
  
  # Assign the subsetted data to a new data frame with the executive's name
  executive_dfs[[executive]] <- executive_data
  
  # Send the data frame to the global environment
  assign(paste0(executive, "_df"), executive_data, envir = .GlobalEnv)
}

#######

# Loop through each executive name
for (executive_name in executive_names) {
  # Get the data frame from the global environment
  df <- get(paste0(executive_name, "_df"))
  
  # Convert date column to Date format
  df$date <- as.Date(df$date)
  
  # Find the minimum and maximum dates
  min_date <- min(df$date)
  max_date <- max(df$date)
  
  # Create a sequence of dates from min_date to max_date
  all_dates <- seq(min_date, max_date, by = "day")
  
  # Use complete() to fill in missing dates with empty rows
  df_filled <- complete(df, date = all_dates)
  
  # Assign the filled data frame back to the global environment
  assign(paste0(executive_name, "_filled_df"), df_filled, envir = .GlobalEnv)
}

#### 

# Load the tidyr package
library(tidyr)

# List of executive names
executive_names <- c("ALBANESE", "NEHAMMER", "DECROO", "LULA", "TRUDEAU", "FIALA", "MACRON",
                     "SCHOLZ", "MODI", "VARADKAR", "MELONI", "KISHIDA", "AMLO", "RUTTE",
                     "GAHRSTORE", "MORAWIECKI", "SEOKYOUL", "SANCHEZ", "BERSET", "SUNAK", "BIDEN")

# Loop through each executive name
for (executive_name in executive_names) {
  # Get the data frame from the global environment
  df <- get(paste0(executive_name, "_filled_df"))
  
  # Count number of missing values for 'number_of_tweets'
  missing_count <- sum(is.na(df$number_of_tweets))
  
  # Calculate the percentage of missing values
  total_rows <- nrow(df)
  missing_percentage <- (missing_count / total_rows) * 100
  
  # Print the results
  cat("Executive:", executive_name, "\n")
  cat("Missing Count:", missing_count, "\n")
  cat("Missing Percentage:", missing_percentage, "%\n")
  cat("\n")
}




# Create empty vectors to store the results
executive_names <- c("ALBANESE", "NEHAMMER", "DECROO", "LULA", "TRUDEAU", "FIALA", "MACRON",
                     "SCHOLZ", "MODI", "VARADKAR", "MELONI", "KISHIDA", "AMLO", "RUTTE",
                     "GAHRSTORE", "MORAWIECKI", "SEOKYOUL", "SANCHEZ", "BERSET", "SUNAK", "BIDEN")
missing_values_count <- numeric(length(executive_names))
percentage_missing <- numeric(length(executive_names))
average_tweets_per_day <- numeric(length(executive_names))
total_tweets <- numeric(length(executive_names))

# Loop through each executive name
for (i in seq_along(executive_names)) {
  executive_name <- executive_names[i]
  
  # Get the filled data frame from the global environment
  df_filled <- get(paste0(executive_name, "_filled_df"))
  
  # Count the number of missing values in the "number_of_tweets" column
  missing_values_count[i] <- sum(is.na(df_filled$number_of_tweets))
  
  # Calculate the percentage of missing values relative to total rows
  percentage_missing[i] <- (missing_values_count[i] / nrow(df_filled)) * 100
  
  # Calculate the average number of tweets per day
  average_tweets_per_day[i] <- mean(df_filled$number_of_tweets, na.rm = TRUE)
  
  # Calculate the total number of tweets
  total_tweets[i] <- sum(df_filled$number_of_tweets, na.rm = TRUE)
}

# Create a data frame from the results
results_table <- data.frame(
  Executive_Name = executive_names,
  Missing_Values_Count = missing_values_count,
  Percentage_Missing = percentage_missing,
  Average_Tweets_Per_Day = average_tweets_per_day,
  Total_Tweets = total_tweets
)


# Install the xtable package 
install.packages("xtable")

# Load the xtable package
library(xtable)

# Convert the results_table data frame to an xtable object
results_table_xtab <- xtable(results_table)

# Specify table caption
caption <- "Summary of Executive Data"

# Specify table label
label <- "executive_summary"

# Print the LaTeX code for the table
print(xtable(results_table_xtab,
             caption = caption,
             label = label),
      include.rownames = FALSE,
      caption.placement = "top",
      sanitize.text.function = function(x) x)  # Prevent sanitizing LaTeX special characters



library(dplyr)


merged_df$netapproval = (merged_df$Approval-merged_df$Disapproval)


# # Normalize the 'value' column within each group
# merged_df <- merged_df %>% 
#   group_by(executive) %>%
#   mutate(normRT = (avgRTcount - min(avgRTcount)) / (max(avgRTcount) - min(avgRTcount)),
#          normREPLY = (avgREPLYcount - min(avgREPLYcount)) / (max(avgREPLYcount) - min(avgREPLYcount)),
#          normLIKE = (avgLIKEcount - min(avgLIKEcount)) / (max(avgLIKEcount) - min(avgLIKEcount)),
#          normQUOTE = (avgQUOTEcount - min(avgQUOTEcount)) / (max(avgQUOTEcount) - min(avgQUOTEcount)))
# 
# merged_df <- merged_df %>% 
#   group_by(executive) %>%
#   mutate(normApproval = (Approval - min(Approval)) / (max(Approval) - min(Approval)),
#          normDisapproval = (Disapproval - min(Disapproval)) / (max(Disapproval) - min(Disapproval)),
#          normnetapproval = (netapproval - min(netapproval)) / (max(netapproval) - min(netapproval)))
# 
# 
# plot(merged_dfSUB2500$normRT, merged_dfSUB2500$netapproval)

#merged_dfSUB2500 = subset(merged_df, avgRTcount <= 2500)


merged_df <- merged_df %>%
  group_by(executive) %>%
  mutate(approvalDIFF = Approval - lag(Approval),
         disapprovalDIFF = Disapproval - lag(Disapproval),
         netapprovalDIFF = netapproval - lag(netapproval),
         RTDIFF = normRT - lag(normRT),
         replyDIFF = normREPLY - lag(normREPLY),
         likeDIFF = normLIKE - lag(normLIKE),
         quoteDIFF = normQUOTE - lag(normQUOTE))



merged_df <- merged_df %>%
  group_by(executive) %>%
  mutate(RTDIFFlag = lag(RTDIFF),
         replyDIFFlag = lag(replyDIFF),
         likeDIFFlag = lag(likeDIFF),
         quoteDIFFlag = lag(quoteDIFF))


merged_df <- merged_df %>%
  group_by(executive) %>%
  mutate(RTDIFFlag = lag(RTDIFFlag),
         replyDIFFlag = lag(replyDIFFlag),
         likeDIFFlag = lag(likeDIFFlag),
         quoteDIFFlag = lag(quoteDIFFlag))

plot(merged_df$netapprovalDIFF, merged_df$likeDIFFlag)

summary(merged_df$RTDIFFlag)


# Calculate difference/change for each group and classify values


##### THINK ABOUT INCORPORATING VERY POS AND VERY NEG
##### THINK ABOUT INCORPORATING VERY POS AND VERY NEG
##### THINK ABOUT INCORPORATING VERY POS AND VERY NEG

merged_df <- merged_df %>%
    mutate(approvalTrend = case_when(
    approvalDIFF > 0.9 ~ "Positive",
    approvalDIFF > 0.1 ~ "Equal",
   TRUE ~ "Negative"
         ))

merged_df <- merged_df %>%
  mutate(disapprovalTrend = case_when(
    disapprovalDIFF > 0.9 ~ "Positive",
    disapprovalDIFF > 0.1 ~ "Equal",
    TRUE ~ "Negative"
  ))


merged_df <- merged_df %>%
  mutate(netapprovalTrend = case_when(
    netapprovalDIFF > 0.9 ~ "Positive",
    netapprovalDIFF > 0.1 ~ "Equal",
    TRUE ~ "Negative"
  ))


merged_df <- merged_df[complete.cases(merged_df), ]




############# RANDOM FOREST ###############

# Install required packages if not already installed
# install.packages("randomForest")
   
# Load the required library
library(randomForest)
library(caret)

# Subset the data for the variables of interest (Approval, avgRTcount, avgLIKEcount)
data_subset <- merged_df[, c("disapprovalDIFF", "RTDIFFlag", "replyDIFFlag", "likeDIFFlag", "quoteDIFFlag", "executive")]

# Subset the data for the variables of interest (Approval, avgRTcount, avgLIKEcount)
data_subset <- merged_df[, c("netapprovalDIFF", "RTDIFFlag", "likeDIFFlag", "executive")]
   
# Convert the data to a time series object
ts_data <- ts(data_subset, frequency = 1)  # Assuming weekly data
   
# Create lagged variables
RT <- (ts_data[, "RTDIFFlag"])
REPLY <- (ts_data[, "replyDIFFlag"])
LIKE <- (ts_data[, "likeDIFFlag"])
QUOTE <- (ts_data[, "quoteDIFFlag"])
EXECUTIVE <- (ts_data[, "executive"])
   
# Combine the variables into a new data frame
   data <- data.frame(Disapproval = ts_data[, "disapprovalDIFF"],
             RT,
         REPLY,
            LIKE,
        QUOTE,
            EXECUTIVE)
   
   # Remove missing values
   #lagged_data <- na.omit(lagged_data)
   
   # Split the data into training and test sets
   train_data <- data[1:(nrow(data) - 7), ]
   test_data <- data[(nrow(data) - 6):nrow(data), ]
   
   
   # Set seed for reproducibility
   set.seed(123)
   
   # Split data into 70% training and 30% test sets
   trainIndex <- createDataPartition(data$Disapproval, p = 0.8, list = FALSE)
   train_data <- data[trainIndex, ]
   test_data <- data[-trainIndex, ]
   
# Fit the Random Forest model
model <- randomForest(Disapproval ~ ., data = train_data, ntree = 600)

# Make predictions on the test data
predictions <- predict(model, newdata = test_data)
   
# Print the predicted approval levels
#print(predictions)

   
   
# Calculate accuracy metrics
accuracy_metrics <- data.frame(
     MAE = mean(abs(predictions - test_data$Disapproval)),
     RMSE = sqrt(mean((predictions - test_data$Disapproval)^2)))
   
print(accuracy_metrics)



actual = test_data$Disapproval

# Function to calculate sMAPE
smape <- function(actual, predictions) {
  numerator <- abs(predictions - actual)
  denominator <- (abs(actual) + abs(predictions)) / 2
  smape_values <- 100 * numerator / denominator
  mean(smape_values, na.rm = TRUE)
}

smape_value <- smape(actual, predictions)
print(smape_value)  
   
# Load the required library
   library(randomForest)
   library(caret)
   
summary(merged_df$netapprovalDIFF)
   
   merged_df <- merged_df %>%
     mutate(netapprovalTrend = case_when(
       netapprovalDIFF > 5 ~ "Very Positive",
       netapprovalDIFF > 0.9 ~ "Positive",
       netapprovalDIFF > -0.9 ~ "Equal",
       netapprovalDIFF >= -5 ~ "Negative",
       netapprovalDIFF < -5 ~ "Very Negative"
     ))
   
   
   merged_df <- merged_df %>%
     mutate(approvalTrend = case_when(
       approvalDIFF > 0 ~ "Positive",
       approvalDIFF < 0 ~ "Negative",
       TRUE ~ "Equal"
     ))
   
   merged_dfMODI = subset(merged_df, executive == "ALBANESE" | executive == "BIDEN")
   
   # Subset the data for the variables of interest (Approval, avgRTcount, avgLIKEcount)
   data <- merged_df[, c("approvalTrend", "RTDIFFlag", "replyDIFFlag", "likeDIFFlag", "quoteDIFFlag", "executive")]
   
   data$approvalTrend = as.factor(data$approvalTrend)
   
   # Convert the data to a time series object
   # ts_data <- ts(data_subset, frequency = 1)  # Assuming daily data
   # 
   # # Create lagged variables
   # RT <- (ts_data[, "RTDIFFlag"])
   # REPLY <- (ts_data[, "replyDIFFlag"])
   # LIKE <- (ts_data[, "likeDIFFlag"])
   # QUOTE <- (ts_data[, "quoteDIFFlag"])
   # executive <- (ts_data[, "quoteDIFFlag"])
   # 
   # Combine the variables into a new data frame
   
   #data <- data.frame(NetApproval = data[, "netapprovalTrend"],
      # RT,
      # REPLY,
      # LIKE,
      # QUOTE,
      # executive)
      # 
   
   
   # Remove missing values
   #lagged_data <- na.omit(lagged_data)
   
# Split the data into training and test sets
#train_data <- data[1:(nrow(data) - 7), ]
#test_data <- data[(nrow(data) - 6):nrow(data), ]
   
   
# Set seed for reproducibility
set.seed(123)
   
# Split data into 70% training and 30% test sets
trainIndex <- createDataPartition(data$approvalTrend, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]
   

# Train the random forest classifier
model <- randomForest(approvalTrend ~ ., data = train_data, class.factors = levels(approvalTrend))
   

# Make predictions on the test data
predictions <- predict(model, newdata = test_data)
   
# Print the predicted approval levels
print(predictions)
   
   
# Evaluate the accuracy of the model
accuracy <- sum(predictions == test_data$approvalTrend) / length(test_data$approvalTrend)
cat("Accuracy:", accuracy, "\n")
   
   
# Create the confusion matrix
confusion_matrix <- table(Actual = test_data$approvalTrend, Predicted = predictions)



###########
##########

summary(merged_df$approvalDIFF)
summary(merged_df$disapprovalDIFF)
summary(merged_df$netapprovalDIFF)
summary(merged_df$RTDIFFlag)



# Perform inner join based on executive and week_start
merged_df <- inner_join(daily, Approval, by = c("executive", "date"))

merged_df$netapproval = (merged_df$Approval-merged_df$Disapproval)


# Group by consecutive weeks and calculate the sum of values for each week and count of entries
library(dplyr)

weekly <- merged_df %>%
  group_by(executive) %>%
  mutate(week_start = floor_date(date, "week"),
         week_end = ceiling_date(date, "week") - days(1)) %>%
  group_by(country.x, week_start, week_end) %>%
  summarise(Approval = sum(Approval),
    Disapproval = sum(Disapproval),
    Country = country.x,
    Executive = executive,
    retweet_count = sum(avgRTcount),
    reply_count = sum(avgREPLYcount),
    like_count = sum(avgLIKEcount),
    quote_count = sum(avgQUOTEcount),
    count_entries = n()) %>%
  distinct()


weekly$avgApproval = weekly$Approval/weekly$count_entries
weekly$avgDisapproval = weekly$Disapproval/weekly$count_entries
weekly$avgRT = weekly$retweet_count/weekly$count_entries
weekly$avgREPLY = weekly$reply_count/weekly$count_entries
weekly$avgLIKES = weekly$like_count/weekly$count_entries
weekly$avgQUOTE = weekly$quote_count/weekly$count_entries

weekly <- weekly %>%
  group_by(Executive) %>%
  mutate(approvalDIFF = avgApproval - lag(avgApproval),
         disapprovalDIFF = avgDisapproval - lag(avgDisapproval),
         #netapprovalDIFF = netapproval - lag(netapproval),
         RTDIFF = avgRT - lag(avgRT),
         replyDIFF = avgREPLY - lag(avgREPLY),
         likeDIFF = avgLIKES - lag(avgLIKES),
         quoteDIFF = avgQUOTE - lag(avgQUOTE))


weekly <- weekly %>%
  group_by(Executive) %>%
  mutate(RTDIFFlag = lag(RTDIFF),
         replyDIFFlag = lag(replyDIFF),
         likeDIFFlag = lag(likeDIFF),
         quoteDIFFlag = lag(quoteDIFF))

weekly <- weekly[complete.cases(weekly), ]

library(randomForest)
library(caret)

# Subset the data for the variables of interest (Approval, avgRTcount, avgLIKEcount)
data_subset <- weekly[, c("approvalDIFF", "RTDIFFlag", "replyDIFFlag", "likeDIFFlag",
          "quoteDIFFlag", "Executive")]



# Convert the data to a time series object
ts_data <- ts(data_subset, frequency = 7)  # Assuming weekly data

# Create lagged variables
RT <- (ts_data[, "RTDIFFlag"])
REPLY <- (ts_data[, "replyDIFFlag"])
LIKE <- (ts_data[, "likeDIFFlag"])
QUOTE <- (ts_data[, "quoteDIFFlag"])
EXECUTIVE <- (ts_data[, "Executive"])

# Combine the variables into a new data frame
data <- data.frame(APPROVAL = ts_data[, "approvalDIFF"],
           RT,
           REPLY,
           LIKE,
           QUOTE,
           EXECUTIVE)




# Remove missing values
#lagged_data <- na.omit(lagged_data)

# Split the data into training and test sets
train_data <- data[1:(nrow(data) - 7), ]
test_data <- data[(nrow(data) - 6):nrow(data), ]


# Set seed for reproducibility
set.seed(123)

# Split data into 70% training and 30% test sets
trainIndex <- createDataPartition(data$APPROVAL, p = 0.8, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Fit the Random Forest model
model <- randomForest(APPROVAL ~ ., data = train_data, ntree = 100)

# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Print the predicted approval levels
print(predictions)



# Calculate accuracy metrics
accuracy_metrics <- data.frame(
  MAE = mean(abs(predictions - test_data$APPROVAL)),
  RMSE = sqrt(mean((predictions - test_data$APPROVAL)^2)))

print(accuracy_metrics)



actual = test_data$APPROVAL

# Function to calculate sMAPE
smape <- function(actual, predictions) {
  numerator <- abs(predictions - actual)
  denominator <- (abs(actual) + abs(predictions)) / 2
  smape_values <- 100 * numerator / denominator
  mean(smape_values, na.rm = TRUE)
}

smape_value <- smape(actual, predictions)
print(smape_value)  



########## NO OUTLIERS ##########

#### Approval ###
library(dplyr)
merged_df <- inner_join(daily, Approval, by = c("executive", "date"))

merged_df$netapproval = merged_df$Approval-merged_df$Disapproval

merged_df <- merged_df %>%
  mutate(start = ifelse(executive == "ALBANESE", "2022-05-23",
        ifelse(executive == "NEHAMMER", "2021-12-06",
               ifelse(executive == "AMLO", "2018-12-01",
              ifelse(executive == "LULA", "2023-01-01",
             ifelse(executive == "BIDEN", "2021-01-20",
                    ifelse(executive == "KISHIDA", "2021-10-04",
                   ifelse(executive == "TRUDEAU", "2015-11-04",
    ifelse(executive == "FIALA", "2021-11-28",
   ifelse(executive == "MACRON", "2017-05-14",
          ifelse(executive == "SCHOLZ", "2021-12-08",
         ifelse(executive == "MODI", "2014-05-26",
                ifelse(executive == "VARADKAR", "2022-12-17",
               ifelse(executive == "MELONI", "2022-10-22",
              ifelse(executive == "RUTTE", "2010-10-14",
                     ifelse(executive == "GAHRSTORE", "2021-10-14",
                    ifelse(executive == "MORAWIECKI", "2017-12-11",
     ifelse(executive == "SEOKYOUL", "2022-05-10",
    ifelse(executive == "SANCHEZ", "2018-06-02",
           ifelse(executive == "BERSET", "2023-01-01",
          ifelse(executive == "SUNAK", "2022-10-25",
         ifelse(executive == "DECROO", "2020-10-01", NA))))))))))))))))))))))

merged_df$start =  as.Date(merged_df$start)

# Calculate the number of days between the two dates
merged_df$daysinoffice <- as.numeric(difftime(merged_df$date, merged_df$start, units = "days"))



# Calculate the lower and upper percentile thresholds
lower_thresholdRT <- quantile(merged_df$avgRTcount, 0.01)
upper_thresholdRT <- quantile(merged_df$avgRTcount, 0.99)

lower_thresholdReply <- quantile(merged_df$avgREPLYcount, 0.01)
upper_thresholdReply <- quantile(merged_df$avgREPLYcount, 0.99)

lower_thresholdQuote <- quantile(merged_df$avgQUOTEcount, 0.01)
upper_thresholdQuote <- quantile(merged_df$avgQUOTEcount, 0.99)

lower_thresholdLike <- quantile(merged_df$avgLIKEcount, 0.01)
upper_thresholdLike <- quantile(merged_df$avgLIKEcount, 0.99)



# merged_df <- merged_df %>%
#   group_by(executive) %>%
#   mutate(approvalDIFF = Approval - lag(Approval),
#          disapprovalDIFF = Disapproval - lag(Disapproval),
#          netapprovalDIFF = netapproval - lag(netapproval),
#          RTlag = lag(avgRTcount),
#          replylag = lag(avgREPLYcount),
#          likelag = lag(avgLIKEcount),
#          quotelag =lag(avgQUOTEcount))


merged_df <- merged_df %>%
  group_by(executive) %>%
  mutate(Approvallag = lag(Approval),
         Disapprovallag = lag(Disapproval),
         Netapprovallag = lag(netapproval),
         RTlag = lag(avgRTcount),
         replylag = lag(avgREPLYcount),
         likelag = lag(avgLIKEcount),
         quotelag =lag(avgQUOTEcount),
         datelag = lag(date),
         number_of_tweetslag = lag(number_of_tweets))
         


merged_df <- merged_df[complete.cases(merged_df), ]




# # Filter the data frame by removing values outside the threshold range
# filtered_df <- merged_df[merged_df$avgRTcount >= lower_thresholdRT & merged_df$avgRTcount <= upper_thresholdRT, ]
# filtered_df = filtered_df[filtered_df$avgREPLYcount >= lower_thresholdReply & filtered_df$avgREPLYcount <= upper_thresholdReply, ]
# filtered_df = filtered_df[filtered_df$avgQUOTEcount >= lower_thresholdQuote & filtered_df$avgQUOTEcount <= upper_thresholdQuote, ]
# filtered_df = filtered_df[filtered_df$avgLIKEcount >= lower_thresholdLike & filtered_df$avgLIKEcount <= upper_thresholdLike, ]



# Filter the data frame by removing values outside the threshold range for lagged variables
filtered_df <- merged_df[merged_df$RTlag >= lower_thresholdRT & merged_df$RTlag <= upper_thresholdRT, ]
filtered_df = filtered_df[filtered_df$replylag >= lower_thresholdReply & filtered_df$replylag <= upper_thresholdReply, ]
filtered_df = filtered_df[filtered_df$quotelag >= lower_thresholdQuote & filtered_df$quotelag <= upper_thresholdQuote, ]
filtered_df = filtered_df[filtered_df$likelag >= lower_thresholdLike & filtered_df$likelag <= upper_thresholdLike, ]


merged_df = filtered_df




library(randomForest)
library(caret)


####################### CONSIDER AN ANALYSIS OF THE OUTLIERS

# Best Model

data_subset <- merged_df[, c("Approval", "RTlag", "executive", "Approvallag", "Disapprovallag",
                             "Netapprovallag", "number_of_tweetslag", "daysinoffice" )]


# Pure APP
data_subset <- merged_df[, c("Approval", "executive", "Approvallag", "Disapprovallag",
             "Netapprovallag", "daysinoffice")]

# Set seed for reproducibility
set.seed(123)

# Split data into 70% training and 30% test sets
trainIndex <- createDataPartition(data_subset$Approval, p = 0.8, list = FALSE)
train_data <- data_subset[trainIndex, ]
test_data <- data_subset[-trainIndex, ]

# Fit the Random Forest model
model <- randomForest(Approval ~ ., data = train_data, ntree = 600, max_depth = 4)



# Purely SM 
data_subset <- merged_df[, c("Approval", "RTlag", "replylag", "likelag",
                             "quotelag", "executive", "daysinoffice")]

set.seed(123)
trainIndex <- createDataPartition(data_subset$Approval, p = 0.8, list = FALSE)
train_data <- data_subset[trainIndex, ]
test_data <- data_subset[-trainIndex, ]

model <- randomForest(Approval ~ ., data = train_data, ntree = 300, max_depth = 6)


# Best no exec

data_subset <- merged_df[, c("Approval", "RTlag", 
                              "Approvallag", "Disapprovallag",
                             "Netapprovallag", "number_of_tweetslag", "daysinoffice")]

set.seed(123)
trainIndex <- createDataPartition(data_subset$Approval, p = 0.8, list = FALSE)
train_data <- data_subset[trainIndex, ]
test_data <- data_subset[-trainIndex, ]

model <- randomForest(Approval ~ ., data = train_data, ntree = 200, max_depth = 7)



# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Calculate accuracy metrics
accuracy_metrics <- data.frame(
  MAE = mean(abs(predictions - test_data$Approval)),
  MAPE = mean(abs((test_data$Approval - predictions) / test_data$Approval) * 100, na.rm = TRUE))


print(accuracy_metrics)



# # Function to calculate MAE, RMSE, and MAPE
# calculate_metrics <- function(predictions, actual) {
#   mae <- mean(abs(predictions - actual))
#   mape <- mean(abs((predictions - actual) / actual)) * 100
#   return(list(MAE = mae, MAPE = mape))
# }
# 
# # Perform grid search and evaluate models
# results <- lapply(1:nrow(rf_grid), function(i) {
#   model <- randomForest(
#     Approval ~ .,                        # Formula for the model
#     data = train_data,                  # Training data
#     ntree = rf_grid$ntree[i],          # Number of trees
#     max_depth = rf_grid$max_depth[i]   # Maximum depth of the tree
#   )
#   
#   predictions <- predict(model, newdata = test_data)
#   actual <- test_data$Approval
#   
#   metrics <- calculate_metrics(predictions, actual)
#   metrics <- c(rf_grid$max_depth[i], rf_grid$ntree[i], metrics)
#   
#   return(metrics)
# })
# 
# 
# 
# # Convert the results to a data frame
# results_df <- do.call(rbind, results)
# colnames(results_df) <- c("max_depth", "ntree", "MAE", "MAPE")
# 
# # Print the results
# print(results_df)


test_data$Lag1AppNoExecutive =  predictions

write.csv(test_data, "Lag1AppNoExecutive.csv")



library(dplyr)


# List of names 
names_list <- c("ALBANESE", "NEHAMMER", "DECROO", "LULA", "TRUDEAU", "FIALA",
                "MACRON", "SCHOLZ", "MODI", "VARADKAR", "MELONI", "KISHIDA",
                "AMLO", "RUTTE", "GAHRSTORE", "MORAWIECKI", "SEOKYOUL", "SANCHEZ", "BERSET",
                "SUNAK", "BIDEN")

Lag1AppBestModel = read.csv("Lag1AppBestModel.csv")



# Create a list of data frames for each executive
executive_dfs <- split(Lag1AppBestModel, Lag1AppBestModel$executive)

# Iterate over the names in the names_list and assign a data frame for each executive
for (name in names_list) {
  # Create a data frame for the current executive
  exec_df <- executive_dfs[[name]]
  
  # Perform any additional operations on the data frame if needed
  
  # Assign the data frame to a variable named after the executive
  assign(name, exec_df)
}



# Create an empty data frame to store the results
results <- data.frame(executive = character(),
                      MAE = numeric(),
                      MAPE = numeric(),
                      MSE = numeric(),
                      RMSE = numeric(),
                      stringsAsFactors = FALSE)

# Loop through each executive in names_list
for (executive in names_list) {
  # Get the data frame for the current executive
  executive_data <- get(paste(executive, sep = ""))
  
  # Calculate the evaluation metrics
  MAE <- mean(abs(executive_data$Approval - executive_data$Lag1AppBestModel))
  MAPE <- mean(abs((executive_data$Approval - executive_data$Lag1AppBestModel) / executive_data$Approval)) * 100

  # Add the results to the results data frame
  results <- rbind(results, data.frame(executive = executive,
                                       MAE = MAE,
                                       MAPE = MAPE))
}

# Print the results
print(results)




library(beepr)

beep(8)

########## Disapproval ######



# Pure APP
data_subset <- merged_df[, c("Disapproval", "Approvallag", "Disapprovallag",
             "Netapprovallag", "daysinoffice")]

set.seed(123)

trainIndex <- createDataPartition(data_subset$Disapproval, p = 0.8, list = FALSE)
train_data <- data_subset[trainIndex, ]
test_data <- data_subset[-trainIndex, ]

model <- randomForest(Disapproval ~ ., data = train_data, ntree = 500, max_depth = 5)


# Pure SM
data_subset <- merged_df[, c("Disapproval", "RTlag", "replylag", "likelag",
             "quotelag", "executive", "daysinoffice")]


set.seed(123)

trainIndex <- createDataPartition(data_subset$Disapproval, p = 0.8, list = FALSE)
train_data <- data_subset[trainIndex, ]
test_data <- data_subset[-trainIndex, ]

model <- randomForest(Disapproval ~ ., data = train_data, ntree = 400, max_depth = 7)



# Best Model
data_subset <- merged_df[, c("Disapproval", "RTlag", "Disapprovallag", "executive", "replylag", 
             "Netapprovallag",  "daysinoffice")]

set.seed(123)

trainIndex <- createDataPartition(data_subset$Disapproval, p = 0.8, list = FALSE)
train_data <- data_subset[trainIndex, ]
test_data <- data_subset[-trainIndex, ]

model <- randomForest(Disapproval ~ ., data = train_data, ntree = 300, max_depth = 7)



# no executive
data_subset <- merged_df[, c("Disapproval", "RTlag", "replylag", 
             "quotelag", "Approvallag", "Disapprovallag",
             "Netapprovallag", "number_of_tweetslag", "daysinoffice")]

set.seed(123)

trainIndex <- createDataPartition(data_subset$Disapproval, p = 0.8, list = FALSE)
train_data <- data_subset[trainIndex, ]
test_data <- data_subset[-trainIndex, ]

model <- randomForest(Disapproval ~ ., data = train_data, ntree = 300, max_depth = 6)



# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Print the predicted approval levels
#print(predictions)

# Calculate accuracy metrics
accuracy_metrics <- data.frame(
  MAE = mean(abs(predictions - test_data$Disapproval)),
  RMSE = sqrt(mean((predictions - test_data$Disapproval)^2)),
  MSE = mean((test_data$Disapproval - predictions)^2),
  MAPE = mean(abs((test_data$Disapproval - predictions) / test_data$Disapproval) * 100, na.rm = TRUE))

print(accuracy_metrics)


test_data$Lag1DisNoExec =  predictions
 
write.csv(test_data, "Lag1DisNoExec.csv")


# List of names 
names_list <- c("ALBANESE", "NEHAMMER", "DECROO", "LULA", "TRUDEAU", "FIALA",
                "MACRON", "SCHOLZ", "MODI", "VARADKAR", "MELONI", "KISHIDA",
                "AMLO", "RUTTE", "GAHRSTORE", "MORAWIECKI", "SEOKYOUL", "SANCHEZ", "BERSET",
                "SUNAK", "BIDEN")

Lag1DisBestModel = read.csv("Lag1DisBestModel.csv")


# Create a list of data frames for each executive
executive_dfs <- split(Lag1DisBestModel, Lag1DisBestModel$executive)

# Iterate over the names in the names_list and assign a data frame for each executive
for (name in names_list) {
  # Create a data frame for the current executive
  exec_df <- executive_dfs[[name]]
  
  # Perform any additional operations on the data frame if needed
  
  # Assign the data frame to a variable named after the executive
  assign(name, exec_df)
}



# Create an empty data frame to store the results
results <- data.frame(executive = character(),
                      MAE = numeric(),
                      MAPE = numeric(),
                      stringsAsFactors = FALSE)

# Loop through each executive in names_list
for (executive in names_list) {
  # Get the data frame for the current executive
  executive_data <- get(paste(executive, sep = ""))
  
  # Calculate the evaluation metrics
  MAE <- mean(abs(executive_data$Disapproval - executive_data$Lag1DisBestModel))
  MAPE <- mean(abs((executive_data$Disapproval - executive_data$Lag1DisBestModel) / executive_data$Disapproval)) * 100
  
  # Add the results to the results data frame
  results <- rbind(results, data.frame(executive = executive,
                                       MAE = MAE,
                                       MAPE = MAPE))
}

# Print the results
print(results)





# 
# # Function to calculate MAE, RMSE, and MAPE
# calculate_metrics <- function(predictions, actual) {
#   mae <- mean(abs(predictions - actual))
#   mape <- mean(abs((predictions - actual) / actual)) * 100
#   return(list(MAE = mae, MAPE = mape))
# }
# 
# # Perform grid search and evaluate models
# results <- lapply(1:nrow(rf_grid), function(i) {
#   model <- randomForest(
#     Disapproval ~ .,                        # Formula for the model
#     data = train_data,                  # Training data
#     ntree = rf_grid$ntree[i],          # Number of trees
#     max_depth = rf_grid$max_depth[i]   # Maximum depth of the tree
#   )
#   
#   predictions <- predict(model, newdata = test_data)
#   actual <- test_data$Disapproval
#   
#   metrics <- calculate_metrics(predictions, actual)
#   metrics <- c(rf_grid$max_depth[i], rf_grid$ntree[i], metrics)
#   
#   return(metrics)
# })
# 
# 
# 
# # Convert the results to a data frame
# results_df <- do.call(rbind, results)
# colnames(results_df) <- c("max_depth", "ntree", "MAE", "MAPE")
# 
# # Print the results
# print(results_df)






# Get feature importance
importance_scores <- importance(model)
importance_scores





# Create an empty data frame to store the results
results <- data.frame(ntree = numeric(),
      MAE = numeric(),
      RMSE = numeric())







############## LAGGED 2 MODEL  ############
############## LAGGED 2 MODEL  ############
############## LAGGED 2 MODEL  ############

#### Approval ###
library(dplyr)

merged_df <- inner_join(daily, Approval, by = c("executive", "date"))

merged_df$netapproval = merged_df$Approval-merged_df$Disapproval

merged_df$start = NA


# Add a column with the date to rows matching a value of another column


merged_df <- merged_df %>%
  mutate(start = ifelse(executive == "ALBANESE", "2022-05-23",
         ifelse(executive == "NEHAMMER", "2021-12-06",
         ifelse(executive == "AMLO", "2018-12-01",
         ifelse(executive == "LULA", "2023-01-01",
         ifelse(executive == "BIDEN", "2021-01-20",
         ifelse(executive == "KISHIDA", "2021-10-04",
         ifelse(executive == "TRUDEAU", "2015-11-04",
         ifelse(executive == "FIALA", "2021-11-28",
         ifelse(executive == "MACRON", "2017-05-14",
         ifelse(executive == "SCHOLZ", "2021-12-08",
         ifelse(executive == "MODI", "2014-05-26",
         ifelse(executive == "VARADKAR", "2022-12-17",
         ifelse(executive == "MELONI", "2022-10-22",
         ifelse(executive == "RUTTE", "2010-10-14",
         ifelse(executive == "GAHRSTORE", "2021-10-14",
         ifelse(executive == "MORAWIECKI", "2017-12-11",
         ifelse(executive == "SEOKYOUL", "2022-05-10",
         ifelse(executive == "SANCHEZ", "2018-06-02",
         ifelse(executive == "BERSET", "2023-01-01",
         ifelse(executive == "SUNAK", "2022-10-25",
         ifelse(executive == "DECROO", "2020-10-01", NA))))))))))))))))))))))

merged_df$start =  as.Date(merged_df$start)

# Calculate the number of days between the two dates
merged_df$daysinoffice <- as.numeric(difftime(merged_df$date, merged_df$start, units = "days"))


# Calculate the lower and upper percentile thresholds
lower_thresholdRT <- quantile(merged_df$avgRTcount, 0.01)
upper_thresholdRT <- quantile(merged_df$avgRTcount, 0.99)

lower_thresholdReply <- quantile(merged_df$avgREPLYcount, 0.01)
upper_thresholdReply <- quantile(merged_df$avgREPLYcount, 0.99)

lower_thresholdQuote <- quantile(merged_df$avgQUOTEcount, 0.01)
upper_thresholdQuote <- quantile(merged_df$avgQUOTEcount, 0.99)

lower_thresholdLike <- quantile(merged_df$avgLIKEcount, 0.01)
upper_thresholdLike <- quantile(merged_df$avgLIKEcount, 0.99)




merged_df <- merged_df %>%
  group_by(executive) %>%
  mutate(Approvallag = lag(Approval),
         Disapprovallag = lag(Disapproval),
         Netapprovallag = lag(netapproval),
         RTlag = lag(avgRTcount),
         replylag = lag(avgREPLYcount),
         likelag = lag(avgLIKEcount),
         quotelag =lag(avgQUOTEcount),
         datelag = lag(date),
         number_of_tweetslag = lag(number_of_tweets))


merged_df <- merged_df %>%
  group_by(executive) %>%
  mutate(RTlag2 = lag(RTlag),
         replylag2 = lag(replylag),
         likelag2 = lag(likelag),
         quotelag2 =lag(quotelag),
         number_of_tweetslag2 = lag(number_of_tweetslag))


# merged_df_lag3 <- merged_df %>%
#   group_by(executive) %>%
#   mutate(RTlag3 = lag(RTlag2),
#          replylag3 = lag(replylag2),
#          likelag3 = lag(likelag2),
#          quotelag3 =lag(quotelag2))



merged_df <- merged_df[complete.cases(merged_df), ]



# Filter the data frame by removing values outside the threshold range for lagged 2 variables
filtered_df <- merged_df[merged_df$RTlag2 >= lower_thresholdRT & merged_df$RTlag2 <= upper_thresholdRT, ]
filtered_df = filtered_df[filtered_df$replylag2 >= lower_thresholdReply & filtered_df$replylag2 <= upper_thresholdReply, ]
filtered_df = filtered_df[filtered_df$quotelag2 >= lower_thresholdQuote & filtered_df$quotelag2 <= upper_thresholdQuote, ]
filtered_df = filtered_df[filtered_df$likelag2 >= lower_thresholdLike & filtered_df$likelag2 <= upper_thresholdLike, ]


merged_df = filtered_df 


####################### FOR OUTLIERS TEST ##################
# outliers <-  anti_join(merged_df, filtered_df)



library(randomForest)
library(caret)

# All covars
data_subset <- merged_df[, c("Approval", "RTlag2", "replylag2", "likelag2",
             "quotelag2", "executive", "Approvallag", "Disapprovallag",
             "Netapprovallag", "number_of_tweetslag2", "daysinoffice")]


# All covars -number of tweets
data_subset <- merged_df[, c("Approval", "RTlag2", "replylag2", "likelag2",
             "quotelag2", "executive", "Approvallag", "Disapprovallag",
             "Netapprovallag", "daysinoffice")]


####################### CONSIDER AN ANALYSIS OF THE OUTLIERS


## Pure Approval
data_subset <- merged_df[, c("Approval", "executive", "Approvallag", "Disapprovallag",
             "Netapprovallag", "daysinoffice")]

set.seed(123)

trainIndex <- createDataPartition(data_subset$Approval, p = 0.8, list = FALSE)
train_data <- data_subset[trainIndex, ]
test_data <- data_subset[-trainIndex, ]


model <- randomForest(Approval ~ ., data = train_data, ntree = 700, max_depth = 7 )



### Pure SM
data_subset <- merged_df[, c("Approval", "RTlag2", "replylag2", "likelag2",
                              "number_of_tweetslag2", "executive", "daysinoffice")]


set.seed(123)

trainIndex <- createDataPartition(data_subset$Approval, p = 0.8, list = FALSE)
train_data <- data_subset[trainIndex, ]
test_data <- data_subset[-trainIndex, ]


model <- randomForest(Approval ~ ., data = train_data, ntree = 600, max_depth = 7 )




# Best Model No exec
data_subset <- merged_df[, c("Approval", "RTlag2",
                             "Approvallag", "Disapprovallag",
                             "Netapprovallag", "daysinoffice", 
                             "number_of_tweetslag2")]

set.seed(123)

trainIndex <- createDataPartition(data_subset$Approval, p = 0.8, list = FALSE)
train_data <- data_subset[trainIndex, ]
test_data <- data_subset[-trainIndex, ]


model <- randomForest(Approval ~ ., data = train_data, ntree = 400, max_depth = 3 )




# Best Model
data_subset <- merged_df[, c("Approval", "RTlag2","Approvallag", "Disapprovallag",
                             "Netapprovallag", "executive", "daysinoffice", 
                             "number_of_tweetslag2")]




# Set seed for reproducibility
set.seed(123)

# Split data into 80% training and 20% test sets
trainIndex <- createDataPartition(data_subset$Approval, p = 0.8, list = FALSE)
train_data <- data_subset[trainIndex, ]
test_data <- data_subset[-trainIndex, ]


#3 Fit the Random Forest model
model <- randomForest(Approval ~ ., data = train_data, ntree = 400, max_depth =4 )




# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Calculate accuracy metrics
accuracy_metrics <- data.frame(
  MAE = mean(abs(predictions - test_data$Approval)),
  RMSE = sqrt(mean((predictions - test_data$Approval)^2)),
  MSE = mean((test_data$Approval - predictions)^2),
  MAPE = mean(abs((test_data$Approval - predictions) / test_data$Approval) * 100, na.rm = TRUE))

print(accuracy_metrics)


test_data$Lag2AppNoExec =  predictions

write.csv(test_data, "Lag2AppNoExec.csv")


# List of names 
names_list <- c("ALBANESE", "NEHAMMER", "DECROO", "LULA", "TRUDEAU", "FIALA",
                "MACRON", "SCHOLZ", "MODI", "VARADKAR", "MELONI", "KISHIDA",
                "AMLO", "RUTTE", "GAHRSTORE", "MORAWIECKI", "SEOKYOUL", "SANCHEZ", "BERSET",
                "SUNAK", "BIDEN")

Lag2AppBestModel = read.csv("Lag2AppBestModel.csv")




# Create a list of data frames for each executive
executive_dfs <- split(Lag2AppBestModel, Lag2AppBestModel$executive)

# Iterate over the names in the names_list and assign a data frame for each executive
for (name in names_list) {
  # Create a data frame for the current executive
  exec_df <- executive_dfs[[name]]
  
  # Perform any additional operations on the data frame if needed
  
  # Assign the data frame to a variable named after the executive
  assign(name, exec_df)
}



# Create an empty data frame to store the results
results <- data.frame(executive = character(),
                      MAE = numeric(),
                      MAPE = numeric(),
                      stringsAsFactors = FALSE)

# Loop through each executive in names_list
for (executive in names_list) {
  # Get the data frame for the current executive
  executive_data <- get(paste(executive, sep = ""))
  
  # Calculate the evaluation metrics
  MAE <- mean(abs(executive_data$Approval - executive_data$Lag2AppBestModel))
  MAPE <- mean(abs((executive_data$Approval - executive_data$Lag2AppBestModel) / executive_data$Approval)) * 100
  
  # Add the results to the results data frame
  results <- rbind(results, data.frame(executive = executive,
                                       MAE = MAE,
                                       MAPE = MAPE))
}

# Print the results
print(results)






# Get feature importance
importance_scores <- importance(model)
importance_scores



# # Load required packages
# library(caret)
# library(randomForest)
# 
# 
# # Define the hyperparameter grid for tuning
# rf_grid <- expand.grid(
#   max_depth = c(3, 4, 5, 6, 7),    # Maximum depth of the tree
#   ntree = c(200, 300, 400, 500, 600, 700, 900, 1000, 2000)  # Number of trees in the forest
# )
# 
# # Function to calculate MAE, RMSE, and MAPE
# calculate_metrics <- function(predictions, actual) {
#   mae <- mean(abs(predictions - actual))
#   mape <- mean(abs((predictions - actual) / actual)) * 100
#   return(list(MAE = mae, MAPE = mape))
# }
# 
# # Perform grid search and evaluate models
# results <- lapply(1:nrow(rf_grid), function(i) {
#   model <- randomForest(
#     Approval ~ .,                        # Formula for the model
#     data = train_data,                  # Training data
#     ntree = rf_grid$ntree[i],          # Number of trees
#     max_depth = rf_grid$max_depth[i]   # Maximum depth of the tree
#   )
#   
#   predictions <- predict(model, newdata = test_data)
#   actual <- test_data$Approval
#   
#   metrics <- calculate_metrics(predictions, actual)
#   metrics <- c(rf_grid$max_depth[i], rf_grid$ntree[i], metrics)
#   
#   return(metrics)
# })
# 
# 
# 
# 
# # Convert the results to a data frame
# results_df <- do.call(rbind, results)
# colnames(results_df) <- c("max_depth", "ntree", "MAE", "MAPE")
# 
# # Print the results
# print(results_df)
# 




################3
##########333 DISAPPROVAL lag 2
##########333


library(randomForest)
library(caret)

# All covars
data_subset <- merged_df[, c("Disapproval", "RTlag2", "replylag2", "likelag2",
             "quotelag2", "executive", "Approvallag", "Disapprovallag",
             "Netapprovallag", "number_of_tweetslag2", "daysinoffice")]


# All covars -number of tweets
data_subset <- merged_df[, c("Disapproval", "RTlag2", "replylag2", "likelag2",
             "quotelag2", "executive", "Approvallag", "Disapprovallag",
             "Netapprovallag", "daysinoffice")]


####################### CONSIDER AN ANALYSIS OF THE OUTLIERS

## BEST MODEL
data_subset <- merged_df[, c("Disapproval", "executive", "Approvallag", "Disapprovallag",
             "Netapprovallag", "daysinoffice", "number_of_tweetslag2", "RTlag2", "quotelag2")]


## Pure approval data -# tweets
data_subset <- merged_df[, c("Disapproval", "executive", "Approvallag", "Disapprovallag",
             "Netapprovallag", "daysinoffice")]



### Best SM model ###
data_subset <- merged_df[, c("Disapproval", "RTlag2", "replylag2", "likelag2",
             "quotelag2", "executive", "daysinoffice")]


# For model with approval and some covariates NO EXEC MAE =  .684 200 trees
data_subset <- merged_df[, c("Disapproval", "RTlag2", "replylag2", "likelag2",
             "quotelag", "Approvallag", "Disapprovallag",
             "Netapprovallag", "daysinoffice", "number_of_tweetslag2")]


####  Best Model


data_subset <- merged_df[, c("Disapproval", "executive", "RTlag2", "replylag2", "likelag2", "quotelag", 
                             "Approvallag", "Disapprovallag", "Netapprovallag", "daysinoffice")]

# Set seed for reproducibility
set.seed(123)

# Split data into 80% training and 20% test sets
trainIndex <- createDataPartition(data_subset$Disapproval, p = 0.8, list = FALSE)
train_data <- data_subset[trainIndex, ]
test_data <- data_subset[-trainIndex, ]


#3 Fit the Random Forest model
model <- randomForest(Disapproval ~ ., data = train_data, ntree = 2000, max_depth = 6)





# Make predictions on the test data
predictions <- predict(model, newdata = test_data)



# Calculate accuracy metrics
accuracy_metrics <- data.frame(
  MAE = mean(abs(predictions - test_data$Disapproval)),
  RMSE = sqrt(mean((predictions - test_data$Disapproval)^2)),
  MSE = mean((test_data$Disapproval - predictions)^2),
  MAPE = mean(abs((test_data$Disapproval - predictions) / test_data$Disapproval) * 100, na.rm = TRUE))


print(accuracy_metrics)




test_data$Lag2DisNoExec = predictions
 

  write.csv(test_data, "Lag2DisNoExec.csv")
  
  

names_list <- c("ALBANESE", "NEHAMMER", "DECROO", "LULA", "TRUDEAU", "FIALA",
                  "MACRON", "SCHOLZ", "MODI", "VARADKAR", "MELONI", "KISHIDA",
                  "AMLO", "RUTTE", "GAHRSTORE", "MORAWIECKI", "SEOKYOUL", "SANCHEZ", "BERSET",
                  "SUNAK", "BIDEN")
  
Lag2DisBestModel = read.csv("Lag2DisBestModel.csv")
  

  
# Create a list of data frames for each executive
executive_dfs <- split(Lag2DisBestModel, Lag2DisBestModel$executive)
  
  # Iterate over the names in the names_list and assign a data frame for each executive
  for (name in names_list) {
    # Create a data frame for the current executive
    exec_df <- executive_dfs[[name]]
    
    # Perform any additional operations on the data frame if needed
    
    # Assign the data frame to a variable named after the executive
    assign(name, exec_df)
  }
  
  
  
  # Create an empty data frame to store the results
  results <- data.frame(executive = character(),
                        MAE = numeric(),
                        MAPE = numeric(),
                        stringsAsFactors = FALSE)
  
  # Loop through each executive in names_list
  for (executive in names_list) {
    # Get the data frame for the current executive
    executive_data <- get(paste(executive, sep = ""))
    
    # Calculate the evaluation metrics
    MAE <- mean(abs(executive_data$Disapproval - executive_data$Lag2DisBestModel))
    MAPE <- mean(abs((executive_data$Disapproval - executive_data$Lag2DisBestModel) / executive_data$Disapproval)) * 100
    
    
    # Add the results to the results data frame
    results <- rbind(results, data.frame(executive = executive,
                                         MAE = MAE,
                                         MAPE = MAPE
                                         ))
  }
  
  # Print the results
  print(results)
  
  
  

  
# Load required packages
# library(caret)
# library(randomForest)
# 
# 
# # Define the hyperparameter grid for tuning
# rf_grid <- expand.grid(
#   max_depth = c(2, 3, 4, 5, 6, 7),    # Maximum depth of the tree
#   ntree = c(100, 200, 300, 400, 500, 600, 700, 1000, 2000)  # Number of trees in the forest
# )
# 
# # Function to calculate MAE, RMSE, and MAPE
# calculate_metrics <- function(predictions, actual) {
#   mae <- mean(abs(predictions - actual))
#   mape <- mean(abs((predictions - actual) / actual)) * 100
#   return(list(MAE = mae, MAPE = mape))
# }
# 
# # Perform grid search and evaluate models
# results <- lapply(1:nrow(rf_grid), function(i) {
#   model <- randomForest(
#     Disapproval ~ .,                        # Formula for the model
#     data = train_data,                  # Training data
#     ntree = rf_grid$ntree[i],          # Number of trees
#     max_depth = rf_grid$max_depth[i]   # Maximum depth of the tree
#   )
#   
#   predictions <- predict(model, newdata = test_data)
#   actual <- test_data$Disapproval
#   
#   metrics <- calculate_metrics(predictions, actual)
#   metrics <- c(rf_grid$max_depth[i], rf_grid$ntree[i], metrics)
#   
#   return(metrics)
# })
# 
# 
# 
# 
# # Convert the results to a data frame
# results_df <- do.call(rbind, results)
# colnames(results_df) <- c("max_depth", "ntree", "MAE", "MAPE")
# 
# # Print the results
# print(results_df)
# 




BIDENBestNoExec = filter(BestNOexecDisLag2, X >= 1777)




library(dplyr)

library(stargazer)


# Create the LaTeX table using stargazer
table_latex <- stargazer(metricsDisapprovalLag2, title = "Performance Metrics for Disapproval Model based on Pure SM", align = TRUE, type = "latex")





############ GRAPHS #############
############ GRAPHS #############
############ GRAPHS #############

merged_df <- inner_join(daily, Approval, by = c("executive", "date"))

merged_df$netapproval = merged_df$Approval-merged_df$Disapproval

merged_df <- merged_df %>%
  mutate(start = ifelse(executive == "ALBANESE", "2022-05-23",
  ifelse(executive == "NEHAMMER", "2021-12-06",
  ifelse(executive == "AMLO", "2018-12-01",
  ifelse(executive == "LULA", "2023-01-01",
  ifelse(executive == "BIDEN", "2021-01-20",
  ifelse(executive == "KISHIDA", "2021-10-04",
  ifelse(executive == "TRUDEAU", "2015-11-04",
  ifelse(executive == "FIALA", "2021-11-28",
ifelse(executive == "MACRON", "2017-05-14",
          ifelse(executive == "SCHOLZ", "2021-12-08",
         ifelse(executive == "MODI", "2014-05-26",
                ifelse(executive == "VARADKAR", "2022-12-17",
               ifelse(executive == "MELONI", "2022-10-22",
              ifelse(executive == "RUTTE", "2010-10-14",
                     ifelse(executive == "GAHRSTORE", "2021-10-14",
                    ifelse(executive == "MORAWIECKI", "2017-12-11",
     ifelse(executive == "SEOKYOUL", "2022-05-10",
    ifelse(executive == "SANCHEZ", "2018-06-02",
           ifelse(executive == "BERSET", "2023-01-01",
          ifelse(executive == "SUNAK", "2022-10-25",
         ifelse(executive == "DECROO", "2020-10-01", NA))))))))))))))))))))))

merged_df$start =  as.Date(merged_df$start)

# Calculate the number of days between the two dates
merged_df$daysinoffice <- as.numeric(difftime(merged_df$date, merged_df$start, units = "days"))


MACRONseries = filter(merged_df, executive == "MACRON")

setwd("C:/Users/Juan/OneDrive - Georgia State University/APPROVAL VS MACHINE LEARNING")

 

Lag2AppOnlySM = read.csv("Lag2AppOnlySM.csv")
Lag2AppOnlyApp = read.csv("Lag2AppOnlyApp.csv") 
Lag2AppBestModel = read.csv("Lag2AppBestModel.csv") 
Lag2AppNoExec = read.csv("Lag2AppNoExec.csv") 

Lag2DisOnlySocialMedia = read.csv("Lag2DisOnlySocialMedia.csv")
Lag2DisPureaApp = read.csv("Lag2DisPureaApp.csv") 
Lag2DisBestModel = read.csv("Lag2DisBestModel.csv") 
Lag2DisNoExec = read.csv("Lag2DisNoExec.csv")


# TOP 6
# Biden         
# Albanese   
# Sanchez    	       
# Sunak         
# Macron       
# Modi


# LOWEST 5
# Nehammer   
# Varadkar   
# Seokyoul   
# Gahrstore  
# Berset     

# Approval Predictions
Lag2AppOnlySM = Lag2AppOnlySM[Lag2AppOnlySM$X >= 821 & Lag2AppOnlySM$X <= 1086, ] # >= 1777 is for Biden / Seoyoul 1507-1515
Lag2AppOnlyApp = Lag2AppOnlyApp[Lag2AppOnlyApp$X >= 821 & Lag2AppOnlyApp$X <= 1086, ] # <= 69 in for Albanese / For GAHRSTORE 1424-1454
Lag2AppBestModel = Lag2AppBestModel[Lag2AppBestModel$X >= 821 & Lag2AppBestModel$X <= 1086, ] # <= 161-184 for LULAv/ For VARADKAR 986-1008
Lag2AppNoExec = Lag2AppNoExec[Lag2AppNoExec$X >= 821 & Lag2AppNoExec$X <= 1086, ] # for Macron 553-724 / For SUNAK 1749-1774
# For Decroo 115-160 / NEHAMMER App 70-114

# Disapproval Predictions
Lag2DisOnlySocialMedia = Lag2DisOnlySocialMedia[Lag2DisOnlySocialMedia$X >= 807 & Lag2DisOnlySocialMedia$X <= 1047, ] # >= 1775 is for Biden / For Seoyokul 1503-1511
Lag2DisPureaApp = Lag2DisPureaApp[Lag2DisPureaApp$X >= 807 & Lag2DisPureaApp$X <= 1047, ] # <= 74 for ALABANESE / For GAHRSTORE 1432-1457
Lag2DisBestModel = Lag2DisBestModel[Lag2DisBestModel$X >= 807 & Lag2DisBestModel$X <= 1047, ] # <= 173-199 for LULA / For VARADKAR 991-1004
Lag2DisNoExec = Lag2DisNoExec[Lag2DisNoExec$X >= 807 & Lag2DisNoExec$X <= 1047, ] # for Macron 549-725 For SUNAK 1739-1776
# For Decroo 128-172 / / NEHAMMER 75-127



Lag2AppOnlySM = Lag2AppOnlySM[, c("daysinoffice", "Lag2AppOnlySM")]
Lag2AppOnlyApp = Lag2AppOnlyApp[, c("daysinoffice","Lag2AppOnlyApp")]
Lag2AppBestModel = Lag2AppBestModel[, c("daysinoffice","Lag2AppBestModel")]
Lag2AppNoExec = Lag2AppNoExec[, c("daysinoffice","Lag2AppNoExec")]

Lag2DisOnlySocialMedia = Lag2DisOnlySocialMedia[, c("daysinoffice","Lag2DisOnlySocialMedia")]
Lag2DisPureaApp = Lag2DisPureaApp[, c("daysinoffice","Lag2DisPureaApp")]
Lag2DisBestModel = Lag2DisBestModel[, c("daysinoffice","Lag2DisBestModel")] 
Lag2DisNoExec = Lag2DisNoExec[, c("daysinoffice","Lag2DisNoExec")]



library(dplyr)
# Loop for Approval Predictions
data_frames <- c("Lag2AppOnlySM", "Lag2AppOnlyApp", "Lag2AppBestModel", "Lag2AppNoExec", 
                 "Lag2DisOnlySocialMedia", "Lag2DisPureaApp", "Lag2DisBestModel", "Lag2DisNoExec")


for (df_name in data_frames) {
  MODIseries <- left_join(MODIseries, get(df_name), by = "daysinoffice")
}

library(ggplot2)
library(scales)



ggplot(MODIseries, aes(x = date)) +
  geom_line(aes(y = Approval), color = "darkblue") +
  geom_ribbon(aes(ymin = Approval - 4, ymax = Approval + 4), fill = "lightblue", alpha = 0.3) +
  geom_point(aes(y = Lag2AppBestModel, shape = "Best Model"), color = "black", na.rm = TRUE) +
  geom_point(aes(y = Lag2AppOnlyApp, shape = "Approval Data Only"), color = "black", na.rm = TRUE) +
  geom_point(aes(y = Lag2AppOnlySM, shape = "Social Media Data Only"), color = "black", na.rm = TRUE) +
  geom_point(aes(y = Lag2AppNoExec, shape = "No Executive Name"), color = "black", na.rm = TRUE) +
  geom_line(aes(y = Disapproval), color = "darkred") +
  geom_ribbon(aes(ymin = Disapproval - 4, ymax = Disapproval + 4), fill = "pink", alpha = 0.3) +
  geom_point(aes(y = Lag2DisBestModel, shape = "Best Model"), color = "black", na.rm = TRUE) +
  geom_point(aes(y = Lag2DisPureaApp, shape = "Approval Data Only"), color = "black", na.rm = TRUE) +
  geom_point(aes(y = Lag2DisOnlySocialMedia, shape = "Social Media Data Only"), color = "black", na.rm = TRUE) +
  geom_point(aes(y = Lag2DisNoExec, shape = "No Executive Name"), color = "black", na.rm = TRUE) +
  labs(x = "Date", y = "Approval/Disapproval", shape = "Model") +
  theme_minimal() +
  scale_x_date(labels = date_format("%Y-%m"), breaks = date_breaks("6 months")) +
  scale_shape_manual(values = c("Best Model" = 18, "Approval Data Only" = 8, "Social Media Data Only" = 1, "No Executive Name" = 3)) +
  guides(shape = guide_legend(title = "Model")) +
  scale_y_continuous(breaks = seq(10, 70, length.out = 5))








write.csv(predictions, "predictions.csv")
write.csv(merged_df, "totaldf.csv")
write.csv(test_data, "test_data.csv")

# Get feature importance
importance_scores <- importance(model)
importance_scores

library(ggplot2)
library(randomForest)




# Generate example data
set.seed(123)
n <- 100
x <- seq(1, 10, length.out = n)
y_actual <- 2 * x + rnorm(n, mean = 0, sd = 1)

+# Combine the predictor and target variables into a data frame
data <- data.frame(x = x, y = y_actual)

# Fit a random forest regression model
rf_model <- randomForest(y ~ x, data = data)

# Predict on the same range of x-values
y_predicted <- predict(rf_model, newdata = data)

# Combine the actual and predicted values into a data frame
data <- cbind(data, y_predicted)

# Create the scatter plot with the line of best fit
ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "blue") +
  geom_line(aes(y = y_predicted), color = "red") +
  labs(x = "x", y = "y") +
  ggtitle("Random Forest Regression") +
  theme_minimal()








############## LAGGED 3 MODEL  ############
############## LAGGED 3 MODEL  ############
############## LAGGED 3 MODEL  ############

#### Approval ###

merged_df <- inner_join(daily, Approval, by = c("executive", "date"))

merged_df$netapproval = merged_df$Approval-merged_df$Disapproval


# Calculate the lower and upper percentile thresholds
lower_thresholdRT <- quantile(merged_df$avgRTcount, 0.01)
upper_thresholdRT <- quantile(merged_df$avgRTcount, 0.99)

lower_thresholdReply <- quantile(merged_df$avgREPLYcount, 0.01)
upper_thresholdReply <- quantile(merged_df$avgREPLYcount, 0.99)

lower_thresholdQuote <- quantile(merged_df$avgQUOTEcount, 0.01)
upper_thresholdQuote <- quantile(merged_df$avgQUOTEcount, 0.99)

lower_thresholdLike <- quantile(merged_df$avgLIKEcount, 0.01)
upper_thresholdLike <- quantile(merged_df$avgLIKEcount, 0.99)




merged_df <- merged_df %>%
  group_by(executive) %>%
  mutate(Approvallag = lag(Approval),
         Disapprovallag = lag(Disapproval),
         Netapprovallag = lag(netapproval),
         RTlag = lag(avgRTcount),
         replylag = lag(avgREPLYcount),
         likelag = lag(avgLIKEcount),
         quotelag =lag(avgQUOTEcount),
         datelag = lag(date),
         number_of_tweetslag = lag(number_of_tweets))


merged_df <- merged_df %>%
  group_by(executive) %>%
  mutate(RTlag2 = lag(RTlag),
         replylag2 = lag(replylag),
         likelag2 = lag(likelag),
         quotelag2 =lag(quotelag),
         number_of_tweetslag2 = lag(number_of_tweetslag))



merged_df <- merged_df %>%
  group_by(executive) %>%
  mutate(RTlag3 = lag(RTlag2),
         replylag3 = lag(replylag2),
         likelag3 = lag(likelag2),
         quotelag3 =lag(quotelag2),
         number_of_tweetslag3 = lag(number_of_tweetslag))



merged_df <- merged_df[complete.cases(merged_df), ]




# Filter the data frame by removing values outside the threshold range
filtered_df <- merged_df[merged_df$avgRTcount >= lower_thresholdRT & merged_df$avgRTcount <= upper_thresholdRT, ]
filtered_df = filtered_df[filtered_df$avgREPLYcount >= lower_thresholdReply & filtered_df$avgREPLYcount <= upper_thresholdReply, ]
filtered_df = filtered_df[filtered_df$avgQUOTEcount >= lower_thresholdQuote & filtered_df$avgQUOTEcount <= upper_thresholdQuote, ]
filtered_df = filtered_df[filtered_df$avgLIKEcount >= lower_thresholdLike & filtered_df$avgLIKEcount <= upper_thresholdLike, ]



# Filter the data frame by removing values outside the threshold range for lagged variables
filtered_df <- filtered_df[filtered_df$RTlag >= lower_thresholdRT & filtered_df$RTlag <= upper_thresholdRT, ]
filtered_df = filtered_df[filtered_df$replylag >= lower_thresholdReply & filtered_df$replylag <= upper_thresholdReply, ]
filtered_df = filtered_df[filtered_df$quotelag >= lower_thresholdQuote & filtered_df$quotelag <= upper_thresholdQuote, ]
filtered_df = filtered_df[filtered_df$likelag >= lower_thresholdLike & filtered_df$likelag <= upper_thresholdLike, ]


# Filter the data frame by removing values outside the threshold range for lagged 2 variables
filtered_df <- filtered_df[filtered_df$RTlag2 >= lower_thresholdRT & filtered_df$RTlag2 <= upper_thresholdRT, ]
filtered_df = filtered_df[filtered_df$replylag2 >= lower_thresholdReply & filtered_df$replylag2 <= upper_thresholdReply, ]
filtered_df = filtered_df[filtered_df$quotelag2 >= lower_thresholdQuote & filtered_df$quotelag2 <= upper_thresholdQuote, ]
filtered_df = filtered_df[filtered_df$likelag2 >= lower_thresholdLike & filtered_df$likelag2 <= upper_thresholdLike, ]


# Filter the data frame by removing values outside the threshold range for lagged 2 variables
filtered_df <- filtered_df[filtered_df$RTlag3 >= lower_thresholdRT & filtered_df$RTlag3 <= upper_thresholdRT, ]
filtered_df = filtered_df[filtered_df$replylag3 >= lower_thresholdReply & filtered_df$replylag3 <= upper_thresholdReply, ]
filtered_df = filtered_df[filtered_df$quotelag3 >= lower_thresholdQuote & filtered_df$quotelag3 <= upper_thresholdQuote, ]
filtered_df = filtered_df[filtered_df$likelag3 >= lower_thresholdLike & filtered_df$likelag3 <= upper_thresholdLike, ]



merged_df = filtered_df

library(randomForest)
library(caret)

# For model with approval and all covariates MAE = 0.677 200 trees
data_subset <- merged_df[, c("Approval", "RTlag3", "replylag3", "likelag3",
             "quotelag3", "executive", "Approvallag", "Disapprovallag",
             "Netapprovallag", "number_of_tweetslag3", "datelag")]



# For model with approval and some covariates MAE =    200 trees
data_subset <- merged_df[, c("Approval", "RTlag2", "replylag2", "likelag2",
             "quotelag2", "executive", "Approvallag", "Disapprovallag",
             "Netapprovallag", "datelag")]


######################### I CANNOT USE DATE TO TRAIN THE MODEL
############### CONSIDER CHANGE IT FOR DAYS IN OFFICE
####################### CONSIDER AN ANALYSIS OF THE OUTLIERS

# Purely previous data on approval MAE =  .685 200 trees
data_subset <- merged_df[, c("Approval", "executive", "Approvallag", "Disapprovallag",
             "Netapprovallag", "datelag")]


# Purely SM data MAE =  1.85 200 trees ##### Prove a pure SM with 2 and 3 lags...
data_subset <- merged_df[, c("Approval", "RTlag3", "replylag3", "likelag3",
             "quotelag3", "executive", "datelag")]


# For model with approval and some covariates NO EXEC MAE =  .684 200 trees
data_subset <- merged_df[, c("Approval", "RTlag", "replylag", "likelag",
             "quotelag", "Approvallag", "Disapprovallag",
             "Netapprovallag", "datelag")]



# Set seed for reproducibility
set.seed(123)

# Split data into 70% training and 30% test sets
trainIndex <- createDataPartition(data_subset$Approval, p = 0.8, list = FALSE)
train_data <- data_subset[trainIndex, ]
test_data <- data_subset[-trainIndex, ]

# Fit the Random Forest model
model <- randomForest(Approval ~ ., data = train_data, ntree = 300)

# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Print the predicted approval levels
#print(predictions)

#write.csv(predictions, "approvalpred2.csv")

#write.csv(test_data, "approvaltest.csv")

# Calculate accuracy metrics
accuracy_metrics <- data.frame(
  MAE = mean(abs(predictions - test_data$Approval)),
  RMSE = sqrt(mean((predictions - test_data$Approval)^2)))

print(accuracy_metrics)




actual = test_data$Approval

# Function to calculate sMAPE
smape <- function(actual, predictions) {
  numerator <- abs(predictions - actual)
  denominator <- (abs(actual) + abs(predictions)) / 2
  smape_values <- 100 * numerator / denominator
  mean(smape_values, na.rm = TRUE)
}

smape_value <- smape(actual, predictions)
print(smape_value)  


# Get feature importance
importance_scores <- importance(model)
importance_scores








########### CLASSIFICATION WITH NO OUTLIERS #############
########### CLASSIFICATION WITH NO OUTLIERS #############
########### CLASSIFICATION WITH NO OUTLIERS #############

install.packages("ROSE")
library(ROSE)

merged_df <- merged_df %>%
  mutate(approvalTrend = case_when(
    approvalDIFF > 0 ~ "Positive",
    approvalDIFF < 0 ~ "Negative",
    TRUE ~ "Equal"
  ))

summary(merged_df$approvalTrend == "Equal")
summary(merged_df$approvalTrend == "Positive")
summary(merged_df$approvalTrend == "Negative")


merged_df <- merged_df %>%
  mutate(disapprovalTrend = case_when(
    disapprovalDIFF > 0 ~ "Positive",
    disapprovalDIFF < 0 ~ "Negative",
    TRUE ~ "Equal"
  ))

summary(merged_df$disapprovalTrend == "Equal")
summary(merged_df$disapprovalTrend == "Positive")
summary(merged_df$disapprovalTrend == "Negative")



# Check the class distribution before oversampling
table(merged_df$disapprovalTrend)


# Convert the target variable to a factor in the merged_df
merged_df$approvalTrend <- factor(merged_df$approvalTrend)
merged_df$disapprovalTrend <- factor(merged_df$disapprovalTrend)

# Subset the data for the variables of interest (Approval, avgRTcount, avgLIKEcount)
data <- merged_df[, c("approvalTrend", "RTlag", "replylag", "likelag",
                      "quotelag", "executive")]

# Subset the data for the variables of interest (Approval, avgRTcount, avgLIKEcount)
data <- merged_df[, c("disapprovalTrend", "RTlag", "replylag", "likelag",
                      "quotelag", "executive")]

# Oversampling Approval Trend
Positive = subset(data, approvalTrend == "Positive")
Negative = subset(data, approvalTrend == "Negative")
Equal = subset(data, approvalTrend == "Equal")

# Oversampling Disapproval Trend
Positive = subset(data, disapprovalTrend == "Positive")
Negative = subset(data, disapprovalTrend == "Negative")
Equal = subset(data, disapprovalTrend == "Equal")


# Randomly sample 80% of the data frame
PositiveTrain <- sample_frac(Positive, 0.9, replace = FALSE)
PositiveTest <- anti_join(Positive, PositiveTrain)

NegativeTrain <- sample_frac(Negative, 0.9, replace = FALSE)
NegativeTest <- anti_join(Negative, NegativeTrain)

EqualTrain <- sample_frac(Equal, 0.7, replace = FALSE)
EqualTest <- anti_join(Equal, EqualTrain)


# Set seed for reproducibility
set.seed(123)

# Split data into 70% training and 30% test sets
train_data <- rbind(EqualTrain, PositiveTrain, NegativeTrain)
test_data <-   rbind(EqualTest, PositiveTest, NegativeTest)

table(train_data$disapprovalTrend)
table(merged_df$approvalTrend)



# Train the random forest classifier
model <- randomForest(disapprovalTrend ~ ., data = train_data, class.factors = levels(disapprovalTrend), ntree = 500) #500 best so far with disapproval...63.2 accuracy


# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Print the predicted approval levels
#print(predictions)


# Evaluate the accuracy of the model
accuracy <- sum(predictions == test_data$disapprovalTrend) / length(test_data$disapprovalTrend)
cat("Accuracy:", accuracy, "\n")


# Create the confusion matrix
confusion_matrix <- table(Actual = test_data$disapprovalTrend, Predicted = predictions)




######### Models for Netapproval #########


merged_df <- merged_df %>%
  mutate(netapprovalTrend = case_when(
    netapprovalDIFF > 1 ~ "Positive",
    netapprovalDIFF < -1 ~ "Negative",
    TRUE ~ "Equal"
  ))

merged_df <- merged_df %>%
  mutate(netapprovalTrend = case_when(
    netapprovalDIFF > 1 ~ "Positive",
    netapprovalDIFF < -1 ~ "Negative",
    TRUE ~ "Equal"
  ))


# Check the class distribution before oversampling
table(merged_df$netapprovalTrend)


# Convert the target variable to a factor in the merged_df
merged_df$netapprovalTrend <- factor(merged_df$netapprovalTrend)

# Subset the data for the variables of interest 
data <- merged_df[, c("netapprovalTrend", "RTlag", "replylag", "likelag",
                      "quotelag", "executive", "number_of_tweets")]

# Subset the data for the variables of interest  38%
data <- merged_df[, c("netapprovalTrend", "RTlag", "likelag",
                      "executive")]

# Subset the data for the variables of interest  36%
data <- merged_df[, c("netapprovalTrend", "RTlag",
                      "executive")]

# Subset the data for the variables of interest  36
data <- merged_df[, c("netapprovalTrend", "likelag",
                      "executive")]

# Subset the data for the variables of interest  37
data <- merged_df[, c("netapprovalTrend", "RTlag", "quotelag",
                      "executive")]

# Subset the data for the variables of interest  34
data <- merged_df[, c("netapprovalTrend", "RTlag", "quotelag")]

# Subset the data for the variables of interest  38
data <- merged_df[, c("netapprovalTrend", "RTlag", "quotelag", "number_of_tweets", "executive")]


# Oversampling Approval Trend
#Positive = subset(data, netapprovalTrend == "Positive")
#Negative = subset(data, netapprovalTrend == "Negative")
#Equal = subset(data, netapprovalTrend == "Equal")


# Randomly sample 80% of the data frame
PositiveTrain <- sample_frac(Positive, 0.9, replace = FALSE)
PositiveTest <- anti_join(Positive, PositiveTrain)

NegativeTrain <- sample_frac(Negative, 0.9, replace = FALSE)
NegativeTest <- anti_join(Negative, NegativeTrain)

EqualTrain <- sample_frac(Equal, 0.5, replace = FALSE)
EqualTest <- anti_join(Equal, EqualTrain)


# Set seed for reproducibility
set.seed(123)

# Split data into 70% training and 30% test sets
train_data <- rbind(EqualTrain, PositiveTrain, NegativeTrain)
test_data <-   rbind(EqualTest, PositiveTest, NegativeTest)

table(train_data$netapprovalTrend)

# Train the random forest classifier
model <- randomForest(netapprovalTrend ~ ., data = train_data, class.factors = levels(netapprovalTrend, ntree = 500)) #500 best so far with disapproval...63.2 accuracy


# Make predictions on the test data
predictions <- predict(model, newdata = test_data)

# Print the predicted approval levels
#print(predictions)


# Evaluate the accuracy of the model
accuracy <- sum(predictions == test_data$netapprovalTrend) / length(test_data$netapprovalTrend)
cat("Accuracy:", accuracy, "\n")


# Create the confusion matrix
confusion_matrix <- table(Actual = test_data$netapprovalTrend, Predicted = predictions)

