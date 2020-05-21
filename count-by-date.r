# Count all people who answered the eligibility survey by day
# ===========================================================
recruitment_date <- as.data.frame(table(recruitment_hear$date_of_survey))
names(recruitment_date) <- c("date", "freq")

# Count people according to their answers on how they heard about the study 
for(category in unique(recruitment_hear$hear_clean_NCat)){
  
  # Change behavior depending if the category is a missing value or a real entry
  if(is.na(category)) {
    table_temp <- as.data.frame(table(recruitment_hear[is.na(recruitment_hear$hear_clean_NCat), "date_of_survey"]))
    
  } else {
    table_temp <- as.data.frame(table(recruitment_hear[recruitment_hear$hear_clean_NCat %in% category, "date_of_survey"]))
  }
  
  # Add missing values to the table to get the dates that no one is recruited 
  table_NAs <- data.frame(Var1 = setdiff(recruitment_date$date, table_temp$Var1), Freq = 0)
  
  # Append to get a complete set of times
  table_temp <- rbind(table_temp, table_NAs)
  
  # Reorder
  table_temp <- table_temp[order(as.character(table_temp$Var1)),]
  
  # Append to main table
  
  if(is.na(category)){
    recruitment_date[, "Unreported"] <- table_temp$Freq
  } else {
    recruitment_date[, category] <- table_temp$Freq
  }
}

# Evaluate if sums within categories fit with total frequency
# recruitment_date$sum <- apply(recruitment_date[,3:9], 1, sum)
# recruitment_date <- recruitment_date[, c("date", "Letter", "Partners_Newsletter",  "Social_Media",  "Media",  
#                                          "Snowball" , "Other", "Unreported", "sum", "freq")]

# DIAGNOSIS!!!!
# Check if they are all 0s
# table(recruitment_date$freq - recruitment_date$sum)

# mean age of all participants who answered the eligibility survey 
T_age_mean <- aggregate(recruitment_hear$Age, by = list(recruitment_hear$date_of_survey),
                        FUN = mean)
names(T_age_mean) <- c("date", "All_mean_age")

# Calculate mean age per date 
# ---------------------------
# I calcuated the Age variable earlier in STATA from the date of eligibility survey and birth date:
age_mean <- aggregate(recruitment_hear$Age, by = list(recruitment_hear$date_of_survey, recruitment_hear$Completed_HSurvey),
                      FUN = mean)

# reshape age variable 
age_mean <- spread(age_mean, key = "Group.2", value = "x")
names(age_mean) <- c("date", "drop_mean_age", "participate_mean_age")

# Merge age variables to counts per day 
recruitment_date <- merge(recruitment_date, age_mean, by = "date")
recruitment_date <- merge(recruitment_date, T_age_mean, by = "date")

# Rename how partipcants heard about the study- Put Hear as a suffix before the Social Media, Letter, etc. to differniate it from the other data set.
variable_names <- c("Letter", "Partners_Newsletter",  "Social_Media", "Media",  "Snowball" , "Other")

names(recruitment_date)[match(variable_names, names(recruitment_date))] <- 
  paste("Heard", variable_names, sep = "_")


# Count only people who answered the health survey
# ================================================

# Subset participants that only answered the health survey... these are 1158 participants from 1536 
recruitment_hear_Par <- recruitment_hear[recruitment_hear$Completed_HSurvey %in% 1,  ]

# Count people who answered the health survey by day
recruitment_date_Par <- as.data.frame(table(recruitment_hear_Par$date_of_survey))
names(recruitment_date_Par) <- c("date", "freq")

# Count people according to their answers on how they heard about the study
for(category in unique(recruitment_hear_Par$hear_clean_NCat)){
  
  # Change behavior depending if the category is a missing value or a real entry
  if(is.na(category)) {
    table_temp <- as.data.frame(table(recruitment_hear_Par[is.na(recruitment_hear_Par$hear_clean_NCat), "date_of_survey"]))
    
  } else {
    table_temp <- as.data.frame(table(recruitment_hear_Par[recruitment_hear_Par$hear_clean_NCat %in% category, "date_of_survey"]))
  }
  
  # Add missing values to the table to get the dates that no one is recruited 
  table_NAs <- data.frame(Var1 = setdiff(recruitment_date_Par$date, table_temp$Var1), Freq = 0)
  
  # Append to get a complete set of times
  table_temp <- rbind(table_temp, table_NAs)
  
  # Reorder
  table_temp <- table_temp[order(as.character(table_temp$Var1)),]
  
  # Append to main table
  
  if(is.na(category)){
    recruitment_date_Par[, "Unreported"] <- table_temp$Freq
  } else {
    recruitment_date_Par[, category] <- table_temp$Freq
  }
}

# DIAGNOSIS!!!!
# Evaluate if sums within categories fit with total frequency
# Check if they are all 0s

# recruitment_date_Par$sum <- apply(recruitment_date_Par[,3:9], 1, sum)
# recruitment_date_Par <- recruitment_date_Par[, c("date", "Letter", "Partners_Newsletter",  "Social_Media",  "Media",  "Snowball" , "Other", "Unreported", "sum", "freq")]
# recruitment_date_Par$difference <- recruitment_date_Par$freq - recruitment_date_Par$sum
# table(recruitment_date_Par$difference)

# Calculate mean age per date 
# ------------------------------------
### I calculated the Age variable earlier in STATA from the date of eligibility survey and birth date:
age_mean <- aggregate(recruitment_hear_Par$Age, by = list(recruitment_hear_Par$date_of_survey),
                      FUN = mean)
names(age_mean) <- c("date", "participate_mean_age")


# Merge age variables to counts per day 
recruitment_date_Par <- merge(recruitment_date_Par, age_mean, by = "date")

# Rename how partipcants heard about the study- Put Hear as a suffix before the Social Media, Letter, etc. to differniate it from the other data set.
variable_names <- c("freq", "Letter", "Partners_Newsletter",  "Social_Media", "Media",  "Snowball" , "Other")

names(recruitment_date_Par)[match(variable_names, names(recruitment_date_Par))] <- 
  paste("Heard", variable_names, "P", sep = "_")

# Create a data frame with these new dates
# -----------------------------------------

## Find date where participant dropped
date_dropped <- setdiff(recruitment_date$date, recruitment_date_Par$date)

## Create empty data frame to store these dates
Diff_Part_dropped <- as.data.frame(matrix(nrow = length(date_dropped), ncol = ncol(recruitment_date_Par)))

## Change colnames according to recruitment_date_Par data
colnames(Diff_Part_dropped) <- colnames(recruitment_date_Par)

## Add dropped dates
Diff_Part_dropped$date <- date_dropped

## Bind both tables
recruitment_date_HSPar <- rbind(recruitment_date_Par, Diff_Part_dropped)

# Merging the recruitment counts (everyone and health survey complete)
# ==============================
total_rec_counts_date <- merge(recruitment_date, recruitment_date_HSPar[, !colnames(recruitment_date_HSPar) %in% "participate_mean_age"], 
                               by.x = "date", by.y = "date", all= TRUE, sort = TRUE)

