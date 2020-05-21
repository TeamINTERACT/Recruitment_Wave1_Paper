# ==========================================
# Data: Preparation for National recruitment paper data analysis
# Name: Rania Wasfi
# Note: Master file for data preparation and analysis
# ==========================================

setwd("D:/INTERACT/Recruitment Analysis")

## Call libraries
library(tidyverse)
library(dplyr)
library (tidyr)
library(plyr)
library(data.table)
library (ggplot2)
library(haven)
library(nlme)
library(lme4)
library(pscl)
library (boot)
library (MASS)
library (foreign)
library (Hmisc)
library(dynlm)
library(remotes)
library(lubridate)


# choose.files()

# load data  


recruitment_clean <- read_dta("D:/INTERACT/Recruitment Analysis/data/All_Cities_Health_Eligibility_Clean_Rec.dta")
recruitment_method <- read.csv2("D:/INTERACT/Recruitment Analysis/data/Recruitment_Tracking_Mtl_clean_adjusted_dates.csv", stringsAsFactors=FALSE)


Letter_Sampling_groups <- read.csv("D:/INTERACT/Recruitment Analysis/data/Codes Postaux INTERACT Montreal 2018-05-18.csv")



Facebook_Reach <- read.csv2("D:\\INTERACT\\Recruitment Analysis\\data\\FB_reach_byday_mtl_agregated.csv", stringsAsFactors=FALSE)

Facebook_Reach$X <- NULL
Facebook_Reach$X.1 <-  NULL
Facebook_Reach$X.2 <- NULL
Facebook_Reach$X.3 <- NULL


# Facebook Reach divided by 1000 person ============
 Facebook_Reach$Reach <- (Facebook_Reach$Reach/ 1000) 




# Data cleaning =============

source("./data-preparation-scripts/data-cleaning_modified_CBC.r")


# subset the people that heard about the survey through letter in Montreal =============

source("./data-preparation-scripts/Letter_analysis.r")



# Cout by date  ============
source("./data-preparation-scripts/count-by-date.r")


# Get the time period for the recruitment methods ===============================================

source("./data-preparation-scripts/time-period.r")



# Merge count data  ================

source("./data-preparation-scripts/merge-count-data.r")

# Calculate time exposure ========================

source("./data-preparation-scripts/time-exposure.r")

                                         
# remove unwanted objects ========================
rm (rec_promotion_count, grouped_RecMed, no_promotion, no_recruitment, RecMed_Wide, T_age_mean, age_mean, category, 
   table_NAs, table_temp, Diff_Part_dropped, recruitment_date_Par, date_dropped, variable_names, i, 
   condition, variable, exposure_start, exposure_end, Recruit_Period, j, na_condition, variable2 )

# #  rm (j, na_condition, variable2 )


Methods_AND_Recruited[Methods_AND_Recruited$Letter_exposure %in% c(3), "Letter_exposure"] <- 1

Methods_AND_Recruited[Methods_AND_Recruited$Letter_Reminder_exposure %in% c(3), "Letter_Reminder_exposure"] <- 1





# Subet the exposure variables in a small dataframe to check it and compare ==========================================
exposure <- Methods_AND_Recruited[, c("date", "Social_Media_Boosted_exposure", "Social_Media_exposure", "Other_Media_exposure", 
                                      "Big_newspapers_exposure", "Letter_exposure", "Partners_exposure", "Other_exposure", 
           
                                                                 "Snowball_Campaign_exposure", "Letter_Reminder_exposure")]


#  creating time variables between dates

Methods_AND_Recruited$date <- as.Date(Methods_AND_Recruited$date)


Methods_AND_Recruited$Days <- 0 

 for (i in 2:nrow(Methods_AND_Recruited)) {
    
    Methods_AND_Recruited$Days[i] <- Methods_AND_Recruited$date[i]- Methods_AND_Recruited$date[1]
}
   
Methods_AND_Recruited$Days <- Methods_AND_Recruited$Days + 1





Test <- Methods_AND_Recruited[ , c("date", "Days", "freq", "Heard_Letter", "Letter_exposure", "Letter_Reminder_exposure", 
                                  "Heard_Snowball", "Snowball_Campaign_exposure",  "Heard_Social_Media", 
                                  "Social_Media_Boosted_exposure", "Social_Media_exposure",  
                                  "Heard_Partners_Newsletter",  "Partners_exposure", "Heard_Other",   "Other_exposure", 
                                  "Heard_Media", "Other_Media_exposure",  "Big_newspapers_exposure" )]
                                  
              

# day 36 letters were sent

   Methods_AND_Recruited$days_Since_letter  <- 0 

   for (i in 36:nrow(Methods_AND_Recruited)) {
   
   Methods_AND_Recruited$days_Since_letter[i] <- Methods_AND_Recruited$date[i]- Methods_AND_Recruited$date[36]
}


# day 51- letter reminders sent:

Methods_AND_Recruited$days_Since_letter_Reminder  <- 0 

for (i in 51:nrow(Methods_AND_Recruited)) {
   
   Methods_AND_Recruited$days_Since_letter_Reminder[i] <- Methods_AND_Recruited$date[i]- Methods_AND_Recruited$date[51]
}



# day 154- Snowball campaign:
 
Methods_AND_Recruited$days_Since_Snowball  <- 0 

for (i in 154:nrow(Methods_AND_Recruited)) {
   
   Methods_AND_Recruited$days_Since_Snowball[i] <- Methods_AND_Recruited$date[i]- Methods_AND_Recruited$date[154]
}




# Merge the facebook data to the exsposure dataframe =================================================================
exposure <- merge (x=Facebook_Reach, y=exposure, by="date", sort= TRUE)




#Compute lags

source("./data-preparation-scripts/Compute_lags_Normalized.R")




# Create time period 
# ========================
Methods_AND_Recruited <- Methods_AND_Recruited[order(as.data.frame(Methods_AND_Recruited$date)),]
Methods_AND_Recruited$days <- seq.int(nrow(Methods_AND_Recruited))

Methods_AND_Recruited$new_campain <- rowSums(Methods_AND_Recruited[, c("Letter", "Other_Media", "Big_newspapers" ,
                                                                       "Other", "Partners" , "Snowball_Campaign",
                                                                       "Social_Media", "Social_Media_Boosted", 
                                                                       "Letter_Reminder")])




write.csv(Methods_AND_Recruited, file = "D:/INTERACT/Recruitment Analysis/data/Methods_AND_Recruited2.csv")






rm (exposure, exposure_lg, Facebook_Reach, recruitment_clean, recruitment_clean_check, recruitment_clean_HSurvey,
    recruitment_date, recruitment_date_HSPar, recruitment_ethnicity, recruitment_ethnicity_merge, recruitment_hear,
    recruitment_hear_Par, recruitment_method, Test, total_rec_counts_date, i, Compute_Lagged_Effect )
    
rm (test)



source( "./data-preparation-scripts/Testing_possible_lag_combinations_in_regressions_loop")


   