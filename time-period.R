# Whole recruitment period
Recruit_Period <- seq(as.Date("2018-06-06"), as.Date("2018-12-21"), by="days")

# setdiff(as.character(Recruit_Period), recruitment_date$date)

# Find date without recruitment
no_recruitment <- data.frame(date = setdiff(as.character(Recruit_Period), recruitment_date$date),
                             freq = 0)

#  Count source occurence by Date.start and Source
grouped_RecMed <- aggregate(x = recruitment_method$Source, 
                            by = with(recruitment_method, list(Date.start, Source)), 
                            FUN = length)

names(grouped_RecMed) <- c("date.start", "Rec.Method", "freq") 
grouped_RecMed <- grouped_RecMed[order(grouped_RecMed$date.start),]


# Reshape long to wide format
RecMed_Wide <- spread(grouped_RecMed, Rec.Method, freq)

### Delete the row that has unknown value of time 
RecMed_Wide <- RecMed_Wide[RecMed_Wide$date.start != "unknown", ]

### Change NA to 0 in the frequency of recruitment methods used 
RecMed_Wide[is.na(RecMed_Wide)] <- 0

# Create a data frame with dates without recruitment 
no_promotion <- data.frame(date.start = setdiff(as.character(Recruit_Period), RecMed_Wide$date),
                           Letter = NA, Other_Media = NA, Other = NA, Partners = NA, Snowball_Campaign= NA, 
                           Social_Media = NA, Social_Media_Boosted = NA, Big_newspapers=NA, Letter_Reminder=NA) 




# Row bind promotion campain 
rec_promotion_count <- rbind(RecMed_Wide, no_promotion)
rec_promotion_count <- rec_promotion_count[order(rec_promotion_count$date.start), ]

# Change NAs for 0s
rec_promotion_count[is.na(rec_promotion_count)] <- 0

