# Methods and recruited
Methods_AND_Recruited <- merge(rec_promotion_count, total_rec_counts_date , by.x = "date.start", by.y = "date", all= TRUE, sort = TRUE)

# Change date.start for date
names(Methods_AND_Recruited)[names(Methods_AND_Recruited) %in% "date.start"] <- "date"

# Change NAs for 0s
Methods_AND_Recruited[is.na(Methods_AND_Recruited)] <- 0
Methods_AND_Recruited[Methods_AND_Recruited$drop_mean_age  %in% 0, "drop_mean_age" ] <- NA
Methods_AND_Recruited[Methods_AND_Recruited$participate_mean_age %in% 0.00, "participate_mean_age" ] <- NA

### Add recruitement soft launch
Methods_AND_Recruited[Methods_AND_Recruited$date.start %in% "2018-06-06", "Other"] <- 1

### create a binary variable for the recruitment campaigns
Methods_AND_Recruited$Letter_B <- Methods_AND_Recruited$Letter
Methods_AND_Recruited[Methods_AND_Recruited$Letter_B %in% c(1:3), "Letter_B"] <- 1

Methods_AND_Recruited$Social_Media_B <- Methods_AND_Recruited$Social_Media
Methods_AND_Recruited[Methods_AND_Recruited$Social_Media_B %in% c(1:5), "Social_Media_B"] <- 1

Methods_AND_Recruited$Social_Media_Boosted_B <- Methods_AND_Recruited$Social_Media_Boosted
Methods_AND_Recruited[Methods_AND_Recruited$Social_Media_Boosted_B %in% c(1:5), "Social_Media_Boosted_B"] <- 1

Methods_AND_Recruited$Other_B<- Methods_AND_Recruited$Other
Methods_AND_Recruited[Methods_AND_Recruited$Other_B %in% c(1:5), "Other_B"] <- 1

Methods_AND_Recruited$Other_B<- Methods_AND_Recruited$Other
Methods_AND_Recruited[Methods_AND_Recruited$Other_B %in% c(1:5), "Other_B"] <- 1

Methods_AND_Recruited$Media_B<- Methods_AND_Recruited$Other_Media
Methods_AND_Recruited[Methods_AND_Recruited$Media_B %in% c(1:2), "Media_B"] <- 1


Methods_AND_Recruited$Prom_newspaper_B<- Methods_AND_Recruited$Prominent_newspapers
Methods_AND_Recruited[Methods_AND_Recruited$Prom_newspaper_B %in% c(1), "Big_newspaper_B"] <- 1

