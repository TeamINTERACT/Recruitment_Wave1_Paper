Compute_Lagged_Effect <- function(data, main_variable, prefix, n_lags){
  "Add and compute N lagged variables from main variable
  ---
  Parameters:
   - data (data.frame): the data with the variable to be lagged
   - main_variable (string): name of the variable from which lagged varaibles will be computed
   - prefix (string): prefix string of the lagged variables, an integer suffix will be added 
      corresponding to the depth of the lag
   - n_lags (int): the number of lags to be computed
  "
  #initalize lagged variables
  for (i in 2:n_lags) {
    data[,paste0(prefix, i)] <- 0
  }
  
  
  #create the weights as a list of vectors
  lagged_weight <- list()
  
  for (i in 2:n_lags) {
    w <- c(i:1) / i
    w <- w / sum(w) # Extra line to normalize weight
    lagged_weight[[i]] <- w
  }
  
  #iteratively compute lagged variables
  for (lag in 2:n_lags) {
    
    #create the weights || shift of variable * weight is done here, not latter as in the original code
    weighted_var <- as.data.frame(data[, main_variable]) # Observation vector
    colnames(weighted_var) <- c("lagged_Effect_w1")
    
    ## Modify the first lagged so that it is weighted by the sum
    weighted_var[weighted_var$lagged_Effect_w1 %in% 1, "lagged_Effect_w1"] <- lagged_weight[[lag]][1]
    
    #calculate all other lags
    for (i in 2:lag) {
      weighted_var[,paste0("lagged_Effect_w", i)] <- c(rep(NA, i-1), head(data[, main_variable], - (i-1))) * lagged_weight[[lag]][i]
    }
    
    #Multiply the exposure variable by the weight of the N days lag effect
    for (i in 1:lag) {
      data[,paste0(prefix, lag)] <- data[,paste0(prefix, lag)] + weighted_var[,paste0("lagged_Effect_w", i)]
      #set NA values to 0
      for (j in 1:i) {
        data[j, paste0(prefix, lag)] <- 0
      }
    }
  }
  
  return(data)
}





# create lagged variables
exposure <- Compute_Lagged_Effect(exposure, "Reach", "R_Lag", 10)
exposure <- Compute_Lagged_Effect(exposure, "Social_Media_exposure", "SM_Lag", 10)
exposure <- Compute_Lagged_Effect(exposure, "Letter_exposure", "Letter_Lag", 20)
exposure <- Compute_Lagged_Effect(exposure, "Letter_Reminder_exposure", "Letter_Reminder_Lag", 10)
exposure <- Compute_Lagged_Effect(exposure, "Big_newspapers_exposure", "Big_newspaper_Lag", 10)
# exposure <- Compute_Lagged_Effect(exposure, "Smaller_newspapers_exposure", "Smaller_newspaper_Lag", 10)
exposure <- Compute_Lagged_Effect(exposure, "Other_Media_exposure", "Other_Media_Lag", 10)
exposure <- Compute_Lagged_Effect(exposure, "Partners_exposure", "Partners_Lag", 10)
exposure <- Compute_Lagged_Effect(exposure, "Snowball_Campaign_exposure", "Snowball_Lag", 10)
exposure <- Compute_Lagged_Effect(exposure, "Other_exposure", "Other_Lag", 10)


exposure$Dollars_Spent <- NULL



# All the variables are logged in the new data frame: exposure_M



exposure_lg <- exposure

exposure_lg[ , -1] <- exposure_lg[ , -1] + 0.1

exposure_lg[ , -1] <- log(exposure_lg[ , -1])



#  Delete variables that are repeated in the main data frame (Methods_AND_Recruited)

exposure$Social_Media_Boosted_exposure <- NULL
exposure$Social_Media_exposure <- NULL
exposure$Other_Media_exposure <- NULL
exposure$Letter_exposure <- NULL
exposure$Partners_exposure <- NULL
exposure$Other_exposure <- NULL
exposure$Snowball_Campaign_exposure <- NULL
exposure$Big_newspapers_exposure <- NULL
# exposure$Smaller_newspapers_exposure <- NULL
exposure$Letter_Reminder_exposure <- NULL



exposure$lagged_Effect_w5 <- NULL
exposure$lagged_Effect_w4 <- NULL
exposure$lagged_Effect_w3 <- NULL
exposure$lagged_Effect_w2 <- NULL
exposure$lagged_Effect_w1 <- NULL




colnames(exposure_lg) <- paste("lg", colnames(exposure_lg), sep= "_")

exposure_lg$date <- exposure_lg$lg_date
exposure_lg$lg_date <-NULL


exposure_lg$lg_Social_Media_Boosted_exposure <- NULL
exposure_lg$lg_Social_Media_exposure <- NULL
exposure_lg$lg_Other_Media_exposure <- NULL
exposure_lg$lg_Letter_exposure <- NULL
exposure_lg$lg_Partners_exposure <- NULL
exposure_lg$lg_Other_exposure <- NULL
exposure_lg$lg_Snowball_Campaign_exposure <- NULL
exposure_lg$lg_Big_newspapers_exposure <- NULL
# exposure$Smaller_newspapers_exposure <- NULL
exposure_lg$lg_Letter_Reminder_exposure <- NULL




# Merge lags and the logged lags to main dataframe.

# In the main dataframe the main effects of the recruitment variables are not logged. 

Methods_AND_Recruited$date <- as.character(Methods_AND_Recruited$date)

Methods_AND_Recruited <-  merge(Methods_AND_Recruited, exposure, by.x = "date", by.y = "date", all= TRUE, sort = TRUE)

Methods_AND_Recruited <-  merge(Methods_AND_Recruited, exposure_lg, by.x = "date", by.y = "date", all= TRUE, sort = TRUE)


