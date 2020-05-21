# Create the new variables in which we will add exposure
Methods_AND_Recruited[, paste(unique(recruitment_method$Source), "exposure", sep = "_")] <- 0

# Create a loop for the time exposure variable of the different recruitement campaigns. 
for (i in 1:nrow(recruitment_method)) {
  
  # Extract exposure period
  exposure_start <- as.Date(recruitment_method[i, "Date.start"])
  exposure_end <- as.Date(recruitment_method[i, "Date.end"])
  
  # Define date condition
  condition <- (as.Date(Methods_AND_Recruited$date) >= exposure_start) & (as.Date(Methods_AND_Recruited$date) <= exposure_end)
  
  # Find variable name
  variable <- paste( recruitment_method[i, "Source"], "exposure", sep = "_")
  
  # Add exposure boolean value
  Methods_AND_Recruited[condition, variable] <- Methods_AND_Recruited[condition, variable] + 1

  for(j in 1:5){
     # Create lagged variable
     variable2 <- paste(variable, "Lagged", j, sep = "_")

     # Create lag variable
     Methods_AND_Recruited[, variable2] <- Lag(Methods_AND_Recruited[, variable], +j)

     # Change NAs for 0
     na_condition <- is.na(Methods_AND_Recruited[, variable2])
     Methods_AND_Recruited[na_condition, variable2] <- 0
   }
}




