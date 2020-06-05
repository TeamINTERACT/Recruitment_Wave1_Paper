
# ==========================================
# Data: National recruitment paper data analysis - Montreal data
# Name: Rania Wasfi
# Note: Montreal regression analysis
# ==========================================


#---------- Load needed library ------------
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
library(readr)
library(MASS)
library(qwraps2)
library(summarytools)
library(datasets)
library(arm)
library(jtools)
library(table1)  # https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html
library(sandwich)
library(brms)
library(broom)
library(gridExtra)
library(broom)
library(broom.mixed)
library(gridExtra)
library(mgcv)
library(rsq)
library(gtools)
library (sjPlot)



## Data set that can be shared with public - does not inlcude any personal identifiers

Methods_AND_Recruited <- read.csv("D:/INTERACT/Recruitment Analysis/data/Methods_AND_Recruited2.csv")


      

#---------- Rania's model ---------------

### Same generalized linear model using the lm function.. 

model_testing <- function(y, Xs, data, family){
  
  # Create formula expression
  formula_string <- paste(y, paste(Xs, collapse = " + "), sep = " ~ ")
  formula_express <- eval(parse(text = formula_string))
  
  # Run model
  
  model <- glm(formula_express, data = data,  family = family)
}



# negative binomial function

model_testing2 <- function(y, Xs, data, family){
  
  # Create formula expression
  formula_string <- paste(y, paste(Xs, collapse = " + "), sep = " ~ ")
  formula_express <- eval(parse(text = formula_string))
  
  # Run model
  
  model <- glm.nb(formula_express, data = data)
}



# linear model Functions

model_testing3 <- function(y, Xs, data, family){
  
  # Create formula expression
  formula_string <- paste(y, paste(Xs, collapse = " + "), sep = " ~ ")
  formula_express <- eval(parse(text = formula_string))
  
  # Run model
  lm(formula_express, data = data)
  
}


#--------- Define list of variables to test -------

"Variables are stored in columns, with their lagged
version being stored in as many lines as we have lags.
E.g.:
original variable ->   var1   var2   var3
first lag ->           var1_1 var2_1 var3_1
second lag ->          var1_2        var3_2

We'll then iterate over each element column-wise (we
don't combine the same variable over different lags).
If we want a variable with no lag to be included, we need
to insert it as many times as we have lags for the others

For more flexibility, as we don't have to have the same
number of lags per variable..."


## Note: Reach is a logged variable 


xlist <- list (Reach= c("lg_Reach", "lg_R_Lag2", "lg_R_Lag3", "lg_R_Lag4", "lg_R_Lag5" ),
               SM= c("Social_Media_exposure", "SM_Lag2", "SM_Lag3", "SM_Lag4",  "SM_Lag5"),
               Letter= c( "Letter_Lag5", "Letter_Lag10", "Letter_Lag15"),
               Letter_reminders= c("Letter_Reminder_exposure",  "Letter_Reminder_Lag5",  "Letter_Reminder_Lag10" ),
               Partner= c("Partners_exposure", "Partners_Lag2",  "Partners_Lag3", "Partners_Lag4", "Partners_Lag5"),
               Other=  c("Other_Media_exposure", "Other_Lag2", "Other_Lag3", "Other_Lag4", "Other_Lag5" ),

               
               
               ## In_Person=  c("In_Person_exposure", "In_Person_Lag2", "In_Person_Lag3", "In_Person_Lag4", "In_Person_Lag5", "In_Person_Lag10"  ),
               
               Snowball= c( "Snowball_Campaign_exposure", "Snowball_Lag2", "Snowball_Lag3", "Snowball_Lag4", "Snowball_Lag5"),
               Other_Media = c("Other_Media_exposure", "Other_Media_Lag2", "Other_Media_Lag3", "Other_Media_Lag4", "Other_Media_Lag5"),
               Big_Newspaper= c("Big_newspapers_exposure", "Big_newspaper_Lag2", "Big_newspaper_Lag3",
                                "Big_newspaper_Lag4", "Big_newspaper_Lag5"))
               



#-------- Looping over all combinations ---------

"We want to generate (and store in a list) all possible
combinations of variables at different lags.
As stated before, we don't combine different lags of the
same variable, hence the combinations involve selecting
elements accross vectors, not within one vector."




all_cmb <- list()
cmb = vector(mode="double", length=length(xlist))
idx = rep(1, length(xlist)) #init vector of index position
len = rep(1, length(xlist)) #init vector of length of vectors stored in list
i = 1
for (v in xlist) {
  len[i] <- length(v)
  i <- i + 1
}

l = 1
while (all(idx <= len)) {
  #extract values for current index vector
  for (j in 1:length(idx)) {
    cmb[j] <- xlist[[j]][idx[j]]
  }
  all_cmb[[l]] <- cmb
  l <- l + 1
  
  #increment index vector
  i = length(idx)
  while (i > 0) {
    idx[i] <- idx[i] + 1
    if (idx[i] > len[i] && i != 1) {
      idx[i] <- 1
      i <- i - 1
    }
    else {
      i <- -1
    }
  }
}


#--------- Processing all possible combinations ----------

"Once we have all our combinations, we loop over them to
feed our model. The parameters and results are stored in a 
dataframe for further analysis"

results_all <- data.frame(i=integer(), y=character(), xs=character(), AIC=double(), BIC=double())
n <- 1
step <- 9999
while (n < length(all_cmb)) {
  results <- data.frame(i=integer(), y=character(), xs=character(), AIC=double(), BIC=double())
  i = n
  for (cmb in all_cmb[n:(n+step)]) {
    if (!is.null(cmb)) {
      fit <- model_testing("freq", cmb, data = Methods_AND_Recruited, family = gaussian())
      results <- rbind(results, data.frame(i=i, y="freq", xs=paste(cmb, collapse=";"), AIC=glance(fit)$AIC, BIC=glance(fit)$BIC))
      #print(paste0("#", i, " ", paste(cmb, collapse=";"), " -> ", glance(fit)$AIC))
      i <- i + 1
    }
  }
  results_all <- rbind(results_all, results)
  write.csv(results, file = paste0("D:/INTERACT/Recruitment Analysis/data/results", n, ".csv"), row.names = FALSE)
  print(paste0("Loop #", i, " done"))
  n <- n + step + 1
}

write.csv(results_all, file = "D:/INTERACT/Recruitment Analysis/data/Results/results_all.csv", row.names = FALSE)


results_all <- read.csv("D:/INTERACT/Recruitment Analysis/data/Results/results_all.csv", stringsAsFactors=FALSE)






# 2 -------

results <- data.frame(i=integer(), y=character(), xs=character(), AIC=double(), BIC=double())
i = 1
for (cmb in all_cmb) {
  fit <- model_testing("freq", cmb, data = Methods_AND_Recruited_with_lg, family = gaussian())
  results <- rbind(results, data.frame(i=i, y="freq", xs=paste(cmb, collapse=";"), AIC=glance(fit)$AIC, BIC=glance(fit)$BIC))
  
  print(paste0("#", i, " ", paste(cmb, collapse=";"), " -> ", glance(fit)$AIC))
  i <- i + 1
}
write.csv(results, file = "D:/INTERACT/Recruitment Analysis/data/results.csv")




results_All_df1_20 <-  rbind.data.frame( results_df1,  results_df2,  results_df3,  results_df4,  results_df5, 
                  results_df6,  results_df7,  results_df8,  results_df9,  results_df10, 
                  results_df11,  results_df12,  results_df13,  results_df14, results_df15, results_df16, results_df17, 
                  results_df18, results_df19, results_df20)


write.csv(results_All_df1_20, file = "D:/INTERACT/Recruitment Analysis/data/results_All_df1_20.csv")








# Create Xs lists to see the models, and copy  to the manuscript.... the lags were chosen from the best models fit 
## table that was created from the above setop (the loops of all combination). 


# ==============
x_main_exposure<- list("Reach","Social_Media_exposure", "Letter_exposure", 
                       "Partners_exposure", "Other_exposure",  
                       "Snowball_Campaign_exposure",  "Other_Media_exposure", 
                       "Big_newspapers_exposure", "Letter_Reminder_exposure") 
      
combX <-  list("lg_R_Lag2" ,"SM_Lag2","Letter_Lag10","Letter_Reminder_Lag5",
               "Partners_Lag2","Other_Lag3","Snowball_Lag4","Other_Media_exposure",
               "Big_newspapers_exposure")


## combX2 is put in the manuscript.... 

combX2<-  list("lg_R_Lag2" ,"SM_Lag2","Letter_Lag15","Letter_Reminder_Lag5",
               "Partners_Lag2","Other_Lag2","Snowball_Lag4","Other_Media_Lag2",
               "Big_newspaper_Lag2")


#  Checking different lms
model_best_combX2 <- model_testing(y = "freq", combX2, data = Methods_AND_Recruited, family = gaussian())
summary(model_best_combX2)


tab_model(model_best_combX2,  p.style = "asterisk")










