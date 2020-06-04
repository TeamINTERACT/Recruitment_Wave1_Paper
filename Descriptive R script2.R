
# title: "Descriptive statistics of INTERACT recruitment"
# author: "Rania  Wasfi"
# date: "7/29/2019"
# output: html

  
library(tidyverse)
library(dplyr)
library (tidyr)
library(plyr)
library(data.table)
library (ggplot2)
library(haven)

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

library(table1, quietly = TRUE)


# Upload the data ---------
Methods_AND_Recruited <- read.csv("D:/INTERACT/Recruitment Analysis/data/Methods_AND_Recruited2.csv")



# Sample characteristics by recruitment startegies ---------

# labels 

label(recruitment_clean_HSurvey$education)   <- "Education"
label(recruitment_clean_HSurvey$gender_mod)   <- "Gender"
label(recruitment_clean_HSurvey$dkn)   <- "Do not know / prefer not to answer"
label(recruitment_clean_HSurvey$income_category) <- "Income Category"
label (recruitment_clean_HSurvey$Age_category) <- "Age Category"
label(recruitment_clean$hear_clean_NCat) <- "Reported recruitment method"

label(recruitment_clean_HSurvey$hear_clean_NCat) <- "Reported recruitment method"
label(recruitment_clean_HSurvey$education_category) <- "Education level"

table(recruitment_clean_HSurvey$education_category)
table(recruitment_clean_HSurvey$education)
table(recruitment_clean_HSurvey$Age_category)


# creating factors:


recruitment_clean_HSurvey$education_category <- 
  factor(recruitment_clean_HSurvey$education_category , levels=c(1,4,5,77),
         labels=c("Less than University degree", "University degree", "Graduate degree",  "Prefer not to answer" ))

table(recruitment_clean_HSurvey$education_category)



# Gender:

table(recruitment_clean_HSurvey$gender_mod)

# We only have Men, Women, Trans man and Gender-non conforming 
# Will put in 3 categories 





  

recruitment_clean_HSurvey$gender_mod2 <- recruitment_clean_HSurvey$gender_mod 


# Will name category 5: which is GenderQueer to other


recruitment_clean_HSurvey[((recruitment_clean_HSurvey$gender_mod2 %in% 3) | (recruitment_clean_HSurvey$gender_mod2 %in% 5)), "gender_mod2" ] <- 5
table(recruitment_clean_HSurvey$gender_mod2)


recruitment_clean_HSurvey$gender_mod2 <- 
  
  factor(recruitment_clean_HSurvey$gender_mod , levels=c(1,2,5),
         labels=c("Man",  "Woman", "Other" ))






recruitment_clean_HSurvey$income_category <- 
  factor(recruitment_clean_HSurvey$income_category , levels=c(1,2,3,4,5, NA),
         labels=c("$0 to $49,999",  "$50,000 to $99,999", "$100,000 to $149,999", "$150,000 to $199,999", "$200,000 or more"))



recruitment_clean_HSurvey[recruitment_clean_HSurvey$hear_clean %in% "", "hear_clean_NCat"] <- "Unreported"


recruitment_clean_HSurvey[recruitment_clean_HSurvey$hear_clean %in% NA, "hear_clean_NCat"] <- "Didnot ask about recruitment method"


recruitment_clean_HSurvey$Ethnicity  <- recruitment_clean_HSurvey$Caucasian

table(recruitment_clean_HSurvey$hear_clean_NCat)


recruitment_clean_HSurvey$Ethnicity  <- 
  factor(recruitment_clean_HSurvey$Ethnicity , levels=c(1,0),  labels=c("Caucasian", "Other"))





## Descriptive Statistics - Characteristics of all sample that answered the eligibility survey by City  (1) --------- 
 
recruitment_clean_HSurvey_No_vIc <-   recruitment_clean_HSurvey[recruitment_clean_HSurvey$City %in% c("Montreal", 
                                                                                                      "Vancouver", "Saskatoon"), ]


table1:: table1(~ Age + hear_clean_NCat | City , data= recruitment_clean_HSurvey_No_vIc, overall="Total",  topclass="Rtable1-zebra")
               


# create another age category:

recruitment_clean_HSurvey_No_vIc$age_category2 <-recruitment_clean_HSurvey_No_vIc$Age 

recruitment_clean_HSurvey_No_vIc[(recruitment_clean_HSurvey_No_vIc$Age %in% NA), "age_category2"] <- 0
                                
                                  table(recruitment_clean_HSurvey_No_vIc$age_category2)
                                 

 recruitment_clean_HSurvey_No_vIc[(recruitment_clean_HSurvey_No_vIc$Age %in% 18:24), "age_category2"] <- 1
 recruitment_clean_HSurvey_No_vIc[(recruitment_clean_HSurvey_No_vIc$Age %in% 25:34), "age_category2"] <- 2
 recruitment_clean_HSurvey_No_vIc[(recruitment_clean_HSurvey_No_vIc$Age %in% 35:44), "age_category2"] <- 3
 recruitment_clean_HSurvey_No_vIc[(recruitment_clean_HSurvey_No_vIc$Age %in% 45:54), "age_category2"] <- 4
 recruitment_clean_HSurvey_No_vIc[(recruitment_clean_HSurvey_No_vIc$Age %in% 55:64), "age_category2"] <- 5
 recruitment_clean_HSurvey_No_vIc[(recruitment_clean_HSurvey_No_vIc$Age %in% 65:88), "age_category2"] <- 6
 
 recruitment_clean_HSurvey_No_vIc[(recruitment_clean_HSurvey_No_vIc$age_category2 %in% 0), "age_category2"] <- NA
 
                                  


table(recruitment_clean_HSurvey_No_vIc$age_category2)



recruitment_clean_HSurvey_No_vIc$age_category2 <- factor(recruitment_clean_HSurvey_No_vIc$age_category2, levels=c(1,2,3,4,5,6), 
                                              labels =c("18-24", "25-35", "35-44", "45-54", "55-64", "65-88" ))




table1:: table1(~ age_category2 + education_category + gender_mod2 + income_category + Ethnicity + income_category | hear_clean_NCat , data=recruitment_clean_HSurvey_No_vIc , overall="Total", topclass="Rtable1-zebra")




# create another age category 3 :

recruitment_clean_HSurvey$age_category3 <-recruitment_clean_HSurvey$Age 

recruitment_clean_HSurvey[(recruitment_clean_HSurvey$Age %in% NA), "age_category3"] <- 0

table(recruitment_clean_HSurvey$age_category3)


recruitment_clean_HSurvey[(recruitment_clean_HSurvey_No_vIc$Age %in% 18:34), "age_category3"] <- 1
recruitment_clean_HSurvey[(recruitment_clean_HSurvey_No_vIc$Age %in% 35:54), "age_category3"] <- 2
recruitment_clean_HSurvey[(recruitment_clean_HSurvey_No_vIc$Age %in% 55:64), "age_category3"] <- 3
recruitment_clean_HSurvey[(recruitment_clean_HSurvey_No_vIc$Age %in% 65:88), "age_category3"] <- 4

recruitment_clean_HSurvey[(recruitment_clean_HSurvey_No_vIc$Age_category3 %in% 0), "age_category2"] <- NA




table(recruitment_clean_HSurvey$age_category3)



# create another age category 3 :

recruitment_clean_HSurvey$age_category3 <-recruitment_clean_HSurvey$Age 

recruitment_clean_HSurvey[(recruitment_clean_HSurvey$Age %in% NA), "age_category3"] <- 0

table(recruitment_clean_HSurvey$age_category3)


recruitment_clean_HSurvey[(recruitment_clean_HSurvey$Age %in% 18:34), "age_category3"] <- 1
recruitment_clean_HSurvey[(recruitment_clean_HSurvey$Age %in% 35:54), "age_category3"] <- 2
recruitment_clean_HSurvey[(recruitment_clean_HSurvey$Age %in% 55:64), "age_category3"] <- 3
recruitment_clean_HSurvey[(recruitment_clean_HSurvey$Age %in% 65:88), "age_category3"] <- 4

recruitment_clean_HSurvey[(recruitment_clean_HSurvey$Age_category3 %in% 0), "age_category2"] <- NA




table(recruitment_clean_HSurvey$age_category3)



recruitment_clean_HSurvey$age_category3 <- factor(recruitment_clean_HSurvey$age_category3, levels=c(1,2,3,4), 
                                                         labels =c("18-24", "35-54","55-64", "65-88" ))




## Sample Characteristics by City for all people including those who only answered eligibility surveys  (2) ------



recruitment_clean$Completed_HSurvey <- factor(recruitment_clean$Completed_HSurvey, levels=c(0,1), 
                                            labels =c("Didnot complete Health Survey", "Completed eligibility and health survey"))




table1:: table1(~ Age  | City*Completed_HSurvey , data=recruitment_clean, overall="Total", topclass="Rtable1-zebra")



table1:: table1(~ Age  | City*Completed_HSurvey , data=recruitment_clean, overall="Total", topclass="Rtable1-zebra")



table1:: table1(~ Age + hear_clean_NCat + gender_mod | City*Completed_HSurvey , data=recruitment_clean, overall="Total", topclass="Rtable1-zebra")




table1:: table1(~ Completed_HSurvey  |hear_clean_NCat , data=recruitment_clean, overall="Total", topclass="Rtable1-zebra")






##  Descriptive Statistics -  Recruited Sample Characteristic by City for all people and people who dropped after eligibility survey (3)-----

table1:: table1(~ Age_category + education_category + income_category + gender_mod + Aboriginal + Asian +  Black + Caucasian + Latin_American +  Middle_Eastern + Other + dkn +   hear_clean_NCat| City , data=recruitment_clean_HSurvey, overall="Total", topclass="Rtable1-zebra")





## Participants Characteristics by Recruitment strategy (4)-----

table1:: table1(~ Age_category + education_category + gender_mod + income_category + Aboriginal + Asian +  Black + Caucasian + Latin_American +  Middle_Eastern + Other + dkn +  City  | hear_clean_NCat , data=recruitment_clean_HSurvey, overall="Total", topclass="Rtable1-zebra")



## Participants Characteristics by Recruitment strategy (5)-----

table1:: table1(~ Age_category + education_category + gender_mod + income_category + Ethnicity + income_category  +City | hear_clean_NCat , data=recruitment_clean_HSurvey_No_vIc, overall="Total", topclass="Rtable1-zebra")



## Participants options: 

table(recruitment_clean$participation_option)

recruitment_clean$participation_option <- factor(recruitment_clean$participation_option, level=c(1,2,3,4), 
                                                 labels =c("Complete Participation", "Intermediate Participation - Mobile App",
                                                           "Intermediate Participation - Motion Sensor", "Basic Participation"))



recruitment_clean_HSurvey$Caucasian <- factor(recruitment_clean_HSurvey$Caucasian, level=c(1,2,3,4), 
                                                 labels =c("Complete Participation", "Intermediate Participation - Mobile App",
                                                           "Intermediate Participation - Motion Sensor", "Basic Participation"))



table1:: table1 (~ participation_option | City, data=  recruitment_clean, overall="Total", topclass="Rtable1-zebra")






table1:: table1 (~ participation_option | Completed_HSurvey*City, data=  recruitment_clean, overall="Total", topclass="Rtable1-zebra")


kruskal.test(Time_eligibility_2Health ~ hear_clean_NCat, data = recruitment_clean)






table1:: table1 (~  hear_clean_NCat | Completed_HSurvey*City, data=  recruitment_clean, overall="Total", topclass="Rtable1-zebra")




kruskal.test(Age ~ Completed_HSurvey, data = recruitment_clean)


kruskal.test(Age ~ hear_clean_NCat, data = recruitment_clean)






recruitment_clean_HSurvey_MTl <- recruitment_clean_HSurvey[recruitment_clean_HSurvey$City %in% "Montreal",  ]


recruitment_clean_HSurvey_SKT <- recruitment_clean_HSurvey[recruitment_clean_HSurvey$City %in% "Saskatoon",  ]


recruitment_clean_HSurvey_VAN <- recruitment_clean_HSurvey[recruitment_clean_HSurvey$City %in% "Vancouver",  ]


recruitment_clean_HSurvey_VIC <- recruitment_clean_HSurvey[recruitment_clean_HSurvey$City %in% "Victoria",  ]




table1:: table1(~ Age_category + education_category + gender_mod2 + income_category + Ethnicity | hear_clean_NCat , data=recruitment_clean_HSurvey_No_vIc , overall="Total", topclass="Rtable1-zebra")


table1:: table1(~ Age_category2 + education_category + gender_mod2 + income_category + Ethnicity  | hear_clean_NCat , data=recruitment_clean_HSurvey_No_vIc , overall="Total", topclass="Rtable1-zebra")


table1:: table1(~ age_category3 + education_category + gender_mod2 + income_category + Ethnicity  | hear_clean_NCat , data=recruitment_clean_HSurvey_No_vIc , overall="Total", topclass="Rtable1-zebra")

kruskal.test(Age ~ hear_clean_NCat, data = recruitment_clean_HSurvey )

pairwise.wilcox.test(recruitment_clean_HSurvey$Age, recruitment_clean_HSurvey$hear_clean_NCat)



# Montreal 

table1:: table1(~ age_category3 + education_category + gender_mod2 + income_category + Ethnicity  | hear_clean_NCat , data=recruitment_clean_HSurvey_MTl , overall="Total", topclass="Rtable1-zebra")


kruskal.test(Age ~ hear_clean_NCat, data = recruitment_clean_HSurvey_MTl )

pairwise.wilcox.test(recruitment_clean_HSurvey_MTl$Age, recruitment_clean_HSurvey_MTl$hear_clean_NCat)



# Saskatoon  

table1:: table1(~ age_category3 + education_category + gender_mod2 + income_category + Ethnicity+ income_category | hear_clean_NCat , data=recruitment_clean_HSurvey_SKT , overall="Total", topclass="Rtable1-zebra")


kruskal.test(Age ~ hear_clean_NCat, data = recruitment_clean_HSurvey_SKT)

pairwise.wilcox.test(recruitment_clean_HSurvey_SKT$Age, recruitment_clean_HSurvey_SKT$hear_clean_NCat)



# Vancouver 

table1:: table1(~  age_category3 + education_category + gender_mod2 + income_category + Ethnicity + income_category | hear_clean_NCat , data=recruitment_clean_HSurvey_VAN , overall="Total", topclass="Rtable1-zebra")


kruskal.test(Age ~ hear_clean_NCat, data = recruitment_clean_HSurvey_VAN)

pairwise.wilcox.test( recruitment_clean_HSurvey_VAN$Age, recruitment_clean_HSurvey_VAN$hear_clean_NCat)



# Victoria

table1:: table1(~ Age_category + education_category + gender_mod + income_category + Ethnicity + income_category , data=recruitment_clean_HSurvey_VIC , overall="Total", topclass="Rtable1-zebra")


kruskal.test(Age ~ hear_clean_NCat, data = recruitment_clean_HSurvey_VIC)

pairwise.wilcox.test( recruitment_clean_HSurvey_VIC$Age, recruitment_clean_HSurvey_VIC$hear_clean_NCat)




