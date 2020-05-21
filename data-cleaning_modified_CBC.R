# recruitment_clean
# -----------------

                                                                                                                                                                                                                                       

# Most of the intitial cleaning from the original csv was done in STATA... This is a continuation:


recruitment_clean[((recruitment_clean$hear %in% 3) & (recruitment_clean$City %in% "Montreal")), "hear_clean" ] <- "Social_Media"

recruitment_clean[((recruitment_clean$hear %in% 3) & (recruitment_clean$City %in% "Vancouver")), "hear_clean" ] <- "Social_Media"



recruitment_clean[((recruitment_clean$hear %in% 1) & (recruitment_clean$City %in% "Montreal")), "hear_clean" ] <- "Letter" 

recruitment_clean[((recruitment_clean$hear %in% 1) & (recruitment_clean$City %in% "Vancouver")), "hear_clean" ] <- "Letter" 





recruitment_clean[((recruitment_clean$hear %in% 2) & (recruitment_clean$City %in% "Montreal")), "hear_clean" ] <- "Friend/Family" 

recruitment_clean[((recruitment_clean$hear %in% 2) & (recruitment_clean$City %in% "Vancouver")), "hear_clean" ] <- "Friend/Family" 


recruitment_clean[((recruitment_clean$hear %in% 4) & (recruitment_clean$City %in% "Vancouver")), "hear_clean" ] <- "Met with study team"  



recruitment_clean[((recruitment_clean$hear %in% 5) & (recruitment_clean$City %in% "Montreal")), "hear_clean" ] <- "Website"  




recruitment_clean[recruitment_clean$hear_txt %in% c("La Presse", "Journal Le Devoir", "Journal La Presse", 
                                                    "radio de Radio-Canada", "journal Le Devoir", "Le Devoir"	, 
                                                    "Dans le journal Le Devoir","La Presse"	, 
                                                    "Daybreak radio programme", 	"La Presse"	, 
                                                    "La Presse", 	"La Presse"	, "journal", "La Presse +"), "hear_clean" ] <- "Media"  



recruitment_clean[recruitment_clean$hear_txt %in% c("recrutement CHUM 20181219", "Intranet du CHUM")	 , "hear_clean" ] <- "CHUM"  


recruitment_clean[recruitment_clean$hear_txt %in% c("Publicit� de v�lo qu�bec")	 , "hear_clean" ] <- "Partners_Newsletter"  


recruitment_clean[recruitment_clean$hear_txt %in% c("lumifesty")	 , "hear_clean" ] <- "Social_Media"  




recruitment_clean[recruitment_clean$hear_txt %in% c("South Granville Seniors Newsletter" ), "hear_clean" ] <- "Partners_Newsletter"   



recruitment_clean[recruitment_clean$hear_txt %in% c("email request from Caitlin Pugh.  I have participated in a previous study on the Arbutus Greenway" ), "hear_clean" ] <- "Other"   



	
	




recruitment_clean[recruitment_clean$hear_clean %in% "Partners Newsletter/University website", "hear_clean" ] <- "Partners_Newsletter"





recruitment_clean[recruitment_clean$hear_clean_4Cat %in%  "", "hear_clean_4Cat"]  <- NA
recruitment_clean[recruitment_clean$hear_clean_4Cat %in%  "Social Media", "hear_clean_4Cat"]  <- "Social_Media"
recruitment_clean[recruitment_clean$date_Health_survey %in% "", "date_Health_survey"] <- ""



recruitment_clean[recruitment_clean$hear_clean %in% "Partners Newsletter/University website", "hear_clean" ] <- "Partners_Newsletter"


recruitment_clean[recruitment_clean$hear_clean %in% "Partners Newsletter/University website", "hear_clean" ] <- "Partners_Newsletter"




recruitment_clean[recruitment_clean$hear_txt %in% c("Referral from university" ,  "email from school of urban planning via zoe poirier" , 
                                                    "Accueil du portail CRCHUM" ,  "Intranet du CHUM", "Site interne CHUM", "intranet chum" , "Chum" , 
                                                    "Through my college's website (Marianopolis College)", 
                                                    "Publicit� au CHUM", "Au chum", "Vu l'annonce au CHUM", "Au chum", "Vu l'annonce au CHUM", "CHUM",
                                                    "Par le Centre Hopitalier de l'Universit� de Montr�al", "employee CHUM", "McGill PGSS newswire" ), 
                  "hear_clean" ]  <- "University_Website"    



recruitment_clean[recruitment_clean$hear_clean %in% "Social Media", "hear_clean" ] <- "Social_Media" 

# Create a new column for another categories for "how did you hear about the study"
recruitment_clean$hear_clean_NCat <- recruitment_clean$hear_clean


recruitment_clean[recruitment_clean$hear_clean %in% c("Met with study team", "Website" , "Poster" , "University_Website", "CHUM"), "hear_clean_NCat"] <- "Other"




recruitment_clean[recruitment_clean$hear_clean %in% "", "hear_clean_NCat"] <- "Unreported"


recruitment_clean[recruitment_clean$hear_clean_NCat %in% "Friend/Family", "hear_clean_NCat"] <- "Snowball"



recruitment_clean_check <-  recruitment_clean [ , c("interact_id", "City"  , "hear_clean_NCat", "hear", "hear_txt", "hear_clean", "hear_clean_skt") ]

recruitment_clean_check <- NULL



# change the date of eligibility survey in some cases to a +1 (where there was a time difference problem detection between the two dates)"



recruitment_clean[recruitment_clean$City %in% "Victoria", "hear_clean_NCat"] <- NA


recruitment_clean[((recruitment_clean$City %in% "Saskatoon") & (recruitment_clean$hear_clean_NCat %in% "Letter")) , "hear_clean_NCat"] <- "Other"



unique(recruitment_clean$education)



recruitment_clean$education_category <-  recruitment_clean$education

recruitment_clean[((recruitment_clean$education_category %in% 2)  | (recruitment_clean$education_category %in% 3)), "education_category"] <- 1
recruitment_clean[(recruitment_clean$education %in% NA) , "education_category"] <- NA

                                       
unique(recruitment_clean$education_category)
table (recruitment_clean$education_category)

### Flag  participants that completed the health survey 

recruitment_clean$Completed_HSurvey <- 1
recruitment_clean[recruitment_clean$date_Health_survey %in% NA, "Completed_HSurvey" ] <- 0 

recruitment_clean_HSurvey <- recruitment_clean[recruitment_clean$Completed_HSurvey %in% 1,  ] 



unique (recruitment_clean$hear_clean_NCat)


unique(recruitment_clean_HSurvey$income_category)

#### code 


## // 1= "$0 to $49,999" if income<=7
##  2= "$50,000 to $99,999" if income==8
## 3= "$100,000 to $149,999" if income==9
## 4= "$150,000 to $199,999" if income==10
## 5= "$200,000 or more" if income==11
## . = "" if income==77 //** 
  

# Demographic charcateristics  - Ethnicity ==============

# Recoding Ethnicity variables from Mtl and Skt  to Vancouver and Victoria coding


recruitment_ethnicity <- recruitment_clean_HSurvey[ , c("interact_id", "City", "group_id_mtl", "group_id_mtl_1", "group_id_mtl_2", "group_id_mtl_3", 
                                                                 "group_id_mtl_4" , "group_id_mtl_5", "group_id_mtl_6", "group_id_mtl_7", 
                                                                 "group_id_mtl_8", "group_id_mtl_9", "group_id_mtl_10", "group_id_mtl_11", 
                                                                 "group_id_mtl_12",   "group_id_mtl_13", "group_id_mtl_77", "group_id_mtl_txt", 
                                                                 
                                                                  "group_id_skt", "group_id_skt_1", "group_id_skt_2", "group_id_skt_3", 
                                                                 "group_id_skt_4" , "group_id_skt_5", "group_id_skt_6", "group_id_skt_7", 
                                                                 "group_id_skt_8", "group_id_skt_9", "group_id_skt_10", "group_id_skt_11", 
                                                                 "group_id_skt_12",   "group_id_skt_13", "group_id_skt_77" , "group_id_skt_txt", 
                                                                 
                                                                 "group_id", "group_id_1", "group_id_2", "group_id_3", "group_id_4", "group_id_5", 
                          
                                                        
                                                                 "group_id_6", "group_id_7", "group_id_77", "group_id_txt")]





# Recoding Montreal Ethnicity the other category... Jewsish - should I put them as white 


# Montreal ehnicity clean (other)  ----

# [1] ""                                                                                                         
# [2] "Scottish and West Indian" ---    White  &  West Asian                                                                                
# [3] "Jewish"    --- Other                                                                                                  
# [4] "Arménien" ---    White                                                                                                
# [5] "jewish" --- Other                                                                                                     
# [6] "A mix of Polish and Jewish ..."  ---  white                                                                          
# [7] "Middle Eastern"  ---  Arab (for the sake of this analysis will code it as arab, as it will be recoded back to the Vancouver categories)                                                                                        
# [8] "Français"     ---    White                                                                                       
# [9] "Eurasienne"    ---    White                                                                                       
# [10] "créole"       --- Other                                                                                           
# [11] "Greek, Italian"  ---   White                                                                                      
# [12] "European Caucasian"  --- White                                                                                    
# [13] "Métis antillais" ---   Indigenous or Aboriginal                                                                                  
# [14] "Québécois"   --- White                                                                                            
# [15] "Je choisis Autre pour faire un commentaire. Blanc ou Noir ne sont pas des groupes ethniques ou culturels." - other
# [16] "Araboquebecoise (metis)"- Arab, White                                                                                
# [17] "berbère (Kabyle)" - Arab                                                                                       
# [18] "European"  - white                                                                                                
# [19] "caucasien"  - white                                                                                                 
# [20] "Greek"      - white                                                                                                 
# [21] "France"     - white                                                                                                 
# [22] "Eastern European descent"    - white                                                                                
# [23] "Hungarian"   - Other                                                                                             
# [24] "European (West; Celtic; Nordic); African; Indo-Asian; Rom"   - White - South Asian - Black                                       
# [25] "Roumain"   - White                                                                                               
# [26] "Polish"   -   White                                                                                                  
# [27] "Maghrébine"  -  Arab                                                                                          
# [28] "Belge"     - White                                                                    


# change the other to 0 
recruitment_ethnicity[((recruitment_ethnicity$group_id_mtl_txt %in% c( "Scottish and West Indian", "Arménien", "Français", "Eurasienne" , 
                                                                       "Greek, Italian" , "European Caucasian"  ,  "Québécois", 
                                                                       "Araboquebecoise (metis)", "European", "caucasien", "Greek", 
                                                                       "France", "Eastern European descent", "European (West; Celtic; Nordic); African; Indo-Asian; Rom",
                                                                       "Roumain", "Polish", "Belge", "A mix of Polish and Jewish ..." )) & 
                                 (recruitment_ethnicity$group_id_mtl_2 != 1)),  "group_id_mtl_13"] <- 0



recruitment_ethnicity[((recruitment_ethnicity$group_id_mtl_txt %in% c( "Scottish and West Indian", "European (West; Celtic; Nordic); African; Indo-Asian; Rom")) &
                                  (recruitment_ethnicity$group_id_mtl_10 != 1)),  "group_id_mtl_13"] <- 0


recruitment_ethnicity[((recruitment_ethnicity$group_id_mtl_txt %in% c( "créole", "European (West; Celtic; Nordic); African; Indo-Asian; Rom" )) & 
                                 (recruitment_ethnicity$group_id_mtl_5 != 1)),  "group_id_mtl_13"] <- 0




recruitment_ethnicity[((recruitment_ethnicity$group_id_mtl_txt %in% c( "Maghrébine", "Middle Eastern", "Araboquebecoise (metis)", "berbère (Kabyle)", "Maghrébine")) & 
                                (recruitment_ethnicity$group_id_mtl_5 != 1)),  "group_id_mtl_13"] <- 0
                                                                        



recruitment_ethnicity[((recruitment_ethnicity$group_id_mtl_txt %in% c( "Métis antillais"  )) & 
                                (recruitment_ethnicity$group_id_mtl_1 != 1)),  "group_id_mtl_13"] <- 0



# change the other categories to 1
recruitment_ethnicity[((recruitment_ethnicity$group_id_mtl_txt %in% c( "Scottish and West Indian", "Arménien", "Français", "Eurasienne" , 
                                                                       "Greek, Italian" , "European Caucasian"  ,  "Québécois", 
                                                                       "Araboquebecoise (metis)", "European", "caucasien", "Greek", 
                                                                       "France", "Eastern European descent", "European (West; Celtic; Nordic); African; Indo-Asian; Rom",
                                                                       "Roumain", "Polish", "Belge", "A mix of Polish and Jewish ..." )) & 
                                                                       (recruitment_ethnicity$group_id_mtl_2 != 1)),  "group_id_mtl_2"] <- 1



recruitment_ethnicity[((recruitment_ethnicity$group_id_mtl_txt %in% c( "Scottish and West Indian", "European (West; Celtic; Nordic); African; Indo-Asian; Rom")) &
                        (recruitment_ethnicity$group_id_mtl_10 != 1)),  "group_id_mtl_10"] <- 1


recruitment_ethnicity[((recruitment_ethnicity$group_id_mtl_txt %in% c( "créole", "European (West; Celtic; Nordic); African; Indo-Asian; Rom" )) & 
                         (recruitment_ethnicity$group_id_mtl_5 != 1)),  "group_id_mtl_5"] <- 1




recruitment_ethnicity[((recruitment_ethnicity$group_id_mtl_txt %in% c( "Maghrébine", "Middle Eastern", "Araboquebecoise (metis)",
                                                                       "berbère (Kabyle)", "Maghrébine")) & (recruitment_ethnicity$group_id_mtl_5 != 1)),  
                                                                        "group_id_mtl_5"] <- 1



recruitment_ethnicity[((recruitment_ethnicity$group_id_mtl_txt %in% c( "Métis antillais"  )) & 
                         (recruitment_ethnicity$group_id_mtl_1 != 1)),  "group_id_mtl_1"] <- 1




# Saskatoon ethnicity clean (other)  ----

# [2] "Baloch from Turbat, Balochistan"- south Asian
# [3] "Europe" - white                        
# [4] "Mauritian"- south Asian               
# [5] "fransaskois (white)" - White           
# [6] "Caucasian"  - white                    
# [7] "Indian"    - south Asian                     
# [8] "Jamaican" - Black (the country is 90% black)                      
# [9] "celtic"   - white                      
# [10] "Fransaskois" - white 

# change the other to 0 group_id_skt_txt to 0 

recruitment_ethnicity[((recruitment_ethnicity$group_id_skt_txt %in% c( "Baloch from Turbat, Balochistan","Mauritian", "Indian" )) 
                       & (recruitment_ethnicity$group_id_skt_3 != 1)),  
                      "group_id_skt_13"] <- 0



recruitment_ethnicity[((recruitment_ethnicity$group_id_skt_txt %in% c( "Europe", "fransaskois (white)", "Caucasian", "celtic" , "Fransaskois" )) 
                       & (recruitment_ethnicity$group_id_skt_2 != 1)),  
                      "group_id_skt_13"] <- 0


recruitment_ethnicity[((recruitment_ethnicity$group_id_skt_txt %in% c( "Jamaican" )) 
                       & (recruitment_ethnicity$group_id_skt_5 != 1)),  
                      "group_id_skt_13"] <- 0



# change the other categories to 1

recruitment_ethnicity[((recruitment_ethnicity$group_id_skt_txt %in% c( "Baloch from Turbat, Balochistan","Mauritian", "Indian" )) 
                       & (recruitment_ethnicity$group_id_skt_3 != 1)),  
                      "group_id_skt_3"] <- 1



recruitment_ethnicity[((recruitment_ethnicity$group_id_skt_txt %in% c( "Europe", "fransaskois (white)", "Caucasian", "celtic" , "Fransaskois" )) 
                       & (recruitment_ethnicity$group_id_skt_2 != 1)),  
                      "group_id_skt_2"] <- 1


recruitment_ethnicity[((recruitment_ethnicity$group_id_skt_txt %in% c( "Jamaican" )) 
                       & (recruitment_ethnicity$group_id_skt_5 != 1)),  
                      "group_id_skt_5"] <- 1






# Vancouver and Victoria other 

#[1] ""                                                    
#[2] "Eastern European"  -   Caucasian                                
#[3] "Greek"       Caucasian                                        
#[4] "Canadian"    Other                                       
#[5] "french"      Caucasian                                        
#[6] "Ashkenazy Jews (Eastern European)"  - Caucasian                 
#[7] "English" - Caucasian                                            
#[8] "European - German" -  Caucasian                            
#[9] "I am adopted and do not know my full family history." - other 
#[10] "Polynesian"  - other                                        
#[11] "Neanderthal"  -  nice one :!  other    



# change the other to 0 

recruitment_ethnicity[((recruitment_ethnicity$group_id_txt %in% c( "Eastern European", "Greek" , "french"  , 
                                                                   "Ashkenazy Jews (Eastern European)","English", 
                                                                   "European - German" ))  & (recruitment_ethnicity$group_id_4 != 1)),  
                                                                   "group_id_7"] <- 0


# change the other categories to 1

recruitment_ethnicity[((recruitment_ethnicity$group_id_txt %in% c( "Eastern European", "Greek" , "french"  , 
                                                                       "Ashkenazy Jews (Eastern European)","English", 
                                                                       "European - German" ))  & (recruitment_ethnicity$group_id_4 != 1)),  
                                                                       "group_id_4"] <- 1


                      

                      

# change All the coding to the Vancouver Victoria code: ---


# Aboriginal
  recruitment_ethnicity$Aboriginal  <- recruitment_ethnicity$group_id_1
  
  recruitment_ethnicity[(recruitment_ethnicity$City %in% "Montreal" &  recruitment_ethnicity$group_id_mtl_1 %in% 1),  "Aboriginal"] <- 1
  
  recruitment_ethnicity[(recruitment_ethnicity$City %in% "Saskatoon" &  recruitment_ethnicity$group_id_skt_1 %in% 1),  "Aboriginal"] <- 1
  
  recruitment_ethnicity[recruitment_ethnicity$Aboriginal %in% NA, "Aboriginal"]  <- 0 
  
  
                        
  
  
# Asian
recruitment_ethnicity$Asian  <- recruitment_ethnicity$group_id_2
  
 recruitment_ethnicity[(recruitment_ethnicity$City %in% "Montreal") & 
                       (recruitment_ethnicity$group_id_mtl_3 %in% 1 | 
                        recruitment_ethnicity$group_id_mtl_4 %in% 1 | 
                        recruitment_ethnicity$group_id_mtl_6 %in% 1 |
                        recruitment_ethnicity$group_id_mtl_9 %in% 1 |
                        recruitment_ethnicity$group_id_mtl_10 %in% 1 |
                        recruitment_ethnicity$group_id_mtl_11 %in% 1 |
                        recruitment_ethnicity$group_id_mtl_12 %in% 1) , "Asian"]  <- 1 
 
 
 
 recruitment_ethnicity[(recruitment_ethnicity$City %in% "Saskatoon")  & 
                          (recruitment_ethnicity$group_id_skt_3 %in% 1 | 
                          recruitment_ethnicity$group_id_skt_4 %in% 1 | 
                          recruitment_ethnicity$group_id_skt_6 %in% 1 |
                          recruitment_ethnicity$group_id_skt_9 %in% 1 |
                          recruitment_ethnicity$group_id_skt_10 %in% 1 |
                          recruitment_ethnicity$group_id_skt_11 %in% 1 |
                          recruitment_ethnicity$group_id_skt_12 %in% 1),  "Asian"] <- 1
   
   
 recruitment_ethnicity[recruitment_ethnicity$Asian %in% NA, "Asian"]  <- 0 
 
 
 
 
 # Black
 recruitment_ethnicity$Black  <- recruitment_ethnicity$group_id_3
 
 recruitment_ethnicity[(recruitment_ethnicity$City %in% "Montreal") & 
                      (recruitment_ethnicity$group_id_mtl_5 %in% 1)   , "Black"]  <- 1 
 
               
 recruitment_ethnicity[(recruitment_ethnicity$City %in% "Saskatoon") & 
                         (recruitment_ethnicity$group_id_skt_5 %in% 1)   , "Black"]  <- 1 
 
 
 recruitment_ethnicity[recruitment_ethnicity$Black %in% NA, "Black"]  <- 0 
 
 
  # Caucasian
  recruitment_ethnicity$Caucasian  <- recruitment_ethnicity$group_id_4
 
  recruitment_ethnicity[(recruitment_ethnicity$City %in% "Montreal") & 
                          (recruitment_ethnicity$group_id_mtl_2 %in% 1)   , "Caucasian"]  <- 1 
  
  
  recruitment_ethnicity[(recruitment_ethnicity$City %in% "Saskatoon") & 
                          (recruitment_ethnicity$group_id_skt_2 %in% 1)   , "Caucasian"]  <- 1 
  
  
  recruitment_ethnicity[recruitment_ethnicity$Caucasian %in% NA, "Caucasian"]  <- 0 
  
  
  
  
  # Latin_American
  
  recruitment_ethnicity$Latin_American  <- recruitment_ethnicity$group_id_5 
  
  recruitment_ethnicity[(recruitment_ethnicity$City %in% "Montreal") & 
                          (recruitment_ethnicity$group_id_mtl_7 %in% 1)   , "Latin_American"]  <- 1 
  
  
  recruitment_ethnicity[(recruitment_ethnicity$City %in% "Saskatoon") & 
                          (recruitment_ethnicity$group_id_skt_7 %in% 1)   , "Latin_American"]  <- 1 
  
  
  recruitment_ethnicity[recruitment_ethnicity$Latin_American %in% NA, "Latin_American"]  <- 0 
  
  
  ## This is not very precise as It will put the Arab Morocos with middle East.. (Not very precise with North African- Moraccos country--- ) 
  
  
  
  recruitment_ethnicity$Middle_Eastern  <- recruitment_ethnicity$group_id_mtl_6 
  
  recruitment_ethnicity[(recruitment_ethnicity$City %in% "Montreal") & 
                          (recruitment_ethnicity$group_id_mtl_8 %in% 1)   , "Middle_Eastern"]  <- 1 
  
  
  recruitment_ethnicity[(recruitment_ethnicity$City %in% "Saskatoon") & 
                          (recruitment_ethnicity$group_id_skt_8 %in% 1)   , "Middle_Eastern"]  <- 1 
  
  
  recruitment_ethnicity[recruitment_ethnicity$Middle_Eastern %in% NA, "Middle_Eastern"]  <- 0 
  
  
  
  # Other 
  recruitment_ethnicity$Other  <- recruitment_ethnicity$group_id_7
  
  
  recruitment_ethnicity[(recruitment_ethnicity$City %in% "Montreal") & 
                          (recruitment_ethnicity$group_id_mtl_8 %in% 1)   , "Other"]  <- 1 
  
  
  recruitment_ethnicity[(recruitment_ethnicity$City %in% "Saskatoon") & 
                          (recruitment_ethnicity$group_id_skt_8 %in% 1)   , "Other"]  <- 1 
  
  
  recruitment_ethnicity[recruitment_ethnicity$Other %in% NA, "Other"]  <- 0
  
  
  
  
  # dkn 
  recruitment_ethnicity$dkn  <- recruitment_ethnicity$group_id_77
  
  recruitment_ethnicity[(recruitment_ethnicity$City %in% "Montreal") & 
                          (recruitment_ethnicity$group_id_mtl_77 %in% 1)   , "dkn"]  <- 1 
  
  
  
  
  recruitment_ethnicity[(recruitment_ethnicity$City %in% "Saskatoon") & 
                          (recruitment_ethnicity$group_id_skt_77 %in% 1)   , "dkn"]  <- 1 
  

  recruitment_ethnicity[recruitment_ethnicity$dkn %in% NA, "dkn"]  <- 0
  
  
  
  # subset data to merge health data
  recruitment_ethnicity_merge  <-  recruitment_ethnicity[ , c ("interact_id", "Aboriginal" , "Asian", "Black", "Caucasian", "Latin_American",  "Middle_Eastern", "Other", "dkn")]
  
              

  
  # merge to clean health survey data --- 

  
  recruitment_clean_HSurvey <- merge(recruitment_clean_HSurvey, recruitment_ethnicity_merge, by.x= "interact_id", by.y= "interact_id", sort = TRUE  )
  
                
 
                     
  
  
   
# recruitment method
# -------------------
# recoding source variable
recruitment_method[recruitment_method$Source %in% c("social media (3)", "social Media (3)",
                                                    "Social media (3)", "Social Media (3)",
                                                    "SOcial media (3)", "social media (3) ",
                                                    "Social media (3) ", "Social Media (3) "), "Source"] <- "Social_Media"

  
recruitment_method[recruitment_method$Product %in% c( "Facebook Post"), "Source"] <- "Social_Media"
  


recruitment_method[recruitment_method$Source %in% c("Letter (1)", "Letter (1)", "Letter (1) "), "Source"] <- "Letter"



recruitment_method[recruitment_method$Product %in% c("mail-out : reminder post-card"), "Source"] <- "Letter_Reminder"


 recruitment_method[recruitment_method$Source %in% c("Met with study team (4)", "Other (99)"), "Source"] <- "Other"


##recruitment_method[recruitment_method$Source %in% c("Other (99)"), "Source"] <- "Other"

##recruitment_method[recruitment_method$Source %in% c("Met with study team (4)"), "Source"] <- "In_Person"


# recoding date.start
recruitment_method[recruitment_method$Date.start %in% "18-Jul", "Date.start"] <- "2018-07-18"
recruitment_method[recruitment_method$Date.start %in% c("Novembre 2018 "), "Date.start"] <- "2018-11-01"




# Change source according to product

# recruitment_method[recruitment_method$Product %in% c( "TV segment", "Radio interview"), "Source"] <- "Media"

# recruitment_method[recruitment_method$Product %in% c("News article"), "Source"] <- "Smaller_newspapers"


# Combined Media and smaller newspaper articles


recruitment_method[recruitment_method$Product %in% c("TV segment", "Radio interview", "News article"), "Source"] <- "Other_Media"



recruitment_method[recruitment_method$Channel %in% c( "La Presse", "Le Devoir"), "Source"] <- "Big_newspapers"



#[recruitment_method$Channel %in% c( "La Presse"), "Source"] <- "La_Presse"

#recruitment_method[recruitment_method$Channel %in% c( "Le Devoir"), "Source"] <- "Le_Devoir"

#recruitment_method[recruitment_method$Channel %in% c( "CBC news"), "Source"] <- "CBC_news"
#recruitment_method[recruitment_method$Channel %in% c( "The Gazette"), "Source"] <- "The_Gazette"



# Recode Social media
recruitment_method[recruitment_method$Paid_non.paid %in% "paid" & recruitment_method$Source %in% "Social_Media","Source" ] <- "Social_Media_Boosted"



# Recode Partner newsletter

recruitment_method[recruitment_method$Product %in% c("Partner newsletter" , "Partner newspaper ad") & recruitment_method$Source %in% "Other", "Source" ] <- "Partners"





# remove Xs variables
recruitment_method$X <- NULL
recruitment_method$X.1 <- NULL
recruitment_method$X.2 <- NULL
recruitment_method$X.3 <- NULL
recruitment_method$X.4 <- NULL


# Add snowball campaign .......... NEEDS work to fill in all the fields information if we will need it. Check with ZOE 
recruitment_Snowball <- c(Channel = "Snowball_Campaign" , Source = "Snowball_Campaign", Product = "Snowball_Campaign", 
                          Date.start = "2018-11-06", Date.end = "2018-11-06", Language = "Bilingual", Paid_non.paid = "non-paid", 
                          Geography = "All_study" , Description = NA,  Visual = NA, PRINT.Costs = NA, LIST.Costs = NA, 
                          SEND.Costs = NA, TOTAL.Costs = NA, Reach = NA, Clicks.on.link = NA, Cost.per.click = NA) 

recruitment_method <- rbind(recruitment_method, recruitment_Snowball)
recruitment_method <- recruitment_method[with(recruitment_method, order(Date.start, Source)), ]
recruitment_method[with(recruitment_method, order(Date.start, Source)), ]
rm(recruitment_Snowball)





# Create subset
# =============
recruitment_hear <- recruitment_clean[recruitment_clean$City == "Montreal", c( "interact_id", "treksoft_id", "date_of_survey",  
                                                                               "hear",  "hear_txt",  "hear_clean", 
                                                                              "hear_clean_4Cat", "hear_clean_5Cat", "hear_clean_NCat",
                                                                               "Age", "age", "date_Health_survey", "Completed_HSurvey") ]  
recruitment_hear <- recruitment_hear[order(recruitment_hear$date_of_survey), ]














test <-  recruitment_clean[ , c("interact_id", "hear_clean", "hear",  "hear_clean_skt", "City", "hear_clean_5Cat", "hear_txt", "hear_clean_skt", "hear_clean_NCat")]



