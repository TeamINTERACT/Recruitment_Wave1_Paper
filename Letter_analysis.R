


library(tidyverse)
library(dplyr)
library (tidyr)
library(plyr)

library(table1, quietly = TRUE)





recruitment_Heard_letter <- recruitment_clean[((recruitment_clean$hear_clean_NCat %in% "Letter") & (recruitment_clean$City %in% "Montreal")), ]   

recruitment_Heard_letter_GRPS <- join(recruitment_Heard_letter, Letter_Sampling_groups, by = "residence_cp", type='left', match='all' )




table1:: table1 (~  Age | RecruitGroup , data = recruitment_Heard_letter_GRPS, overall="Total", topclass="Rtable1-zebra")



table1:: table1 (~  Age | RecruitGroup*Completed_HSurvey , data = recruitment_Heard_letter_GRPS, overall="Total", topclass="Rtable1-zebra")


count(data = recruitment_Heard_letter_GRPS|RecruitGroup)

kruskal.test(Age ~ RecruitGroup, data = recruitment_Heard_letter_GRPS)




library(dplyr)

recruitment_Heard_letter_GRPS$RecruitGroup <- ordered(recruitment_Heard_letter_GRPS$RecruitGroup,
                         levels = c("Group 1", "Group 2", "Group 3"))


recruitment_Heard_letter_GRPS$RecruitGroup <- as.character(recruitment_Heard_letter_GRPS$RecruitGroup)


recruitment_Heard_letter_GRPS %>% 
  
   group_by(RecruitGroup) %>% 
  
   summarise(
    n(),
    mean = mean(age, na.rm = TRUE),
    sd = sd(age, na.rm = TRUE),
    median = median(age, na.rm = TRUE), 
    IQR = IQR(age, na.rm = TRUE)
  )

