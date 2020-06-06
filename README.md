# Title: Recruiting Participants for Population Health Intervention Research: Effectiveness and Costs of Recruitment Methods in the INTERACT study

# Authors: 
Wasfi R, Poirier Stephens Z, Sones M, Laberee K, Gough M,  Pugh C, Fuller D, Winters M  &  Kestens Y

# Submitted to: 
Journal of Medical Internet Research 

# Abstract 
## Background:
Public health research studies often rely on population-based participation and draw on various recruitment methods to establish samples. Increasingly researchers are turning to web-based recruitment tools. However, few studies detail traditional and online recruitment efforts in terms of costs and potential biases.
Objective:
Our objective is to report on and evaluate the cost and time effectiveness and sociodemographic representation of diverse recruitment methods used to enroll participants in the INTErventions, Research, and Action in Cities Team (INTERACT) study, a cohort study conducted in 4 Canadian cities. 

## Methods:
Over 2017 and 2018 in Victoria, Vancouver, Saskatoon, and Montreal, INTERACT used the following recruitment methods: mailed letters, social media (including sponsored Facebook ads), news media, partner communications, snowball recruitment, in-person recruitment, and posters. Participation in the study involved answering online questionnaires (at minimum), activating a smartphone app to share sensor data, and wearing a device for mobility and physical activity monitoring. We describe socio-demographic characteristics by recruitment method and analyze performance indicators, including cost, completion rate, and time effectiveness. Effectiveness included calculating cost per completer (i.e., participant who completed at least one questionnaire), completion rate of a health questionnaire, and delay between completion of eligibility and health questionnaires. Cost included producing materials (i.e. printing costs); transmitting recruitment messages (i.e. mailing list rental, postage, sponsored Facebook posts charges); and staff time. In Montreal, the largest INTERACT sample, we model the number of daily recruits through generalized linear models accounting for distributed lagged effects of recruitment campaigns.
## Results: 
Overall, 2,027 participants were recruited and completed at least one questionnaire: 281 in Victoria, 318 in Vancouver, 315 in Saskatoon and 1158 in Montreal. In all cities, the majority of participants chose to fully participate (questionnaire, app, and device). The costs associated with one completed participant varied across recruitment methods and by city. Facebook ads generated the most recruits (n=687), at an average cost of $15.04 (including staff time). Mailed letters were the most costly, at $108.30, but served to reach older participants. All methods resulted in a gender imbalance, with women participating more, specifically with social media. Partner newsletters resulted in participation of younger adults and were cost-efficient ($5.20/completer). Generalized linear model for daily Montreal recruitment identified 2-day lag effects on most recruitment methods, except for the snowball campaign (4 days), letters (15 days), and reminder cards (5 days). 
## Conclusions: 
This work presents comprehensive data on costs, effectiveness, and bias of population recruitment in a cohort study in four Canadian cities. More comprehensive documentation and reporting of recruitment efforts across studies is needed to improve our capacity to conduct inclusive intervention research. 

## R script code
R code for the INTERACT national recruitment analyses
The final scripts used in the manuscript are uploaded : https://github.com/TeamINTERACT/Recruitment_Wave1_Paper. 

First run main script : data_preparation_MAIN.. All the other scripts are called within this master file.

The mailed letters analysis script and descriptives are  stand alone scripts, you will need to run sepertaly. They are not called from the master file.

We cannot make the initial dataset public as it has senstive information. However, the final dataset that is created to model the number of recruited participants in Montreal is uploaded.  Special requests for full datasets can be made with the arrangements with INTERACT principle investigators : Prof. Yan Kestens, Prof. Meghan Winters and Prof. Daniel Fuller. For more information please consult INTERACT website: https://teaminteract.ca/  

Original datasets can only been acccessed through secured servers for data confidentiality. 

## Data
Data provided for public is only data that is used to model daily number of participants recruited by recruitment method in Montreal. Data doesnot contain any personal identifiers
Data file: 
- Methods_AND_Recruited <- read.csv("D:/INTERACT/Recruitment Analysis/data/Methods_AND_Recruited2.csv")
The corresponding R script file is: 
- Testing_possible_lag_combinations_in_regressions_loop_no_logs




