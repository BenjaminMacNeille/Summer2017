library("dplyr")
library("haven") #package to read stata files
library("icd")
setwd("~/MEPS")

#import CY2014 Medical Conditions file 
mc14 <- read_dta("2014_MC.dta") 

#Create flag for person IDs (i.e DUPERSID) with Crohns Disease
crohns_marker <- mc14 %>% #pipe operator
  mutate(crohnsICD = ifelse(ICD9CODX == "555", 1, 0)) %>% #555 is the diagnosis code for Crohns
  group_by(DUPERSID) %>% #group_by precedes and informs the summarise function
  summarise(crohns = max(crohnsICD)) %>% #DUPERSID can have more than one row (i.e. diagnosis). 
                                        ##We only care about if they have crohns or not, so a maximum (1) will suffice
  arrange(crohns) 

#import CY2014 Population file
pop14 <- read_dta("2014_Population.dta")

#keep only subsect of variable of interest to be used for this analysis
base <- select(pop14, SEX, AGE14X, DUPERSID, RACEV1X, MARRY14X, 
               EDRECODE,REGION14, INSCOV14)

#In SEX variable, Female originally coded as 2. Female now coded as 0
base$SEX <- ifelse(base$SEX !=1, 0, 1) 

#Want to merge our base file with crohns_marker to flag DUPERSID with Crohns
base <- left_join(base, crohns_marker, by = "DUPERSID")
#However, some DUPERSID show "NA" because they were not in medical conditions file.
##Therefore, these are DUPERSIDs without any diagnoses. Thus, crohns marker equals 0
base$crohns[is.na(base$crohns)] <- 0

#Previous count of chronic diseases. Counts crohns as well, so not technically cobmorbidity of crohns, but total conditions
##I want to verify the co_count somepoint because I think the results are way too high. Make sure we aren't double counting
comorbid <- mc14 %>%
  select(DUPERSID, ICD9CODX) %>%
  group_by(DUPERSID) %>%
  summarise(co_count = n())

base <- left_join(base, comorbid, by = "DUPERSID") #merge to base
base$co_count[is.na(base$co_count)] <- 0 #again, some may be "NA" if not in mc14. These cases are converted to 0

#New Charlson Comorbidity Index scoring from 'icd' package
mc14.charlson <- mc14 %>%
  rename(icd9cm = ICD9CODX, id = DUPERSID) %>%
  select(id, icd9cm)

charlson <- icd_charlson(mc14.charlson, short_code = TRUE, return_df = TRUE)
charlson <- rename(charlson, DUPERSID = id) #convert back to DUPERSID so we can merge with our base file

base <- left_join(base, charlson, by = "DUPERSID")
base$Charlson[is.na(base$Charlson)] <- 0 #again, some may be "NA" if not in mc14. These cases are converted to 0

#import CY2014 office-based file
office14 <- read_dta("2014_Office.dta")
#Sum total office-based expenditures by DUPERSID
officeXPdf <- office14 %>%
  group_by(DUPERSID) %>%
  summarise(officeXP = sum(OBXP14X)) 
#merge total  office-based expenditures with base file
base <- left_join(base, officeXPdf, by = "DUPERSID")
base$officeXP[is.na(base$officeXP)] <- 0

#import CY2014 outpatient file
op14 <- read_dta("2014_Outpatient.dta")
#Sum total outpatient expenditures by DUPERSID
opXPdf <- op14 %>%
  group_by(DUPERSID) %>%
  summarise(opXP = sum(OPXP14X))
#merge total  outpatient expenditures with base file
base <- left_join(base, opXPdf, by = "DUPERSID")
base$opXP[is.na(base$opXP)] <- 0

#create new variable in base file (i.e. totalXP) to sum total office + outpatient expenditures
base <- mutate(base, totalXP = officeXP + opXP)

###READY TO GO GARDENING for CARETS!!!!
