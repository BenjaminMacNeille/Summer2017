library("dplyr")
library("haven")
library("medicalrisk")
library("purrr")
setwd("~/MEPS")

mc14 <- read_dta("2014_MC.dta")
crohns_marker <- mc14 %>%
  mutate(crohnsICD = ifelse(ICD9CODX == "555", 1, 0)) %>%
  group_by(DUPERSID) %>%
  summarise(crohns = max(crohnsICD)) %>%
  arrange(crohns) 

pop14 <- read_dta("2014_Population.dta")

base <- select(pop14, SEX, AGE14X, DUPERSID, RACEV1X, MARRY14X, 
               EDRECODE,REGION14, INSCOV14)


base$SEX <- ifelse(base$SEX !=1, 0, 1)

base <- left_join(base, crohns_marker, by = "DUPERSID")
base$crohns[is.na(base$crohns)] <- 0

comorbid <- mc14 %>%
  select(DUPERSID, ICD9CODX) %>%
  group_by(DUPERSID) %>%
  summarise(co_count = n())


base <- left_join(base, comorbid, by = "DUPERSID")
base$co_count[is.na(base$co_count)] <- 0

office14 <- read_dta("2014_Office.dta")
officeXPdf <- office14 %>%
  group_by(DUPERSID) %>%
  summarise(officeXP = sum(OBXP14X))

base <- left_join(base, officeXPdf, by = "DUPERSID")
base$officeXP[is.na(base$officeXP)] <- 0

op14 <- read_dta("2014_Outpatient.dta")
opXPdf <- op14 %>%
  group_by(DUPERSID) %>%
  summarise(opXP = sum(OPXP14X))

base <- left_join(base, opXPdf, by = "DUPERSID")
base$opXP[is.na(base$opXP)] <- 0

base <- mutate(base, totalXP = officeXP + opXP)

glimpse(base)
#split
set.seed(2)
train <- sample(1:nrow(base), nrow(base)*.7)



#Moving forward in time to GAM
library("gam")
crohns_gam1 <- gam(totalXP~. -DUPERSID -officeXP -opXP, data = base, subset = train)
par(mfrow=c(3,4))
plot(crohns_gam1, se=TRUE, col="blue")
plot.gam(crohns_gam1, se=TRUE, col="red")
summary(crohns_gam1)



crohns_gam2 <- gam(totalXP~s(AGE14X, 3) + s(co_count, 3) + crohns +  SEX + RACEV1X + MARRY14X+ 
                     EDRECODE+REGION14+ INSCOV14, data = base, subset = train)
par(mfrow=c(1,4))
plot(crohns_gam2, se=TRUE, col="blue")
plot.gam(crohns_gam2, se=TRUE, col="red")

max(base$co_count)


#test
base.train <- base[train,]
table(base.train$crohns)
summary(crohns_gam)

base.test <- base[-train, ]
base.test <- select(base.test, -c(DUPERSID,officeXP,opXP))
View(base.test)
crohns_predict <- predict.gam(crohns_gam, newdata = base.test)


