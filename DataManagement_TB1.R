#############################################################################
# NAME: DATA MANAGEMENT FOR TABLE #1 

BRFSS <- read.csv("/Users/mirtamaravilla/Documents/UW Courses/Spring Qtr 2024/EPI 514 /EPI 514 Research/BRFSS")


# set libraries 
library(tidyverse)
library(haven)
library(foreign)
library(dplyr)




#renaming variables working with 
names(BRFSS)[names(BRFSS) == "X_STATE"] <- "state"
names(BRFSS)[names(BRFSS) == "BIRTHSEX"] <- "sex"
names(BRFSS)[names(BRFSS) == "RRHCARE4"] <- "HlthDiscrim"
names(BRFSS)[names(BRFSS) == "CERVSCRN"] <- "CervScrnEver"
names(BRFSS)[names(BRFSS) == "CRVCLHPV"] <- "CervScrnHPV"
names(BRFSS)[names(BRFSS) == "CRVCLPAP"] <- "CervScrnPAP"
names(BRFSS)[names(BRFSS) == "HADHYST2"] <- "HadHyst"
names(BRFSS)[names(BRFSS) == "RRCLASS3"] <- "RaceOthers"
names(BRFSS)[names(BRFSS) == "INCOME3"] <- "income"
names(BRFSS)[names(BRFSS) == "EMPLOY1"] <- "employ"
names(BRFSS)[names(BRFSS) == "PRIMINSR"] <- "insurance"
names(BRFSS)[names(BRFSS) == "EDUCA"] <- "edu"
names(BRFSS)[names(BRFSS) == "X_AGE80"] <- "age"
names(BRFSS)[names(BRFSS) == "GENHLTH"] <- "GenHlth"

colnames(BRFSS) <- trimws(colnames(BRFSS))

#cutting down data set to what we need
BRFSS <- BRFSS[,c("state", "sex","HlthDiscrim", "CervScrnEver", "CervScrnHPV", 
                  "CervScrnPAP", "HadHyst", "RaceOthers", "income", "employ",
                  "insurance", "edu", "age", "GenHlth")]

#filtering down to only those with female sex at birth and respondents to reaction to race 




names(BRFSS) #check names included are correct


#################### EXPOSURE ###################

#Setting values to missing or 5 
BRFSS$HlthDiscrim[BRFSS$HlthDiscrim==5] <- "5"
BRFSS$HlthDiscrim[BRFSS$HlthDiscrim==6] <- "5"
BRFSS$HlthDiscrim[BRFSS$HlthDiscrim==7] <- "5"
BRFSS$HlthDiscrim[BRFSS$HlthDiscrim==9] <- "5"
table(BRFSS$HlthDiscrim)

BRFSS$HlthDiscrim.f <- factor(BRFSS$HlthDiscrim,
                              levels = 1:5, 
                              labels = c("Worse than others", "Same as others", 
                                         "Better than others", "Worse than some,better than others", "Missing"))
table(BRFSS$HlthDiscrim.f)

#################### OUTCOME ###################

BRFSS$CervScrnEver[BRFSS$CervScrnEver==2] <- 0 #assigning 0 for no


BRFSS$CervScrnEver[BRFSS$CervScrnEver==7] <- NA #set missing values
BRFSS$CervScrnEver[BRFSS$CervScrnEver==9] <- NA #set missing values



BRFSS$CervScrnEver.f <- factor(BRFSS$CervScrnEver,
                               levels = 0:1,
                               labels = c("No", "Yes"))

table(BRFSS$CervScrnEver.f) #check


#################### COVAR ###################


# RACE/ETHNCITY
BRFSS$RaceOthers[BRFSS$RaceOthers == "77"] <- 9 
BRFSS$RaceOthers[BRFSS$RaceOthers == "99"] <- 9
BRFSS$RaceOthers[BRFSS$RaceOthers == ""] <- 9

BRFSS$RaceOthers.f <- factor(BRFSS$RaceOthers, 
                             levels = 1:9, 
                             labels = c("White", "Black or African American", 
                                        "Hispanic or Latino", "Asian", 
                                        "Native Hawaiian or other Pacific Islander",
                                        "American Indian or Alaska Native",
                                        "Mixed Race", "Some other group", "Missing"))
table(BRFSS$RaceOthers.f)

###Income 

table(BRFSS$income)
BRFSS$income[BRFSS$income == "77"] <- 8
BRFSS$income[BRFSS$income == "99"] <- 8

BRFSS$income[BRFSS$income >= "1" & BRFSS$income<="6"] <- 0
BRFSS$income[BRFSS$income >= "7" & BRFSS$income<="11"] <- 1





BRFSS$income.f <- factor(BRFSS$income, 
                         levels = 1:8, 
                         labels = c("<10,000", "10,000 - <15,000", "15,000 - <20,000", 
                                    "20,000 - < 25,000", "25,000 - < 35,000", 
                                    "35,000 - < 50,000", ">50,000", "Missing"))
table(BRFSS$income.f)

## Employement 


BRFSS$employ[BRFSS$employ == 2] <- 0 
BRFSS$employ[BRFSS$employ == 3] <- 0
BRFSS$employ[BRFSS$employ == 4] <- 0 
BRFSS$employ[BRFSS$employ == 5] <- 0 
BRFSS$employ[BRFSS$employ == 6] <- 0 
BRFSS$employ[BRFSS$employ == 7] <- 0 
BRFSS$employ[BRFSS$employ == 8] <- 0 
BRFSS$employ[BRFSS$employ == 9] <- 2 

table(BRFSS$employ, useNA="ifany")


BRFSS$employ.f <- factor(BRFSS$employ, 
                         levels=c(0:2), 
                         labels=c("Employed", "Not Employed", "Missing"))

table(BRFSS$employ.f, useNA="ifany")


## Jim Crow 

BRFSS$jimcrow <- ifelse(BRFSS$state %in% c("1", "4", "5", "10", "12", "13", 
                                           "20", "21", "22", "24", "28", "29", 
                                           "35", "37", "40", "45", "47", "48", 
                                           "51", "54", "56", "11"), 1,0)

table(BRFSS$jimcrow, useNA="ifany")

BRFSS$jimcrow.f <- factor(BRFSS$jimcrow, 
                          levels=0:1, 
                          labels = c("No", "Yes"))

table(BRFSS$jimcrow.f, useNA="ifany")

## Insurance 

BRFSS$insurance[BRFSS$insurance<=10] <- 1
BRFSS$insurance[BRFSS$insurance==88] <- 0

BRFSS$insurance[BRFSS$insurance==77] <- 2
BRFSS$insurance[BRFSS$insurance==99] <- 2

#creating categories to prepare for factor variable

#factoring and converting to labeled factor 
BRFSS$insurance.f <- factor(BRFSS$insurance,
                            levels = 0:2,
                            labels = c("No", "Yes", "Missing"))
#checking
table(BRFSS$insurance.f)

## Education

table(BRFSS$edu)

#creating categories to prepare for factor variable
BRFSS$edu.f[BRFSS$edu<=4] <-1
BRFSS$edu.f[BRFSS$edu==5] <-2
BRFSS$edu.f[BRFSS$edu==6] <-3

#converting "refused" values to missing 
BRFSS$edu.f[BRFSS$edu==9] <- 4

#factoring and converting to labeled factor 
BRFSS$edu.f <- factor(BRFSS$edu.f,
                      levels = 1:4,
                      labels = c("≤ High school", "Some college", "College", "Missing"))
table(BRFSS$edu.f)


## AGE 


table(BRFSS$age)

#creating categories to prepare for factor variable
BRFSS$age.f[BRFSS$age>=25 & BRFSS$age<=29] <- 1 
BRFSS$age.f[BRFSS$age>=30 & BRFSS$age<=39] <- 2 
BRFSS$age.f[BRFSS$age>=40 & BRFSS$age<=49] <- 3 
BRFSS$age.f[BRFSS$age>=50 & BRFSS$age<=59] <- 4
BRFSS$age.f[BRFSS$age>=60 & BRFSS$age<=65] <- 5 

#turning values >65 and <25 to missing 
BRFSS$age.f[BRFSS$age >65] <- NA
BRFSS$age.f[BRFSS$age <25] <- NA

#factoring and converting to labeled factor 
BRFSS$age.f <- factor(BRFSS$age.f,
                      levels = 1:5,
                      labels = c("25-29", "30-39", "40-49", "50-59", "60-65"))

#checking
table(BRFSS$age.f)




#GENHLTH
table(BRFSS$GenHlth)

#creating categories to prepare for factor variable
BRFSS$GenHlth.f[BRFSS$GenHlth>=1 & BRFSS$GenHlth<=3] <- 1 
BRFSS$GenHlth.f[BRFSS$GenHlth>=4 & BRFSS$GenHlth<=5] <- 2 
BRFSS$GenHlth.f[BRFSS$GenHlth==9] <- 3
BRFSS$GenHlth.f[BRFSS$GenHlth==7] <- 3


#factoring and converting to labeled factor 
BRFSS$GenHlth.f <- factor(BRFSS$GenHlth.f,
                          levels = 1:3,
                          labels = c("Excellent-Good", "Fair-Poor", "Missing"))

table(BRFSS$GenHlth.f)


## exclusion critera 

BRFSS <- BRFSS %>%
  filter(sex == 2 & !is.na(HlthDiscrim) & !is.na(age))









