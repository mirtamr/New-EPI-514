#############################################################################
# NAME: DATA MANAGEMENT 
rm(list=ls())

#BRFSS <- read.csv("/Users/mirtamaravilla/Documents/UW Courses/Spring Qtr 2024/EPI 514 /EPI 514 Research/BRFSS") 
BRFSS <- read.csv("/Users/samanthagarciaperez/Desktop/EPI 514/R/BRFSS")
#setwd("/Users/mirtamaravilla/Documents/UW Courses/Spring Qtr 2024/EPI 514 /EPI 514 Research")
getwd()

library(tidyverse)
library(haven)
library(foreign)


View(BRFSS)
names(BRFSS)

grep("sex", names(BRFSS), value=TRUE) #check for different sex variables

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


names(BRFSS)

colnames(BRFSS) <- trimws(colnames(BRFSS)) #removing any blank spaces in column names


#cutting down data set to what we need
BRFSS <- BRFSS[,c("state", "sex","HlthDiscrim", "CervScrnEver", "CervScrnHPV", 
                  "CervScrnPAP", "HadHyst", "RaceOthers", "income", "employ",
                  "insurance", "edu", "age", "GenHlth")]

names(BRFSS) #check names included are correct


tab1 <- table(BRFSS$HlthDiscrim, BRFSS$CervScrnEver, useNA="ifany")
(total_tab <- addmargins(tab1, FUN = list(Total = sum)))
nrow(BRFSS)


#################### EXPOSURE (Mirta) ###################
#checking race variables
table(BRFSS$HlthDiscrim)

#Setting values to missing (highlighted red in data dictionary)
BRFSS$HlthDiscrim[BRFSS$HlthDiscrim==5] <- NA 
BRFSS$HlthDiscrim[BRFSS$HlthDiscrim==6] <- NA 
BRFSS$HlthDiscrim[BRFSS$HlthDiscrim==7] <- NA 
BRFSS$HlthDiscrim[BRFSS$HlthDiscrim==9] <- NA
table(BRFSS$HlthDiscrim)

#converting into labeled factor variables 
BRFSS$HlthDiscrim.f <- factor(BRFSS$HlthDiscrim,
                           levels = 1:4, 
                           labels = c("Worse than others", "same as others", 
                                      "better than others", "worse than some,better than others"))
#checking 
table(BRFSS$HlthDiscrim.f)


#converting to binary Yes and No 
BRFSS$Hlthdiscrim_bin.f[BRFSS$HlthDiscrim==1] <-1 #adding worse than others for "Yes" 
BRFSS$Hlthdiscrim_bin.f[BRFSS$HlthDiscrim==4] <-1 # adding worse than some, better than others for "Yes" 

BRFSS$Hlthdiscrim_bin.f[BRFSS$HlthDiscrim==2] <-2 #same as others for NO 
BRFSS$Hlthdiscrim_bin.f[BRFSS$HlthDiscrim==3] <-2 #better than others for NO

#converting into labeled factor variables 
BRFSS$Hlthdiscrim_bin.f <- factor(BRFSS$Hlthdiscrim_bin.f,
                              levels = 1:2, 
                              labels = c("Yes", "No"))
#checking 
table(BRFSS$Hlthdiscrim_bin.f)

BRFSS <- BRFSS %>%
  filter(!is.na(HlthDiscrim)) # excluing those with no exposure 

nrow(BRFSS)



#################### OUTCOMES (Aileen) ########################

BRFSS <- BRFSS %>%
  filter(sex == 2) #excluding males assigned sex at birth 

nrow(BRFSS)

#check cervical cancer variables

summary(BRFSS$CervScrnEver)
summary(BRFSS$CervScrnHPV)
summary(BRFSS$CervScrnPAP)
summary(BRFSS$HadHyst)

###Cervical Screen ever variable


BRFSS$CervScrnEver[BRFSS$CervScrnEver==7] <- NA #set missing values
BRFSS$CervScrnEver[BRFSS$CervScrnEver==9] <- NA #set missing values

summary(BRFSS$CervScrnEver) #check

#factoring and converting to labeled factor 
BRFSS$CervScrnEver.f <- factor(BRFSS$CervScrnEver,
                            levels = 1:2,
                            labels = c("Yes", "No"))
table(BRFSS$CervScrnEver.f) #check

BRFSS <- BRFSS %>%
  filter(!is.na(CervScrnEver)) 
nrow(BRFSS)


###Cervical Screen with HPV test



BRFSS$CervScrnHPV[BRFSS$CervScrnHPV==7] <- NA #set missing values
BRFSS$CervScrnHPV[BRFSS$CervScrnHPV==9] <- NA #set missing values

summary(BRFSS$CervScrnHPV) #check


#factoring and converting to labeled factor 
BRFSS$CervScrnHPV.f <- factor(BRFSS$CervScrnHPV,
                               levels = 1:2,
                               labels = c("Yes", "No"))
table(BRFSS$CervScrnHPV.f) #check

###Cervical Screen with PAP test

BRFSS$CervScrnPAP[BRFSS$CervScrnPAP==2] <- 0 #assigning 0 for no


BRFSS$CervScrnPAP[BRFSS$CervScrnPAP==7] <- NA #set missing values
BRFSS$CervScrnPAP[BRFSS$CervScrnPAP==9] <- NA #set missing values

summary(BRFSS$CervScrnPAP) #check

#factoring and converting to labeled factor 
BRFSS$CervScrnPAP.f <- factor(BRFSS$CervScrnPAP,
                              levels = 0:1,
                              labels = c("No", "Yes"))
table(BRFSS$CervScrnPAP.f) #check

###Hist of hysterectomy

BRFSS$HadHyst[BRFSS$HadHyst==2] <- 0 #assigning 0 for no


BRFSS$HadHyst[BRFSS$HadHyst==7] <- NA #set missing values
BRFSS$HadHyst[BRFSS$HadHyst==9] <- NA #set missing values

summary(BRFSS$HadHyst) #check

#factoring and converting to labeled factor 
BRFSS$HadHyst.f <- factor(BRFSS$HadHyst,
                              levels = 0:1,
                              labels = c("No", "Yes"))
table(BRFSS$HadHyst.f) #check

########################## COVARIATES (Mirta/Samantha) ####################

#INSURANCE 
table(BRFSS$insurance)

#setting values to missing (red highlight)
BRFSS$insurance[BRFSS$insurance==77] <- NA
BRFSS$insurance[BRFSS$insurance==99] <- NA

#creating categories to prepare for factor variable

BRFSS$insurance.f[BRFSS$insurance<=10] <- 1
BRFSS$insurance.f[BRFSS$insurance==88] <- 2


#factoring and converting to labeled factor 
BRFSS$insurance.f <- factor(BRFSS$insurance.f,
                      levels = 1:2,
                      labels = c("Yes", "No"))
#checking
table(BRFSS$insurance.f)


#AGE 

table(BRFSS$age)

#turning values >65 and <25 to missing 
BRFSS$age[BRFSS$age >65] <- NA
BRFSS$age[BRFSS$age <25] <- NA

BRFSS <- BRFSS %>%
  filter(!is.na(age)) # exlcuding age NA 

nrow(BRFSS)

#creating categories to prepare for factor variable
BRFSS$age.f[BRFSS$age>=25 & BRFSS$age<=29] <- 1 
BRFSS$age.f[BRFSS$age>=30 & BRFSS$age<=39] <- 2 
BRFSS$age.f[BRFSS$age>=40 & BRFSS$age<=49] <- 3 
BRFSS$age.f[BRFSS$age>=50 & BRFSS$age<=59] <- 4
BRFSS$age.f[BRFSS$age>=60 & BRFSS$age<=65] <- 5 

#factoring and converting to labeled factor 
BRFSS$age.f <- factor(BRFSS$age.f,
                            levels = 1:5,
                            labels = c("25-29", "30-39", "40-49", "50-59", "60-65"))

#checking
table(BRFSS$age.f)


#EDUCATION
table(BRFSS$edu)

#converting "refused" values to missing 
BRFSS$edu[BRFSS$edu==9] <- NA

#creating categories to prepare for factor variable
BRFSS$edu.f[BRFSS$edu<=4] <-1
BRFSS$edu.f[BRFSS$edu==5] <-2
BRFSS$edu.f[BRFSS$edu==6] <-3

#factoring and converting to labeled factor 
BRFSS$edu.f <- factor(BRFSS$edu.f,
                      levels = 1:3,
                      labels = c("≤ high school", "some college", "college"))

#checking 
table(BRFSS$edu.f)


#GENHLTH
table(BRFSS$GenHlth)

#converting "refused" to missing 
BRFSS$GenHlth[BRFSS$GenHlth==9] <- NA
BRFSS$GenHlth[BRFSS$GenHlth==7] <- NA

#creating categories to prepare for factor variable
BRFSS$GenHlth.f[BRFSS$GenHlth>=1 & BRFSS$GenHlth<=3] <- 1 
BRFSS$GenHlth.f[BRFSS$GenHlth>=4 & BRFSS$GenHlth<=5] <- 2 

#factoring and converting to labeled factor 
BRFSS$GenHlth.f <- factor(BRFSS$GenHlth.f,
                      levels = 1:2,
                      labels = c("excellent-good", "fair-poor"))


#checking
table(BRFSS$GenHlth.f)

# RACE/ETHNCITY
BRFSS$RaceOthers[BRFSS$RaceOthers == "77"] <- NA 
BRFSS$RaceOthers[BRFSS$RaceOthers == "99"] <- NA 
BRFSS$RaceOthers[BRFSS$RaceOthers == ""] <- NA 

BRFSS$RaceOthers.f <- factor(BRFSS$RaceOthers, 
                             levels = 1:8, 
                             labels = c("White", "Black or African American", 
                                        "Hispanic or Latino", "Asian", 
                                        "Native Hawaiian or other Pacific Islander",
                                        "American Indian or Alaska Native",
                                        "Mixed Race", "Some other group"))

table(BRFSS$state)

#JIM CROW 
BRFSS$jimcrow <- ifelse(BRFSS$state %in% c("1", "4", "5", "10", "12", "13", 
                                           "20", "21", "22", "24", "28", "29", 
                                           "35", "37", "40", "45", "47", "48", 
                                           "51", "54", "56", "11"), 1,0)

table(BRFSS$jimcrow)


#Annual Income 

BRFSS$income[BRFSS$income >= "1" & BRFSS$income<="6"] <- 1
BRFSS$income[BRFSS$income == "7"] <- 2
BRFSS$income[BRFSS$income == "8"] <- 2 
BRFSS$income[BRFSS$income == "9"] <- 2
BRFSS$income[BRFSS$income == "10"] <- 2
BRFSS$income[BRFSS$income == "11"] <- 2

BRFSS$income[BRFSS$income == "77"] <- NA
BRFSS$income[BRFSS$income == "99"] <- NA



BRFSS$income.f <- factor(BRFSS$income, 
                         levels = 1:2, 
                         labels = c("<50,000", "≥50,000"))
table(BRFSS$income.f)




#employement 

BRFSS$employ.f[BRFSS$employ == 1] <- 1

BRFSS$employ.f[BRFSS$employ == 2] <- 2 
BRFSS$employ.f[BRFSS$employ == 3] <- 2
BRFSS$employ.f[BRFSS$employ == 4] <- 2
BRFSS$employ.f[BRFSS$employ == 5] <- 2
BRFSS$employ.f[BRFSS$employ == 6] <- 2
BRFSS$employ.f[BRFSS$employ == 7] <- 2
BRFSS$employ.f[BRFSS$employ == 8] <- 2

BRFSS$employ.f[BRFSS$employ == 9] <- NA 



BRFSS$employ.f <- factor(BRFSS$employ, 
                         levels=c(1,2), 
                         labels=c("Employed", "Not Employed"))

table(BRFSS$employ.f, useNA="ifany")


#saving as csv file 

write.csv(BRFSS, "cleanbrfss.csv")


## exclusion critera 

BRFSS <- BRFSS %>%
  filter(sex == 2 & !is.na(HlthDiscrim) & !is.na(age))

nrow(BRFSS)

## For table1 



