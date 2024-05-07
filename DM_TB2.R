#############################################################################
# NAME: DATA MANAGEMENT

#BRFSSclean <- read.csv("/Users/mirtamaravilla/Documents/UW Courses/Spring Qtr 2024/EPI 514 /EPI 514 Research/cleanbrfss.csv") 
BRFSS<- read.csv("/Users/aileenandrade/Desktop/Spring Quarter/Epi 514/BRFSS project/Data/cleanbrfss.csv") 
View(BRFSSclean)
#libraries

library(epiR)

#Prevalence of Cervical cancer 

prevtable <- table(BRFSS$Hlthdiscrim_bin.f, BRFSS$CervScrnEver.f, dnn=c("Discrimination","Cervical Cancer Screening"))

prop.table(prevtable)

table(BRFSS$Hlthdiscrim_bin.f, BRFSS$CervScrnEver.f)

#Unadjusted PR (NO COVARIATES)

#tables for cervical cancer screening 
unadj <- with(BRFSS, table(Hlthdiscrim_bin.f, CervScrnEver.f))

unadj

epi.2by2(unadj, method = "cross.sectional")

######################### Adjusted PRs ########################################


#Race

racerrtab <-table(BRFSS$Hlthdiscrim_bin.f, BRFSS$CervScrnEver.f, BRFSS$RaceOthers.f, deparse.level = 2)
(racestrat <- epi.2by2(dat=racerrtab, method = "cross.sectional"))
racestrat$massoc.detail$PR.strata.wald


#Income 

(incomerrtab <-table(BRFSS$Hlthdiscrim_bin.f, BRFSS$CervScrnEver.f, BRFSS$income.f, deparse.level = 2))
(incomestrat <- epi.2by2(dat=incomerrtab, method = "cross.sectional"))
incomestrat$massoc.detail$PR.strata.wald

#Employment 

employrrtab <-table(BRFSS$Hlthdiscrim_bin.f, BRFSS$CervScrnEver.f, BRFSS$employ.f, deparse.level = 2)
(employstrat <- epi.2by2(dat=employrrtab, method = "cross.sectional"))
employstrat$massoc.detail$PR.strata.wald


#Health Insurance 

(coveragerrtab <-table(BRFSS$Hlthdiscrim_bin.f, BRFSS$CervScrnEver.f, BRFSS$insurance.f, deparse.level = 2))
(prop.table(coveragerrtab))
  
(coveragestrat <- epi.2by2(dat=coveragerrtab, method = "cross.sectional"))
coveragestrat$massoc.detail$PR.strata.wald

# Health Status 
    ##1, exposed 2, unexposed 

#2x2x2 table, health status 
(HSrrtab <- table(BRFSS$Hlthdiscrim_bin.f, BRFSS$CervScrnEver.f, BRFSS$GenHlth.f, deparse.level =2)) 
(HSstrat <- epi.2by2(dat = HSrrtab, method = "cross.sectional")) #0.95 M-H 
HSstrat$massoc.detail$PR.strata.wald #getting stratum specific estimates 



###Age 

#2x2xn table, age 

agerrtab <- table(BRFSS$Hlthdiscrim_bin.f, BRFSS$CervScrnEver.f, BRFSS$age.f, deparse.level =2)
(agestrat <- epi.2by2(dat=agerrtab, method = "cross.sectional"))
agestrat$massoc.detail$PR.strata.wald



#creating categories to prepare for factor variable
BRFSS$age_bin[BRFSS$age>=25 & BRFSS$age<=45] <- 1 
BRFSS$age_bin[BRFSS$age>45] <- 2 
 

#factoring and converting to labeled factor 
BRFSS$age_bin <- factor(BRFSS$age_bin,
                      levels = 1:2,
                      labels = c("<=45", ">45"))

#checking
table(BRFSS$age_bin)




####Education 

#2x2xn table, education
edurrtab <- table(BRFSS$Hlthdiscrim_bin.f, BRFSS$CervScrnEver.f, BRFSS$edu.f, deparse.level =2)
(edustrat <- epi.2by2(dat=edurrtab, method = "cross.sectional"))
edustrat$massoc.detail$PR.strata.wald


#making education into binay variable 

BRFSS$edu_bin[BRFSS$edu<=4] <-1
BRFSS$edu_bin[BRFSS$edu==5] <-2
BRFSS$edu_bin[BRFSS$edu==6] <-2

#factoring and converting to labeled factor 
BRFSS$edu_bin <- factor(BRFSS$edu_bin,
                      levels = 1:2,
                      labels = c("≤ high school", " =>college"))

#checking 
table(BRFSS$edu_bin, useNA = "ifany")



##Jim Crows 

#turning coding into 1,2 

BRFSS$jimcrow.f <- ifelse(BRFSS$state %in% c("1", "4", "5", "10", "12", "13", 
                                           "20", "21", "22", "24", "28", "29", 
                                           "35", "37", "40", "45", "47", "48", 
                                           "51", "54", "56", "11"), 1,2)

BRFSS$jimcrow.f <- factor(BRFSS$jimcrow.f, 
                          levels= 1:2, 
                          labels ="Yes", "No")

table(BRFSS$jimcrow.f)
 #2x2x2 table, jim crow laws 
jcrrtab <- table(BRFSS$Hlthdiscrim_bin.f, BRFSS$CervScrnEver.f, BRFSS$jimcrow.f, deparse.level =2)
(jcstrat <- epi.2by2(dat=jcrrtab, method = "cross.sectional"))
jcstrat$massoc.detail$PR.strata.wald





#adjustment for two or more confounders 

(mhstrat <- xtabs(~Hlthdiscrim_bin.f + CervScrnEver.f + income.f + employ.f + insurance.f + GenHlth.f + age_bin + edu_bin, data = BRFSS))

#array <- array(mhstrat,
#               dim = c(2,2,n), # this creates a 3 dimension array with n tables
#               list(exposure = c('exposure yes', 'exposure no'), # this includes our exposure variable with relevant labels
#                    outcomes = c('outcome yes', 'outcome no'), # this includes our outcome variable with relevant labels
#                    confounders = 1:n)) 


(mharray <- array(mhstrat, 
               dim= c(2,2,256), 
               list(exposure = c("Discrimination", "No Discrimination"), 
                    outcomes = c("Screening", "No Screening"), 
                    confounders = 1:256)))

                 
epi.2by2(mharray, method = "cross.sectional", conf.level = 0.95, units = 1, 
         interpret = FALSE, outcome = "as.columns") 







