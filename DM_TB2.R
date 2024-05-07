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

incomerrtab <-table(BRFSS$Hlthdiscrim_bin.f, BRFSS$CervScrnEver.f, BRFSS$income.f, deparse.level = 2)
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

####Education 

#2x2xn table, education
edurrtab <- table(BRFSS$Hlthdiscrim_bin.f, BRFSS$CervScrnEver.f, BRFSS$edu.f, deparse.level =2)
(edustrat <- epi.2by2(dat=edurrtab, method = "cross.sectional"))
edustrat$massoc.detail$PR.strata.wald

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


