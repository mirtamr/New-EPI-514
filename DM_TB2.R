#############################################################################
# NAME: DATA MANAGEMENT

#BRFSSclean <- read.csv("/Users/mirtamaravilla/Documents/UW Courses/Spring Qtr 2024/EPI 514 /EPI 514 Research/cleanbrfss.csv") 
BRFSSclean <- read.csv("/Users/aileenandrade/Desktop/Spring Quarter/Epi 514/BRFSS project/Data/cleanbrfss.csv") 
View(BRFSSclean)
#libraries
library(epiR)

#Prevalence of Cervical cancer 

prevtable <- table(BRFSSclean$Hlthdiscrim_bin.f, BRFSSclean$CervScrnEver.f, dnn=c("Discrimination","Cervical Cancer Screening"))

prop.table(prevtable)

table(BRFSSclean$Hlthdiscrim_bin.f, BRFSSclean$CervScrnEver.f)

#Unadjusted PR (NO COVARIATES)

#tables for cervical cancer screening 
unadj <- with(BRFSSclean, table(Hlthdiscrim_bin.f, CervScrnEver.f))

unadj

epi.2by2(unadj, method = "cross.sectional")




######################### Adjusted PRs ########################################

cerv_yes <- BRFSSclean$CervScrnEver=="yes"

#Race

RceCervAdj <-table(BRFSSclean$Hlthdiscrim_bin.f, BRFSSclean$cerv_yes, BRFSSclean$race)





