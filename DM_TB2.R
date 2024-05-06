#############################################################################
# NAME: DATA MANAGEMENT

BRFSS <- read.csv("/Users/mirtamaravilla/Documents/UW Courses/Spring Qtr 2024/EPI 514 /EPI 514 Research/cleanbrfss.csv") 


#libraries
library(epiR)

#Prevalence of Cervical cancer 

prevtable <- table(BRFSS$Hlthdiscrim_bin.f, BRFSS$CervScrnEver.f, dnn=c("Discrimination","Cervical Cancer Screening"))

prop.table(prevtable)


#Unadjusted PR (NO COVARIATES)

#tables for cervical cancer screening 
unadj <- with(BRFSS, table(Hlthdiscrim_bin.f, CervScrnEver.f))

unadj

epi.2by2(unadj)
