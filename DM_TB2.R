#############################################################################
# NAME: DATA MANAGEMENT

BRFSS <- read.csv("/Users/mirtamaravilla/Documents/UW Courses/Spring Qtr 2024/EPI 514 /EPI 514 Research/cleanbrfss.csv") 


#Prevalence of Cervical cancer 

prevtable <- table(BRFSS$Hlthdiscrim_bin.f, BRFSS$CervScrnEver.f, dnn=c("Discrimination","Cervical Cancer Screening"))

prop.table(prevtable)


#Unadjusted PR (NO COVARIATES)




