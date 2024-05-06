#############################################################################
# NAME: DATA MANAGEMENT

BRFSS <- read.csv("/Users/mirtamaravilla/Documents/UW Courses/Spring Qtr 2024/EPI 514 /EPI 514 Research/cleanbrfss.csv") 


#Prevalence of Cervical cancer 

prevccs <- table(BRFSS$HlthDiscrim.f, BRFSS$CervScrnEver.f)

prevccs

#Unadjusted PR (NO COVARIATES)




