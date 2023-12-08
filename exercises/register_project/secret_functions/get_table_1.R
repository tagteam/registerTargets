### get_table_1.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2023 (11:51) 
## Version: 
## Last-Updated: Dec  8 2023 (18:00) 
##           By: Thomas Alexander Gerds
##     Update #: 9
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
get_table_1 <- function(prepared_baseline_covariates,time_covariates,regimen_data){
    ## formula <- ~ sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration 
    ## formula <- ~ sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration + statin_0 
    formula <- Drug_0 ~ sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration + statin_0 
    # join data
    dd = prepared_baseline_covariates[time_covariates[,.(pnr,statin_0)],on = "pnr"]
    dd = dd[regimen_data[,.(pnr,Drug_0)],on = "pnr"]
    t1 <- summary(utable(formula,dd))
    fwrite(t1,"export/table1.csv")
    t1
}



######################################################################
### get_table_1.R ends here
