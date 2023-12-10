### get_table_1.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2023 (11:51) 
## Version: 
## Last-Updated: Dec 10 2023 (08:22) 
##           By: Thomas Alexander Gerds
##     Update #: 11
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
get_table_1 <- function(baseline_covariates,time_covariates,regimen_data){
    ## formula <- ~ sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration 
    ## formula <- ~ sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration + statin_0 
    formula <- Drug_0 ~ sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration + statin_0 
    # data.table way of joining data
    # first baseline_covariates with time_covariates (only columns pnr and statin_0)
    dd = baseline_covariates[time_covariates[,.(pnr,statin_0)],on = "pnr"]
    # then with regimen_data (only columns pnr and Drug_0)
    dd = dd[regimen_data[,.(pnr,Drug_0)],on = "pnr"]
    # rest is as in yesterdays exercises
    t1 <- summary(utable(formula,dd))
    fwrite(t1,"export/table1.csv")
    t1
}



######################################################################
### get_table_1.R ends here
