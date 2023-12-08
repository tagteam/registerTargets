### secret_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2023 (08:01) 
## Version: 
## Last-Updated: Dec  8 2023 (19:25) 
##           By: Thomas Alexander Gerds
##     Update #: 40
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
try(setwd("~/metropolis/Teaching/targetedRegisterAnalysis/exercises/register_project/"))
library(targets)

# R-package dependencies
tar_option_set(packages=c("prodlim","foreach","riskRegression","data.table","lava","Publish"))

# Definition of project functions
tar_source("functions")
tar_source("secret_functions")
source("../Ltmle/Ltmle.R")
source("../Ltmle/run_Ltmle.R")

list(
    # pre-defined targets
    tar_target(register_data, fread("data/register_data.csv")),
    tar_target(raw_baseline_covariates, fread("data/baseline_covariates.csv")),
    tar_target(time_covariates, fread("data/time_covariates.csv")),
    tar_target(regimen_data, fread("data/regimen_data.csv")),
    tar_target(survival_outcome_data, fread("data/survival_outcome_data.csv")),
    tar_target(mace_outcome_data, fread("data/mace_outcome_data.csv")),
    # part 1
    tar_target(baseline_covariates,{
        bsl <- copy(raw_baseline_covariates)
        bsl[,sex:=factor(sex,levels=c("0","1"),labels=c("Female","Male"))]
        bsl[,education:=factor(education,levels=c("Basic","Medium","High"),labels=c("Basic","Medium","High"))]
        bsl[,tertile_income:=factor(tertile_income,levels=c("Income_q1","Income_q2","Income_q3"),labels=c("Income_q1","Income_q2","Income_q3"))]
        bsl[,agegroups:=factor(agegroups,levels=c("below 45","45-50","50-55","55-60","60-65","65-70","70-75","75-80","80-85","above 85"),labels=c("below 45","45-50","50-55","55-60","60-65","65-70","70-75","75-80","80-85","above 85"))]
        bsl[,index_heart_failure:=factor(index_heart_failure,levels=c("0","1"),labels=c("No","Yes"))]
        bsl[,diabetes_duration:=factor(diabetes_duration,levels=c("below 5","5-10","above 10"),labels=c("below 5","5-10","above 10"))]
        bsl
    }),
    tar_target(table_1, {
        get_table_1(baseline_covariates,time_covariates,regimen_data)
    }),
    # part 2
    tar_target(ltmle_fit_death_1,
               run_Ltmle(name_outcome="Dead",
                         time_horizon=1,
                         outcome_data=survival_outcome_data,
                         regimen_data=list(Drug = regimen_data),
                         baseline_data=baseline_covariates,
                         timevar_data=time_covariates,
                         abar = list(0,1),
                         SL.library="glm",
                         verbose=TRUE)),
    tar_target(ltmle_fit_death_2,{
        run_Ltmle(name_outcome="Dead",
                  time_horizon=4,
                  outcome_data=survival_outcome_data,
                  regimen_data=list(Drug = regimen_data),
                  baseline_data=baseline_covariates,
                  timevar_data=time_covariates,
                  abar = list(rep(1,4),rep(0,4)),
                  SL.library="glm",
                  verbose=TRUE)
    })
)


######################################################################
### secret_targets.R ends here
