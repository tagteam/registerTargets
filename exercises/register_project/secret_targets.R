### secret_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2023 (08:01) 
## Version: 
## Last-Updated: Dec  9 2023 (14:46) 
##           By: Thomas Alexander Gerds
##     Update #: 98
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
tar_option_set(packages=c("prodlim","foreach","riskRegression","SuperLearner","ranger","glmnet","data.table","lava","Publish"))

# Definition of project functions
tar_source("functions")
tar_source("secret_functions")
tar_source("../Ltmle/")

list(
    # pre-defined targets
    tar_target(register_data, fread("data/register_data.csv")),
    tar_target(raw_baseline_covariates, fread("data/baseline_covariates.csv")),
    tar_target(time_covariates, fread("data/time_covariates.csv")),
    tar_target(regimen_data, fread("data/regimen_data.csv")),
    tar_target(survival_outcome_data, fread("data/survival_outcome_data.csv")),
    tar_target(mace_outcome_data, fread("data/mace_outcome_data.csv")),
    # day 2 part 1
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
    # day 2 parts 2 and 4
    tar_target(ltmle_fit_death_1,
               run_Ltmle(name_outcome="Dead",
                         name_censoring = "Censored",
                         censored_label = 0,
                         time_horizon=1,
                         outcome_data=survival_outcome_data,
                         regimen_data=list(Drug = regimen_data),
                         baseline_data=baseline_covariates,
                         timevar_data=time_covariates,
                         abar = list(control = 0,treat = 1),
                         SL.library="glm",
                         verbose=TRUE)
               ),
    tar_target(ltmle_fit_death_2,{
        run_Ltmle(name_outcome="Dead",
                  name_censoring = "Censored",
                  censored_label = 0,
                  time_horizon=4,
                  outcome_data=survival_outcome_data,
                  regimen_data=list(Drug = regimen_data),
                  baseline_data=baseline_covariates,
                  timevar_data=time_covariates,
                  abar = list(treat = rep(1,4),control = rep(0,4)),
                  SL.library="glm",
                  verbose=TRUE)
    }),
    tar_target(ltmle_summary_death_2,{
        summary(ltmle_fit_death_2)
    }),
    # day 3 part 1
    tar_target(ps1,
               glm(Drug_0~ sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration,family = "binomial",data = register_data)
               ),
    tar_target(ps2,
               glm(Drug_0~ sex + agegroups + tertile_income * education + index_heart_failure + diabetes_duration,family = "binomial",data = register_data)
               ),
    # day 3 part 2
    tar_target(ltmle_sl_death_1,
               run_Ltmle(name_outcome="Dead",
                         name_censoring = "Censored",
                         censored_label = 0,
                         time_horizon=1,
                         outcome_data=survival_outcome_data,
                         regimen_data=list(Drug = regimen_data),
                         baseline_data=baseline_covariates,
                         timevar_data=time_covariates,
                         abar = list(control = 0,treat = 1),
                         SL.library=c("SL.glm","SL.glm.interaction"),
                         SL.cvControl = list(V = 2),
                         verbose=TRUE)),
    tar_target(ltmle_sl_summary_death_1,{
        summary(ltmle_sl_death_1)
    }),
    # day 3 part 3
    tar_target(ltmle_glmnet_death_1,
               run_Ltmle(name_outcome="Dead",
                         name_censoring = "Censored",
                         censored_label = 0,
                         time_horizon=1,
                         outcome_data=survival_outcome_data,
                         regimen_data=list(Drug = regimen_data),
                         baseline_data=baseline_covariates,
                         timevar_data=time_covariates,
                         abar = list(control = 0,treat = 1),
                         SL.library=c("SL.glm","SL.glm.interaction","SL.glmnet"),
                         SL.cvControl = list(V = 2),
                         verbose=TRUE)),
    # day 3 part 4
    tar_target(ltmle_SL_death_2,
               run_Ltmle(name_outcome="Dead",
                         name_censoring = "Censored",
                         censored_label = 0,
                         time_horizon=1,
                         outcome_data=survival_outcome_data,
                         regimen_data=list(Drug = regimen_data),
                         baseline_data=baseline_covariates,
                         timevar_data=time_covariates,
                         abar = list(control = 0,treat = 1),
                         SL.library=c("SL.glm","SL.glm.interaction","SL.ranger2","SL.glmnet"),
                         SL.cvControl = list(V = 2),
                         verbose=TRUE)
               ),
    # day 4 part 1
    tar_target(ltmle_fit_glm_mace_2,
               run_Ltmle(name_outcome="mace",
                         name_competing_risk = "Dead",
                         name_censoring = "Censored",
                         censored_label = 0,
                         time_horizon=4,
                         outcome_data=mace_outcome_data,
                         regimen_data=list(Drug = regimen_data),
                         baseline_data=baseline_covariates,
                         timevar_data=time_covariates,
                         abar = list(control = rep(0,4),treat = rep(1,4)),
                         SL.library="glm",
                         verbose=TRUE)),
    tar_target(ltmle_summary_mace_2,{
        summary(ltmle_fit_glm_mace_2)
    }),
    # day 4 part 2
    tar_target(ltmle_ipw_mace_2,
               run_Ltmle(name_outcome="mace",
                         name_competing_risk = "Dead",
                         name_censoring = "Censored",
                         censored_label = 0,
                         time_horizon=4,
                         outcome_data=mace_outcome_data,
                         regimen_data=list(Drug = regimen_data),
                         baseline_data=baseline_covariates,
                         timevar_data=time_covariates,
                         abar = list(control = rep(0,4),treat = rep(1,4)),
                         iptw.only = TRUE,
                         SL.library="glm",
                         verbose=TRUE)
               ),
    tar_target(ltmle_summary_ipw_mace_2,{
        summary(ltmle_ipw_mace_2)
    }),
    tar_target(ltmle_gcomp_mace_2,
               run_Ltmle(name_outcome="mace",
                         name_competing_risk = "Dead",
                         name_censoring = "Censored",
                         censored_label = 0,
                         time_horizon=4,
                         outcome_data=mace_outcome_data,
                         regimen_data=list(Drug = regimen_data),
                         baseline_data=baseline_covariates,
                         timevar_data=time_covariates,
                         abar = list(control = rep(0,4),treat = rep(1,4)),
                         gcomp = TRUE,
                         SL.library="glm",
                         verbose=TRUE)),
    tar_target(ltmle_summary_gcomp_mace_2,{
        summary(ltmle_gcomp_mace_2)
    }),
    tar_target(ltmle_gcomp_mace_2_subset,
               run_Ltmle(name_outcome="mace",
                         name_competing_risk = "Dead",
                         name_censoring = "Censored",
                         censored_label = 0,
                         time_horizon=4,
                         sub_set = list(data = baseline_covariates[education == "High",.(pnr)]),
                         outcome_data=mace_outcome_data,
                         regimen_data=list(Drug = regimen_data),
                         baseline_data=baseline_covariates,
                         timevar_data=time_covariates,
                         abar = list(control = rep(0,4),treat = rep(1,4)),
                         gcomp = TRUE,
                         SL.library="glm",
                         verbose=TRUE)),
    tar_target(ltmle_summary_gcomp_mace_2_subset,{
        summary(ltmle_gcomp_mace_2_subset)
    }),
    tar_target(ltmle_ipw_mace_2_subset,
               run_Ltmle(name_outcome="mace",
                         name_competing_risk = "Dead",
                         name_censoring = "Censored",
                         censored_label = 0,
                         time_horizon=4,
                         sub_set = list(data = baseline_covariates[education == "High",.(pnr)]),
                         outcome_data=mace_outcome_data,
                         regimen_data=list(Drug = regimen_data),
                         baseline_data=baseline_covariates,
                         timevar_data=time_covariates,
                         abar = list(control = rep(0,4),treat = rep(1,4)),
                         iptw.only = TRUE,
                         SL.library="glm",
                         verbose=TRUE)),
    tar_target(ltmle_summary_ipw_mace_2_subset,{
        summary(ltmle_ipw_mace_2_subset)
    }),
    tar_target(ltmle_fit_glm_mace_2_subset,
               run_Ltmle(name_outcome="mace",
                         name_competing_risk = "Dead",
                         name_censoring = "Censored",
                         censored_label = 0,
                         time_horizon=4,
                         sub_set = list(data = baseline_covariates[education == "High",.(pnr)]),
                         outcome_data=mace_outcome_data,
                         regimen_data=list(Drug = regimen_data),
                         baseline_data=baseline_covariates,
                         timevar_data=time_covariates,
                         abar = list(control = rep(0,4),treat = rep(1,4)),
                         SL.library="glm",
                         verbose=TRUE)),
    tar_target(ltmle_summary_glm_mace_2_subset,{
        summary(ltmle_fit_glm_mace_2_subset)
    })
)


######################################################################
### secret_targets.R ends here
