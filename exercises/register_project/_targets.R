### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  7 2023 (17:32) 
## Version: 
## Last-Updated: Dec  9 2023 (07:24) 
##           By: Thomas Alexander Gerds
##     Update #: 14
#----------------------------------------------------------------------
## 
### Commentary:
#
# This script defines an a targets project for the course exercises
#
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(targets)

# R-package dependencies
tar_option_set(packages=c("prodlim",
                          "foreach",
                          "riskRegression",
                          "SuperLearner",
                          "ranger",
                          "glmnet",
                          "data.table",
                          "lava",
                          "Publish"))
# Definition of project functions
tar_source("functions")
source("../Ltmle/Ltmle.R")

list(
    tar_target(register_data, fread("data/register_data.csv")),
    tar_target(baseline_covariates, fread("data/baseline_covariates.csv")),
    tar_target(time_covariates, fread("data/time_covariates.csv")),
    tar_target(regimen_data, fread("data/regimen_data.csv")),
    tar_target(mace_outcome_data, fread("data/mace_outcome_data.csv")),
    tar_target(survival_outcome_data, fread("data/survival_outcome_data.csv"))
)


######################################################################
### _targets.R ends here
