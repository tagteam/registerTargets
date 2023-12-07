### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  7 2023 (17:32) 
## Version: 
## Last-Updated: Dec  7 2023 (17:54) 
##           By: Thomas Alexander Gerds
##     Update #: 7
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
tar_option_set(packages=c("prodlim","foreach","riskRegression","data.table","lava","Publish"))

# Definition of project functions
tar_source("functions")
tar_source("../Ltmle")

list(
    tar_target(register_data, fread("data/register_data.csv")),
    tar_target(baseline_covariates, fread("data/baseline_covariates.csv")),
    tar_target(time_covariates, fread("data/time_covariates.csv")),
    tar_target(regimen_data, fread("data/regimen_data.csv")),
    tar_target(outcome_data, fread("data/outcome_data.csv"))
)


######################################################################
### _targets.R ends here
