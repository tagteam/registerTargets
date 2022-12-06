### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  4 2022 (11:02) 
## Version: 
## Last-Updated: Dec  6 2022 (07:48) 
##           By: Thomas Alexander Gerds
##     Update #: 13
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(targets)
library(data.table)
setwd("~/metropolis/Teaching/targetedRegisterAnalysis/exercises/nutshell/")
for (f in list.files("functions/",pattern = "R$",full.names = TRUE)){source(f)}
list(
    tar_target(study_start, as.Date("2000-01-01")),
    tar_target(study_end, as.Date("2021-12-31")),
    tar_target(raw_data_path,
               # change to path on own computer
               "~/metropolis/Teaching/targetedRegisterAnalysis/exercises/nutshell/rawdata/"),
    # hospital admissions: outcome, comorbidity 
    tar_target(icd_codes, list(
                              # myocardial infarction
                              "MI" = c("DI2[12]"))),
    # prescription data: exposure, comedicine 
    tar_target(atc_codes,list(#beta blockers
                             bb = c('C07'),
                             #calcium chanel blockers
                             ccb = c('C08'))),
    # study population
    tar_target(pop, get_pop(raw_data_path = raw_data_path,
                            icd_codes = icd_codes)),
    # add demographics and apply exclusion
    tar_target(study_pop,
               get_study_pop(pop = pop,
                             raw_data_path = raw_data_path)),
    tar_target(t1,
               make_table1(study_pop),
               packages = "Publish")
)

######################################################################
### _targets.R ends here
