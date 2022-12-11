### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  4 2022 (11:02) 
## Version: 1
## Last-Updated: Dec 11 2022 (09:37) 
##           By: Thomas Alexander Gerds
##     Update #: 24
#----------------------------------------------------------------------
## 
### Commentary:
## 
##  This project is used during the exercises of the PhD
##  course 'Targeted Register Analysis'.
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(targets)
library(data.table)
for (f in list.files("functions/",pattern = "R$",full.names = TRUE)){
    source(f)
}
list(
    # the study period
    tar_target(study_start, as.Date("2000-01-01")),
    tar_target(study_end, as.Date("2021-12-31")),
    # hospital admissions: outcome and comorbidities
    tar_target(icd_codes, list(
                              # myocardial infarction
                              "MI" = c("DI2[12]"))),
    # prescription data: exposure and comedicine 
    tar_target(atc_codes,list(#beta blockers
                             bb = c('C07'),
                             #calcium chanel blockers
                             ccb = c('C08'))),
    # define study population
    tar_target(pop, get_pop(raw_lpr_file = "rawdata/lpr.csv",
                            icd_codes = icd_codes)),
    # add demographics and apply exclusion
    tar_target(study_pop,
               get_study_pop(pop = pop,
                             raw_cpr_file = "rawdata/cpr.csv")),
    # baseline characteristics
    tar_target(table1,
               make_table1(study_pop = study_pop),
               packages = "Publish")
)

######################################################################
### _targets.R ends here
