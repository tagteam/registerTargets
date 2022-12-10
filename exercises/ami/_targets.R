### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  4 2022 (11:02) 
## Version: 1
## Last-Updated: Dec 10 2022 (15:18) 
##           By: Thomas Alexander Gerds
##     Update #: 23
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
    # where to find the (computer simulated) register data
    tar_target(raw_data_path,"./rawdata/"),
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
    tar_target(pop, get_pop(raw_data_path = raw_data_path,
                            icd_codes = icd_codes)),
    # add demographics and apply exclusion
    tar_target(study_pop,
               get_study_pop(pop = pop,
                             raw_data_path = raw_data_path)),
    # baseline characteristics
    tar_target(table1,
               make_table1(study_pop = study_pop),
               packages = "Publish")
)

######################################################################
### _targets.R ends here
