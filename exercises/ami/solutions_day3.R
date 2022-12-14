### solutions_day3.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 14 2022 (08:39) 
## Version: 
## Last-Updated: Dec 14 2022 (09:58) 
##           By: Thomas Alexander Gerds
##     Update #: 8
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
library(heaven)
for (f in list.files("functions/",pattern = "R$",full.names = TRUE)){
    source(f)
}
for (f in list.files("secrets/",pattern = "R$",full.names = TRUE)){
    source(f)
}
list(
    # the study period
    tar_target(study_start, as.Date("2000-01-01")),
    tar_target(study_end, as.Date("2021-12-31")),
    # hospital admissions: outcome and comorbidities
    tar_target(icd_codes, heaven::charlson.codes),
    # prescription data: exposure and comedicine 
    tar_target(atc_codes,list(
                             #beta blockers
                             bb = c('C07'),
                             #calcium chanel blockers
                             ccb = c('C08'),
                             #RAS inhibitors
                             rasi = c('C09'),
                             #Thiazid
                             thiazid = c('C03A'),
                             #Loop diurestics
                             loop = c('C03C', 'C03EB'),
                             #Mineralcorticoid receptor antagonister
                             mra = c('C03D'),
                             #Digoxin
                             digoxin = c('C01AA05'),
                             #Statins
                             statin = c('C10A', 'A10BH51', 'A10BH52'),
                             #Acetylsalicylic acid (aspirin)
                             asa = c('B01AC06', 'N02BA01'),
                             #ADP receptor inhibitor
                             adpi = c('B01AC'),
                             #Vitamin K antagonists
                             vka = c('B01AA'),
                             # copd
                             copd_med = c('R03BA', paste0('R03AK0',c(6:9)),paste0('R03AK', c(10:12)), paste0('R03AL0', c(8:9)),'R03AC', paste0('R03AK0', c(6:9)), paste0('R03AK', c(10:13)), paste0('R03AL0', c(1:9)),'R03BB'),
                             # dementia
                             dementia_med='N06D',
                             #new oral anti-coagulents
                             noac = c('B01AF')
                         )),
    # define study population
    tar_target(pop, {
        get_pop(raw_lpr_file = "rawdata/lpr.csv",
                icd_codes = icd_codes)
    }),
    # lpr
    tar_target(como_list,{
        get_como_list(icd_codes = icd_codes)
    }),
    # lmdb
    tar_target(drug_list,{
        get_drug_list(atc_codes = atc_codes)
    }),
    # add demographics and apply exclusion
    tar_target(study_pop,{
        get_study_pop_1(pop = pop,
                        study_start = study_start,
                        study_end = study_end,
                        raw_cpr_file = "rawdata/cpr.csv")
    }),
    # baseline characteristics
    tar_target(table1,
               make_table1(study_pop = study_pop),
               packages = "Publish"),
    # day 3
    tar_target(wide_baseline_pop,{
        get_wide_baseline_pop(study_pop = study_pop,
                              como_list = como_list,
                              drug_list = drug_list)
    }),
    # more baseline characteristics
    tar_target(wide_table1,
               make_wide_table1(wide_baseline_pop = wide_baseline_pop),
               packages = "Publish"),
    # Cox regression
    tar_target(hazard_ratio,{
        fit = coxph(Surv(time,event)~bb+age+sex+any.malignancy+diabetes.with.complications,
                    data = wide_baseline_pop,x = TRUE)
        fit$call$data <- wide_baseline_pop
        publish(fit)
    }, packages = c("survival","Publish"))
)


######################################################################
### solutions_day3.R ends here
