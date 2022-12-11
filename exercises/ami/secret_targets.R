### secret_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2022 (17:28) 
## Version: 
## Last-Updated: Dec 11 2022 (19:34) 
##           By: Thomas Alexander Gerds
##     Update #: 124
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
    tar_target(atc_codes,list(#beta blockers
                             bb = c('C07'),
                             #calcium chanel blockers
                             ccb = c('C08'))),
    # define study population
    tar_target(pop, {
        get_pop(raw_lpr_file = "rawdata/lpr.csv",
                icd_codes = icd_codes)
    },cue = tar_cue(mode = "always")),
    # lpr
    tar_target(como_list,{
        get_como_list(icd_codes = icd_codes,
                      lpr = lpr)
    },cue = tar_cue(mode = "always")),
    # lmdb
    tar_target(drug_list,{
        get_drug_list(atc_codes = atc_codes)
    },cue = tar_cue(mode = "always")),
    # add demographics and apply exclusion
    tar_target(study_pop,
               get_study_pop(pop = pop,
                             raw_cpr_file = "rawdata/cpr.csv")),
    # add demographics and apply exclusion
    tar_target(secret_study_pop,{
        secret_get_study_pop(pop = pop,
                             study_start = study_start,
                             study_end = study_end,
                             raw_cpr_file = "rawdata/cpr.csv")
    },cue = tar_cue(mode = "always")),
    # baseline characteristics
    tar_target(table1,
               make_table1(study_pop = study_pop),
               packages = "Publish"),
    # with exclusion
    tar_target(secret_table1,
               make_table1(study_pop = secret_study_pop),
               packages = "Publish"),
    # day 3
    tar_target(secret_baseline_pop,{
        secret_get_baseline_pop(study_pop = secret_study_pop,
                                como_list = como_list,
                                drug_list = drug_list)
    }),
    # more baseline characteristics
    tar_target(very_secret_table1,
               secret_make_table1(baseline_pop = secret_baseline_pop),
               packages = "Publish"),
    # Cox regression
    tar_target(hazard_ratio,{
        secret_baseline_pop[,mean(event),by = "bb"]
        secret_baseline_pop[bb == "Yes"&event == 1,.(pnr)]
        fit = coxph(Surv(time,event)~bb+age+sex+any.malignancy+diabetes.with.complications,
                    data = secret_baseline_pop)
        fit$call$data <- secret_baseline_pop
        publish(fit)
    }, packages = c("survival","Publish")),
    # average treatment effect
    tar_target(risk_ratio,{
        fit = coxph(Surv(time,event)~bb+age+sex+any.malignancy+diabetes.with.complications,
                    data = secret_baseline_pop,x = TRUE)
        x = ate(fit,data = secret_baseline_pop,treatment = "bb",times = c(5,10,15),verbose = FALSE)
        x$ratioRisk[]
    }, packages = c("survival","riskRegression")),
    # day 3
    # random forests
    tar_target(forest,{
        # pseudo value
        km = prodlim(Hist(time,event)~1,data = secret_baseline_pop)
        secret_baseline_pop[,pseudo5:= jackknife(km,times = 5)]
        forest5 = ranger(pseudo5~bb+age+sex+any.malignancy+diabetes.with.complications,
                         data = secret_baseline_pop,num.trees = 50)
        forest5
    }, packages = c("prodlim","survival","ranger"))    
)



######################################################################
### secret_targets.R ends here
