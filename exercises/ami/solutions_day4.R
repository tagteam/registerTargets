### solutions_day4.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 14 2022 (09:47) 
## Version: 
## Last-Updated: Dec 14 2022 (14:25) 
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
    tar_target(cox_fit,{
        fit = coxph(Surv(time,event)~bb+age+sex+any.malignancy+diabetes.with.complications,
                    data = wide_baseline_pop,x = TRUE)
    },packages = "survival"),
    tar_target(cox_table,{
        fit = cox_fit
        fit$call$data <- wide_baseline_pop
        publish(fit)
    }, packages = c("survival","Publish")),
    # average treatment effect (G-formula using Cox)
    tar_target(ate_gformula,{
        x = ate(cox_fit,data = wide_baseline_pop,treatment = "bb",times = c(5,10,15),verbose = FALSE)
        x$ratioRisk[]
    }, packages = c("survival","riskRegression")),
    # propensity score model
    tar_target(propensity_fit,{
        glm(I(bb == "Yes")~+age+sex+any.malignancy+diabetes.with.complications,data = wide_baseline_pop,family = "binomial")
    }),
    # propensity score odds ratio table
    tar_target(propensity_table,{
        fit = propensity_fit
        fit$call$data = wide_baseline_pop
        publish(fit)
    },packages = "Publish"),
    # censoring probability Cox model
    tar_target(cens_fit,{
        fit = coxph(Surv(time,event == 0)~bb+age+sex+any.malignancy+diabetes.with.complications,
                    data = wide_baseline_pop,x = TRUE)
    },packages = "survival"),
    # censoring hazard ratio table
    tar_target(cens_table,{
        fit = cens_fit
        fit$call$data <- wide_baseline_pop
        publish(fit)
    },packages = "Publish"),    
    # average treatment effect (doubly robust)
    tar_target(ate_double_robust,{
        x = ate(event = cox_fit,
                treatment = propensity_fit,
                censor = cens_fit,
                data = wide_baseline_pop,
                times = c(5,10,15),
                se = FALSE,
                verbose = FALSE)
        x
    }, packages = c("survival","riskRegression")),
    # average treatment effect (ltmle, glm)
    tar_target(ate_pseudo_tmle,{
        # pseudo value for 5-year outcome
        km = prodlim(Hist(time,event)~1,data = wide_baseline_pop)
        wide_baseline_pop[,pseudo5:= jackknife(km,times = 5)]
        wbp = wide_baseline_pop[,c("sex","age","diabetes.with.complications","any.malignancy","bb","pseudo5")]
        wbp[,bb := as.numeric(bb == "Yes")]
        ## x = ltmle(data = wbp,Anodes = "bb",Lnodes = c("sex","age","diabetes.with.complications","any.malignancy"),Ynodes = "pseudo5",SL.library = "glm",abar = list(1,0))
        ## summary(x)
    }, packages = c("ltmle","prodlim")),
    # average treatment effect (ltmle, glm)
    tar_target(wide_discrete_data,{
        outcome_data <- wide_baseline_pop[,.(pnr,date = time,event)]
        # approximate event time on a discrete time grid with 6 months long intervals 
        grid <- wide_baseline_pop[,.(date = seq(0,5,.5),interval = 0:10),by = pnr]
        discrete_outcome = map_intervals(grid,data = outcome_data,name = "Death",rollforward = TRUE)
        wbp = wide_baseline_pop[,c("pnr","sex","age","diabetes.with.complications","any.malignancy","bb")]
        wbp = wbp[discrete_outcome,on = "pnr"]
        wbp[,bb := as.numeric(bb == "Yes")]
        ## wbp[,table(Death_0)]
        wbp = wbp[Death_0 != 1]
        wbp[,Death_0 := NULL]
        wbp[]
    }),
    tar_target(ate_ltmle,{
        x = Ltmle(data = wbp,Anodes = "bb",Lnodes = c("sex","age","diabetes.with.complications","any.malignancy"),Ynodes = c("Death_1","Death_2","Death_3","Death_4","Death_5","Death_6","Death_7","Death_8","Death_9","Death_10"),survivalOutcome = TRUE,variance.method = "ic",SL.library = "glm",abar = list(1,0))
        ## summary(x)
    }, packages = c("ltmle","prodlim"))    
)


######################################################################
### solutions_day4.R ends here
