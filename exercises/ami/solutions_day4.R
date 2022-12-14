### solutions_day4.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 14 2022 (09:47) 
## Version: 
## Last-Updated: Dec 14 2022 (16:52) 
##           By: Thomas Alexander Gerds
##     Update #: 75
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
for (f in list.files("functions/",pattern = "R$",full.names = TRUE)){source(f)}
for (f in list.files("secrets/",pattern = "R$",full.names = TRUE)){source(f)}
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
    tar_target(ate_gformula_cox,{
        x = ate(cox_fit,
                data = wide_baseline_pop,
                treatment = "bb",
                times = 5,
                verbose = FALSE)
        x
    }, packages = c("survival","riskRegression")),
    # forest
    tar_target(forest,{
        # 
        # pseudo value for 5-year outcome 
        km = prodlim(Hist(time,event)~1,data = wide_baseline_pop)
        wide_baseline_pop[,pseudo5:= jackknife(km,times = 5)]
        forest5 = ranger(pseudo5~bb+age+sex+any.malignancy+diabetes.with.complications,
                         data = wide_baseline_pop,num.trees = 50)
        forest5
    },package = c("prodlim","ranger")),
    # average treatment effect (G-formula using random forest)
    tar_target(ate_gformula_forest,{
        x = ate(cox_fit,
                data = wide_baseline_pop,
                treatment = "bb",
                times = 5,
                verbose = FALSE)
        x
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
                times = 5,
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
        x = ltmle(data = wbp,Anodes = "bb",Lnodes = c("sex","age","diabetes.with.complications","any.malignancy"),Ynodes = "pseudo5",SL.library = "glm",abar = list(1,0))
        summary(x)
    }, packages = c("ltmle","prodlim")),
    # average treatment effect (ltmle, glm)
    tar_target(wide_discrete_data,{
        outcome_data <- wide_baseline_pop[,.(pnr,date = time,event)]
        outcome_data[event == 1,date := Inf]
        cens_data <- wide_baseline_pop[,.(pnr,date = time,event)]
        cens_data[event == 1,date := Inf]
        # approximate event time on a discrete time grid with 6 months long intervals
        intervals = seq(0,5,.5)
        grid <- wide_baseline_pop[,.(date = intervals,interval = 0:(length(intervals)-1)),by = pnr]
        discrete_outcome = map_intervals(grid,data = outcome_data,name = "Death",rollforward = TRUE)
        discrete_censored = map_intervals(grid,data = cens_data,name = "Censored",rollforward = TRUE)
        wbp = wide_baseline_pop[,c("pnr","sex","age","diabetes.with.complications","any.malignancy","bb")]
        wbp = wbp[discrete_outcome,on = "pnr"]
        wbp = wbp[discrete_censored,on = "pnr"]
        wbp[,bb := as.numeric(bb == "Yes")]
        ## wbp[,table(Death_0)]
        wbp = wbp[Death_0 != 1]
        wbp = wbp[Censored_0 != 1]
        wbp[,Death_0 := NULL]
        wbp[,Censored_0 := NULL]
        wbp[,pnr := NULL]
        wbp[]
    }),
    ## tar_target(survtmle_fit,{
        ## wide_baseline_pop[time>0,{survtmle(ftime = time,
                                     ## ftype = event,
                                     ## trt = bb,
                                     ## adjustVars = data.frame(sex = sex,age = age),
                                     ## glm.trt = "sex + age",
                                     ## glm.ftime = "trt + sex + age",
                                     ## glm.ctime = "trt + sex + age",
                                     ## method = "mean",
                                     ## SL.ftime = c("SL.mean"),
                                     ## SL.ctime = c("SL.mean"),
                                     ## t0 = 5)}]
        
    ## }),
    tar_target(ate_ltmle,{
        w = wide_discrete_data
        ww <- event_node_manipulator(data=w,k=10,outcome="Death",competing=NULL,censored="Censored",outcome_is_competing=NULL)
        x = Ltmle(data = wide_discrete_data,Anodes = "bb",Lnodes = c("sex","age","diabetes.with.complications","any.malignancy"),Cnodes = grep("Censored_",names(wide_discrete_data),value = TRUE),Ynodes = grep("Death_",names(wide_discrete_data),value = TRUE),survivalOutcome = TRUE,variance.method = "ic",SL.library = "glm",abar = list(1,0),verbose = TRUE)
        ## SL.library = c("SL.glm","SL.ranger"),
        summary(x)
    }, packages = c("ltmle","prodlim","SuperLearner")),
    tar_target(ate_ltmle_0,{
        w = wide_discrete_data
        w[,sex := NULL]
        w[,age := NULL]
        w[,diabetes.with.complications := NULL]
        w[,any.malignancy := NULL]
        ww <- event_node_manipulator(data=w,k=10,outcome="Death",competing=NULL,censored="Censored",outcome_is_competing=NULL)
        x = Ltmle(data = wide_discrete_data,
                  Anodes = "bb",
                  ## Lnodes = c("sex","age","diabetes.with.complications","any.malignancy"),
                  Lnodes = NULL,
                  Cnodes = grep("Censored_",names(wide_discrete_data),value = TRUE),
                  Ynodes = grep("Death_",names(wide_discrete_data),value = TRUE),
                  survivalOutcome = TRUE,
                  variance.method = "ic",
                  SL.library = "glm",
                  ## SL.library = c("SL.glm","SL.ranger"),
                  abar = list(1,0),verbose = TRUE)
        summary(x)
    }, packages = c("ltmle","prodlim","SuperLearner"))    
)


######################################################################
### solutions_day4.R ends here
