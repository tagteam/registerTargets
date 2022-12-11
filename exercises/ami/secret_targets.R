### secret_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2022 (17:28) 
## Version: 
## Last-Updated: Dec 11 2022 (18:32) 
##           By: Thomas Alexander Gerds
##     Update #: 120
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
        sgs = secret_get_study_pop(pop = pop,
                                   study_start = study_start,
                                   study_end = study_end,
                                   raw_cpr_file = "rawdata/cpr.csv")
        ## cheating time for day 2
        sgsmod = copy(sgs)
        sgsmod[,max := as.numeric(death_date-index)]
        sgsmod[!is.na(death_date)&1*(death_date-index)<8*365.25&sex == "Female",death_date := death_date-runif(.N,1,max)]
        sgsmod[!is.na(death_date)&1*(death_date-index)<5*365.25&sex == "Male",death_date := death_date+runif(.N,1,.6*365.25)]
        sgsmod[end_fup<death_date,end_fup := pmin(death_date,as.Date("2022-12-12"))]
        sgsmod[end_fup<death_date,death_date := NA]
        sgsmod[,time := as.numeric(end_fup-index)/365.25]
        sgsmod[,event := 0]
        sgsmod[!is.na(death_date),event := 1]
        library(prodlim)
        fit <- prodlim(Hist(time,event)~sex,data = sgsmod)
        u = summary(fit,percent = TRUE,surv = FALSE,times = c(1,5,10))[,c("time","sex","cuminc")]
        setDT(u)
        print(dcast(u,sex~time,value.var = "cuminc",fun = mean))
        sgsmod[,time := NULL]
        sgsmod[,max := NULL]
        sgsmod[,event := NULL]
        message("saving popamis data file")
        ## fwrite(sgs,file = "~/metropolis/Teaching/targetedRegisterAnalysis/exercises/spaghetti/popami.csv")
        fwrite(sgsmod,file = "~/metropolis/Teaching/targetedRegisterAnalysis/exercises/spaghetti/popami.csv")
        sgs[]
    },cue = tar_cue(mode = "always")),
    # baseline characteristics
    tar_target(table1,
               make_table1(study_pop = study_pop),
               packages = "Publish"),
    # day 2
    tar_target(secret_baseline_pop,{
        secret_get_baseline_pop(study_pop = secret_study_pop,
                                como_list = como_list,
                                drug_list = drug_list)
    }),
    # more baseline characteristics
    tar_target(day2_table1,
               day2_make_table1(baseline_pop = secret_baseline_pop),
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
