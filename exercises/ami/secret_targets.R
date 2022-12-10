### secret_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2022 (17:28) 
## Version: 
## Last-Updated: Dec 10 2022 (16:45) 
##           By: Thomas Alexander Gerds
##     Update #: 59
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
list(
    # the study period
    tar_target(study_start, as.Date("2000-01-01")),
    tar_target(study_end, as.Date("2021-12-31")),
    # where to find the (computer simulated) register data
    tar_target(raw_data_path,"./rawdata/"),
    # hospital admissions: outcome and comorbidities
    tar_target(icd_codes, heaven::charlson.codes),
    # prescription data: exposure and comedicine 
    tar_target(atc_codes,list(#beta blockers
                             bb = c('C07'),
                             #calcium chanel blockers
                             ccb = c('C08'))),
    # define study population
    tar_target(pop, get_pop(raw_data_path = raw_data_path,
                            icd_codes = icd_codes)),
    # lpr
    tar_target(como_list,{
        icd_codes
        lpr <- fread(paste0(raw_data_path,"lpr.csv"),
                     keepLeadingZeros = TRUE,
                     colClasses = c("character","character","Date"))
        x = lapply(names(icd_codes),function(disease){
            out = lpr[grep(paste0(paste0("^",icd_codes[[disease]]),collapse = "|"),diag)]
            # remove duplicated entries with same admission date
            out <- out[out[,.I[1],by=c("pnr","inddto")]$V1]
            out[,X := disease]
            out[]
        })
        names(x) = names(icd_codes)
        x
    }),
    # lmdb
    tar_target(drug_list,{
        lmdb <- fread(paste0(raw_data_path,"lmdb.csv"),
                     keepLeadingZeros = TRUE,
                     colClasses = c("character","Date","character"))
        x = lapply(names(atc_codes),function(drug){
            out = lmdb[grep(paste0(paste0("^",atc_codes[[drug]]),collapse = "|"),atc)]
            # remove duplicated entries with same admission date
            out <- out[out[,.I[1],by=c("pnr","eksd")]$V1]
            out[,X := drug]
            out[]
        })
        names(x) = names(atc_codes)
        x
    }),
    # add demographics and apply exclusion
    tar_target(study_pop,{
        sgs = secret_get_study_pop(pop = pop,
                                   study_start = study_start,
                                   study_end = study_end,
                                   raw_data_path = raw_data_path)
        message("saving popamis data file")
        fwrite(sgs,file = "~/metropolis/Teaching/targetedRegisterAnalysis/exercises/example-project/popami.csv")
        sgs[]
               
    }),
    # baseline characteristics
    tar_target(table1,
               make_table1(study_pop),
               packages = "Publish"),
    # day 2
    tar_target(baseline_pop,{
        # select relevant variables from study_pop
        baseline_pop = study_pop[,.(pnr,index,sex,age,end_fup,death_date)]
        # define event time
        baseline_pop[,time := as.numeric(end_fup-index)/365.25]
        baseline_pop[,event := 0]
        baseline_pop[!is.na(death_date),event := 1]
        setkey(baseline_pop,pnr,index)
        # restrict to relevant variables 
        baseline_pop = baseline_pop[,.(pnr,index,sex,age,time,event)]
        # loop across comorbidities to extract values at index (baseline, start of followup)
        for (como in names(como_list)){
            como_dat = como_list[[como]]
            # rename inddto to index in order to roll the join
            setnames(como_dat,"inddto","index")
            setkey(como_dat,pnr,index)
            baseline_pop = como_dat[,.(pnr,index,X)][baseline_pop,roll = TRUE]
            set(baseline_pop,j = como,value = ifelse(is.na(baseline_pop$X),"No","Yes"))
            baseline_pop[,X := NULL]
            baseline_pop
        }
        # loop across drugs to extract exposure 180 days before index (baseline, start of followup)
        for (drug in names(drug_list)){
            drug_dat = drug_list[[drug]]
            # rename inddto to index in order to roll the join
            setnames(drug_dat,"eksd","index")
            setkey(drug_dat,pnr,index)
            baseline_pop = drug_dat[,.(pnr,index,X)][baseline_pop,roll = 180]
            set(baseline_pop,j = drug,value = ifelse(is.na(baseline_pop$X),"No","Yes"))
            baseline_pop[,X := NULL]
            baseline_pop
        }
        baseline_pop[]
    }),
    # more baseline characteristics
    tar_target(day2_table1,
               day2_make_table1(baseline_pop = baseline_pop),
               packages = "Publish"),
    # Cox regression
    tar_target(hazard_ratio,{
        fit = coxph(Surv(time,event)~bb+age+sex+any.malignancy+diabetes.with.complications,
                    data = baseline_pop)
        fit$call$data <- baseline_pop
        publish(fit)
    }, packages = c("survival","Publish")),
    # average treatment effect
    tar_target(risk_ratio,{
        fit = coxph(Surv(time,event)~bb+age+sex+any.malignancy+diabetes.with.complications,
                    data = baseline_pop,x = TRUE)
        x = ate(fit,data = baseline_pop,treatment = "bb",times = c(5,10,15),verbose = FALSE)
        x$ratioRisk[]
    }, packages = c("survival","riskRegression")),
    # day 3
    # random forests
    tar_target(forest,{
        forest = ranger(Surv(time,event)~bb+age+sex+any.malignancy+diabetes.with.complications,
                     data = baseline_pop,num.trees = 5)
        forest
    }, packages = c("survival","ranger"))    
)



######################################################################
### secret_targets.R ends here
