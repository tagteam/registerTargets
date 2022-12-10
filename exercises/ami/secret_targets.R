### secret_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2022 (17:28) 
## Version: 
## Last-Updated: Dec 10 2022 (09:55) 
##           By: Thomas Alexander Gerds
##     Update #: 21
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
    tar_target(icd_codes, c(list(
                              # myocardial infarction
                              "MI" = c("DI2[12]")),
                            heaven::charlson.codes)),
    # prescription data: exposure and comedicine 
    tar_target(atc_codes,list(#beta blockers
                             bb = c('C07'),
                             #calcium chanel blockers
                             ccb = c('C08'))),
    # define study population
    tar_target(pop, get_pop(raw_data_path = raw_data_path,
                            icd_codes = icd_codes)),
    # lpr
    tar_target(lpr_pop,{
        icd_codes
        lpr <- fread(paste0(raw_data_path,"lpr.csv"))
        lapply(names(icd_codes),function(disease){
            out = lpr[grep(paste0(paste0("^",icd_codes[[disease]]),collapse = "|"),diag)]
            # remove duplicated entries with same admission date
            out <- out[out[,.I[1],by=c("pnr","inddto")]$V1]
            out[,X := disease]
            out[]
        })
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
               packages = "Publish")
)



######################################################################
### secret_targets.R ends here
