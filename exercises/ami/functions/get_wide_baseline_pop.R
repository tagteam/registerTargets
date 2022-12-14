### get_wide_baseline_pop.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 14 2022 (08:45) 
## Version: 
## Last-Updated: Dec 14 2022 (08:46) 
##           By: Thomas Alexander Gerds
##     Update #: 1
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
get_wide_baseline_pop <- function(study_pop,como_list,drug_list){
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
    q = baseline_pop[bb == "Yes"&event == 1,.(pnr,bb,event)]
    baseline_pop[]
}


######################################################################
### get_wide_baseline_pop.R ends here
