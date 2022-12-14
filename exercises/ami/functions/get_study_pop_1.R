### get_study_pop.R --
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  4 2022 (11:50) 
## Version: 
## Last-Updated: Dec 14 2022 (09:26) 
##           By: Thomas Alexander Gerds
##     Update #: 20
#----------------------------------------------------------------------
## 
### Commentary: 
## Read demographics and prepare study population
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
get_study_pop_1 <- function(pop,raw_cpr_file,study_start,study_end){
    demo = fread(raw_cpr_file,keepLeadingZeros = TRUE,
                 colClasses = c("character","character",
                                "Date","Date","Date"))
    # sort both datasets by pnr
    setkey(pop,pnr)
    setkey(demo,pnr)
    # join: extract columns sex and birth_date from data 'demo' 
    study_pop = demo[pop]
    # dates
    study_pop[,birth_date:=as.Date(birth_date,format="%Y-%m-%d")]
    study_pop[,index:=as.Date(index,format="%Y-%m-%d")]
    # exclusion criteria
    study_pop <- study_pop[index >= study_start]
    study_pop <- study_pop[index <= study_end]
    study_pop[,end_fup := pmin(emigration_date,death_date,study_end,na.rm = TRUE)]
    # calculate age 
    study_pop[,age := round(as.numeric(index-birth_date)/365.25,2)]
    # factor coding
    study_pop[,sex:=factor(sex,levels=c("0","1"),labels=c("Female","Male"))]
    # exercise: exclude patients who are younger than 40
    study_pop[]
}


######################################################################
### get_study_pop.R ends here
