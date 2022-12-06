### get_demo.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  4 2022 (11:50) 
## Version: 
## Last-Updated: Dec  6 2022 (10:08) 
##           By: Thomas Alexander Gerds
##     Update #: 9
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
get_study_pop <- function(pop,raw_data_path){
    demo = fread(paste0(raw_data_path,"/","cpr.csv"),
                keepLeadingZeros = TRUE)
    # sort both datasets by pnr
    setkey(pop,pnr)
    setkey(demo,pnr)
    # join: extract columns sex and birth_date from data 'demo' 
    study_pop = demo[pop]
    # dates
    study_pop[,birth_date:=as.Date(birth_date,format="%Y-%m-%d")]
    study_pop[,index:=as.Date(index,format="%Y-%m-%d")]
    # exclusion criteria
    
    # calculate age 
    study_pop[,age := round(as.numeric(index-birth_date)/365.25,2)]
    # factor coding
    study_pop[,sex:=factor(sex,levels=c("0","1"),labels=c("Female","Male"))]
    # exercise: exclude patients who are younger than 40
    study_pop[]
}


######################################################################
### get_demo.R ends here
