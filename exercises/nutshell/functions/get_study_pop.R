### get_demo.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  4 2022 (11:50) 
## Version: 
## Last-Updated: Dec  4 2022 (12:40) 
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
get_study_pop <- function(pop,raw_data_path){
    demo = fread(paste0(raw_data_path,"/","cpr.csv"),
                keepLeadingZeros = TRUE)
    # sort both datasets by pnr
    setkey(pop,pnr)
    setkey(demo,pnr)
    # join: extract columns sex and birth_date from data 'demo' 
    pop = demo[pop]
    # dates
    pop[,birth_date:=as.Date(birth_date,format="%Y-%m-%d")]
    pop[,index:=as.Date(index,format="%Y-%m-%d")]
    # calculate age 
    pop[,age := round(as.numeric(index-birth_date)/365.25,2)]
    # factor coding
    pop[,sex:=factor(sex,levels=c("0","1"),labels=c("Female","Male"))]
    # exercise: exclude patients who are younger than 40
    pop[]
}


######################################################################
### get_demo.R ends here
