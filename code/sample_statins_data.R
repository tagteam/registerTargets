### sample_statins_data.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  5 2022 (10:14) 
## Version: 
## Last-Updated: Dec  5 2022 (10:22) 
##           By: Thomas Alexander Gerds
##     Update #: 3
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
sample_statins_data <- function(){
    source("exercises/code/synthesize_statins.R")
    cc <- fread("exercises/data/statin_coeffients.txt")
    ## vars <- c("sex","age","ldl","Statins_0","ASVCD_1","Statins_1","antihypertensive_1","diabetes_1","cancer_1","copd_1","Statins_2","antihypertensive_2","diabetes_2","cancer_2","copd_2","Statins_3","antihypertensive_3","diabetes_3","cancer_3","copd_3")
    vars <- c("sex","age","ldl","Statins_0","CVD_1","Statins_1","CVD_1","Censored_1","Statins_2","CVD_2")
    ## cc <- cc[,c("variable","(Intercept)","sex","age","ldl","Statins_0","antihypertensive_0","diabetes_0","cancer_0","copd_0","Statins_1","antihypertensive_1","diabetes_1","cancer_1","copd_1","Statins_2","antihypertensive_2","diabetes_2","cancer_2","copd_2","Statins_3","antihypertensive_3","diabetes_3","cancer_3","copd_3")]
    CC <- cc[variable%in%vars,c(1:2,unlist(sapply(vars,function(v)grep(paste0(v,"$"),names(cc))))),with=FALSE]
    CC[variable=="Censored_1",sex:=0.01]
    CC[variable=="Censored_1",age:=-0.01]
    CC[variable=="Censored_1",ldl:=0]
    CC[variable=="Censored_1",Statins_1:=0]
    CC[variable=="Censored_1",Intercept:=log(.1)]
    ## CC[variable=="Censored_1"]
    #
    CC[variable=="CVD_1",sex:=-0.2]
    CC[variable=="CVD_1",age:=0.05]
    CC[variable=="CVD_1",ldl:=0.01]
    CC[variable=="CVD_1",Statins_0:=-0.1]
    CC[variable=="CVD_1",Statins_1:=-0.5]
    CC[variable=="CVD_1",Intercept:=log(.001)]
    ## CC[variable=="CVD_1"]
    #
    CC[variable=="CVD_2",sex:=-0.2]
    CC[variable=="CVD_2",age:=0.05]
    CC[variable=="CVD_2",ldl:=0.01]
    CC[variable=="CVD_2",Statins_0:=-0.05]
    CC[variable=="CVD_2",Statins_1:=-0.1]
    CC[variable=="CVD_2",Statins_2:=-0.5]
    CC[variable=="CVD_2",Intercept:=log(.001)]
    ## CC[variable=="CVD_2"]
    #
    m <- synthesize_statins(CC,continuous=list("age"=list(sd=10),"ldl"=list(sd=0.5)))
    class(m) <- c("statins_model","lvm")
    m
}


######################################################################
### sample_statins_data.R ends here
