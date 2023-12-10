### get_propensity_score_table.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 10 2023 (07:44) 
## Version: 
## Last-Updated: Dec 10 2023 (07:45) 
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
get_propensity_score_table <- function(formula,data){
    formula <- treatment~sex+age
    ps <- glm(formula,data=data,family="binomial")
    publish(ps,print=FALSE)
}


######################################################################
### get_propensity_score_table.R ends here
