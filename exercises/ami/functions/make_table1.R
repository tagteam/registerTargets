### make_table1.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  4 2022 (11:31) 
## Version: 
## Last-Updated: Dec 10 2022 (15:18) 
##           By: Thomas Alexander Gerds
##     Update #: 5
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
make_table1 <- function(study_pop){
    t1 <- utable(sex~age,data = study_pop)
    summary(t1)
}


######################################################################
### make_table1.R ends here
