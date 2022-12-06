### make_table1.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  4 2022 (11:31) 
## Version: 
## Last-Updated: Dec  4 2022 (12:40) 
##           By: Thomas Alexander Gerds
##     Update #: 4
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
make_table1 <- function(pop){
    t1 <- utable(sex~age,data = pop)
    summary(t1)
}


######################################################################
### make_table1.R ends here
