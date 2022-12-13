### count_persontime.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 10 2022 (10:15) 
## Version: 
## Last-Updated: Dec 10 2022 (10:15) 
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
count_persontime <- function(data,time_var){
    sum(data[[time_var]])
}
######################################################################
### count_persontime.R ends here
