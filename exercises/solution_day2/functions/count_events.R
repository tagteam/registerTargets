### count_events.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 10 2022 (10:14) 
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
count_events <- function(data,event_var){
    sum(data[[event_var]])
}
######################################################################
### count_events.R ends here
