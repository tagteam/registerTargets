### calculate_rate.R --- 
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
calculate_rate <- function(events, persontime, scale = 1000){
    scale*events/persontime
}
######################################################################
### calculate_rate.R ends here
