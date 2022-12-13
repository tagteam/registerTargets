### get_rates.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 10 2022 (10:16) 
## Version: 
## Last-Updated: Dec 10 2022 (10:18) 
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
get_rates <- function(pop,event_var,time_var){
    events <- count_events(data = pop,event_var = "event")
    persontime <- count_persontime(data = pop,time_var = "time")
    rate <- calculate_rate(events = events,persontime = persontime)
    rate
}


######################################################################
### get_rates.R ends here
