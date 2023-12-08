### summary.runLtmle.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2023 (10:19) 
## Version: 
## Last-Updated: Dec  8 2023 (18:55) 
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
summary.runLtmle <- function(object,time_horizon,regimen){
    if (missing(time_horizon))
        time_horizon = names(object)
    else
        time_horizon = paste0("time_horizon_",time_horizon)
    if (missing(regimen)) regimen = names(object[[1]])
    do.call(rbind,lapply(time_horizon,function(tk){
        do.call(rbind,lapply(regimen,function(r){
            cbind(time_horizon = tk,regimen = r,summary(object[[tk]][[r]]$Ltmle_fit))
    }))}))
}
######################################################################
### summary.runLtmle.R ends here
