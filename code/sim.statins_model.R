### sim.statins_model.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  5 2022 (10:22) 
## Version: 
## Last-Updated: Dec  5 2022 (10:25) 
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
sim.statins_model <- function(m,N,seed){
    requireNamespace("data.table")
    requireNamespace("lava")
    class(m) = "lvm"
    d <- data.table::setDT(lava::sim(m,N))
    d[CVD_1 == 1,CVD_2 := 1]
    d[]
}


######################################################################
### sim.statins_model.R ends here
