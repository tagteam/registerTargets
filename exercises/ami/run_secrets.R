### run_secrets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2022 (17:34) 
## Version: 
## Last-Updated: Dec 10 2022 (15:43) 
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
library(targets)
tar_make(script = "secret_targets.R")
tar_load_everything()
setkey(lpr_pop[[1]],pnr)
lpr_pop[[1]]
table1
######################################################################
### run_secrets.R ends here
