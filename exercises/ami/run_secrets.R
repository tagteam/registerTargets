### run_secrets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2022 (17:34) 
## Version: 
## Last-Updated: Dec 14 2022 (09:31) 
##           By: Thomas Alexander Gerds
##     Update #: 6
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(targets)
tar_make(script = "solutions_day1.R")
tar_load_everything()

library(targets)
tar_make(script = "solutions_day3.R")
tar_load_everything()

tar_make(script = "secret_targets.R")
tar_load_everything()
setkey(lpr_pop[[1]],pnr)
lpr_pop[[1]]
table1
######################################################################
### run_secrets.R ends here
