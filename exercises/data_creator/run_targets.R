### run_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2022 (16:37) 
## Version: 
## Last-Updated: Dec 10 2022 (10:39) 
##           By: Thomas Alexander Gerds
##     Update #: 2
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

library(targets)
tar_make()
tar_load_everything()
ami_pop

tmp <- demo[ami_pop,on = "pnr"]
tmp[end_fup<index]



######################################################################
### run_targets.R ends here
