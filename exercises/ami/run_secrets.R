### run_secrets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2022 (17:34) 
## Version: 
## Last-Updated: Dec  9 2022 (08:23) 
##           By: Thomas Alexander Gerds
##     Update #: 3
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
table1
######################################################################
### run_secrets.R ends here
