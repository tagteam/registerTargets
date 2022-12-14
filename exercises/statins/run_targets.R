### run_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 12 2022 (07:46) 
## Version: 
## Last-Updated: Dec 14 2022 (18:51) 
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
table_ltmle_fit_glm
table_ltmle_fit_glmnet


######################################################################
### run_targets.R ends here
