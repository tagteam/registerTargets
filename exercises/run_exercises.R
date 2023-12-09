### run_exercises.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2023 (18:00) 
## Version: 
## Last-Updated: Dec  9 2023 (06:55) 
##           By: Thomas Alexander Gerds
##     Update #: 7
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(targets)
setwd("~/metropolis/Teaching/targetedRegisterAnalysis/exercises/data_creator/")
tar_destroy(ask = FALSE)
tar_make()
tar_load_everything()
setwd("~/metropolis/Teaching/targetedRegisterAnalysis/exercises/example_project/")
tar_destroy(ask = FALSE)
tar_make(script = "secret_targets.R")
tar_load_everything()
setwd("~/metropolis/Teaching/targetedRegisterAnalysis/exercises/register_project/")
tar_destroy(ask = FALSE)
tar_make(script = "secret_targets.R")
tar_load_everything()

######################################################################
### run_exercises.R ends here
