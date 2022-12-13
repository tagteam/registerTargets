### run_solution_day_1.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 12 2022 (16:48) 
## Version: 
## Last-Updated: Dec 12 2022 (16:56) 
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
tar_make(script = "solutions_day1.R")
tar_load_everything()
study_pop

######################################################################
### run_solution_day_1.R ends here
