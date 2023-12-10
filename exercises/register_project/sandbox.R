### sandbox.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  8 2023 (19:05) 
## Version: 
## Last-Updated: Dec 10 2023 (08:14) 
##           By: Thomas Alexander Gerds
##     Update #: 9
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(targets)
tar_source("functions")
tar_make()
tar_manifest()
tar_visnetwork()
show_performance()
show_warnings()
tar_load_everything()

#--------------------------------------------------------
# day 2
#--------------------------------------------------------

# part 1
tar_load(raw_baseline_covariates)
print(raw_baseline_covariates)

tar_make()
tar_load(baseline_covariates)

# adapt the table_1 target defined in example_project (yesterday exercise)

#--------------------------------------------------------
# day 3
#--------------------------------------------------------

#--------------------------------------------------------
# day 4
#--------------------------------------------------------


######################################################################
### sandbox.R ends here
