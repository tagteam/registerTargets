### run_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2022 (11:06) 
## Version: 
## Last-Updated: Dec  6 2022 (11:08) 
##           By: Thomas Alexander Gerds
##     Update #: 1
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
tar_manifest()
tar_visnetwork(targets_only = TRUE)
tar_make()
tar_load(bp)
print(bp)


######################################################################
### run_targets.R ends here
