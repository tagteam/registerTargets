### run_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2022 (11:06) 
## Version: 
## Last-Updated: Dec  6 2022 (12:56) 
##           By: Thomas Alexander Gerds
##     Update #: 6
#----------------------------------------------------------------------
## 
### Commentary:
## This file applies some basic functions of the targets
## package to the project 'nuts' (which obviously is short for 'nutshell')
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(targets)
# list the targets
tar_manifest()
# plot the dependency graf
tar_visnetwork(targets_only = TRUE)
# list targets that have not run yet
tar_outdated()
# run the targets that need running
tar_make()
# load a specific target
tar_load(bp)
print(bp)

# delete a target long down the road
tar_delete(bp)
# list targets that have not run yet
tar_outdated()
# run the targets that need running
tar_make()

# delete a target further up the road
tar_delete(data)
# list targets that have not run yet
tar_outdated()
# run the targets that need running
tar_make()

# read a specific target
tar_read(answer)

######################################################################
### run_targets.R ends here
