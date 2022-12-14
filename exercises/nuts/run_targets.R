### run_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2022 (11:06) 
## Version: 
## Last-Updated: Dec  8 2022 (14:20) 
##           By: Thomas Alexander Gerds
##     Update #: 10
#----------------------------------------------------------------------
## 
### Commentary:
## 
## This file runs some of the most important functions of the targets
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

# go back to exercise file and continue Exercise 3
# https://github.com/tagteam/registerTargets/blob/main/exercises/targeted-exercises-day1.org

######################################################################
### run_targets.R ends here
