### run_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  7 2023 (08:37) 
## Version: 
## Last-Updated: Dec  7 2023 (09:08) 
##           By: Thomas Alexander Gerds
##     Update #: 16
#----------------------------------------------------------------------
## 
### Commentary: 
## 
## This file runs some of the most important functions of the targets
## package in the project 'example_project'. 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(targets)
# list the targets
tar_manifest()
# plot the dependency graf
tar_visnetwork()
# list targets that have not run yet
tar_outdated()
# run the targets that need running
tar_make()
# load a specific target
tar_load(table_2)
print(table_2)
# read a specific target into an object
t_2 <- tar_read(table_2)
print(t_2)

# delete a specific target further down in the pipeline
tar_delete(table_2)
# list targets that have not run yet
tar_outdated()
# plot the dependency graf
tar_visnetwork()

# run the targets that need running using a new R process
# in the background
tar_make()
# run the targets that need running in the current R session
# first clean the global environment
rm(list = ls())
tar_make(callr_function = NULL)

# delete a target further up the pipeline
tar_delete(data)
# list targets that have not run yet
tar_outdated()
# plot the dependency graf
tar_visnetwork()
# run a specific target which need running
tar_make(table_1)
# plot the dependency graf
tar_visnetwork()

# delete the files in subfolder 'export'
file.remove("export/table1.csv")
file.remove("export/figure1.pdf")
# delete the targets that create these files
tar_delete(table_1)
tar_delete(figure_1)
# run tar_make and check if the files in the 'export' subfolder
# have been created
tar_make()

# go back to exercise file and continue Exercise 3
# https://github.com/tagteam/registerTargets/blob/main/exercises/targeted-exercises-day1-part2.org
######################################################################
### run_targets.R ends here
