### run_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  7 2023 (08:37) 
## Version: 
## Last-Updated: Dec 10 2023 (07:21) 
##           By: Thomas Alexander Gerds
##     Update #: 25
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
library(Publish)
library(gdata)

# load functions
tar_source("functions")

# list the targets
tar_manifest()
# plot the dependency graf
tar_visnetwork(targets_only = TRUE)
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
tar_visnetwork(targets_only = TRUE)

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
tar_load_everything()
# plot the dependency graf
tar_visnetwork(targets_only = TRUE)
# look at a specific target
table_1

# the variables age and biomarker are
# summarized by mean and standard deviation
# to use median and IQR instead
# change the formula for table_1
# in the _targets.R file to be
# formula <- treatment~Q(age)+sex+Q(biomarker)
# then save the file and run
tar_make()
tar_load(table_1)
table_1

# delete the files in subfolder 'export'
file.remove("export/table1.csv")
file.remove("export/figure1.pdf")
# delete the targets that create these files
tar_delete(table_1)
tar_delete(figure_1)
# run tar_make and check if the files in the 'export' subfolder
# have been created
tar_make()

# remove failed and not anymore existing targets
tar_prune()

# load all targets
tar_load_everything()
# look at a specific target
figure_1

# look at the computation times, seeds and warnings/errors
show_performance()
show_warnings()
tar_meta()


######################################################################
### run_targets.R ends here
