### run_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  7 2022 (11:31) 
## Version: 
## Last-Updated: Dec  7 2022 (11:55) 
##           By: Thomas Alexander Gerds
##     Update #: 4
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

library(targets)

# See status
tar_manifest()
tar_visnetwork(targets_only = TRUE)
tar_outdated()

# Run targets
tar_make()

# As you have seen the last target of the pipeline is table1.
# Since this project has been run before, you can directly load/read
# the table:
t1 <- tar_read(table1)
t1

# Open the file =functions/get_study_pop.R= (i.e., put cursor on
# function name and press F2). Find the comment # exclusion criteria
# and enter the following line (add a comment about what the line does):
study_pop[index >= study_start]

# Save the file (Control-s)

# See which targets are now out of date due to this change 
tar_outdated()
#optional: tar_visnetwork()

# Run the part of the pipeline which needs to be run
tar_make()

# Read the new table:
t1a <- tar_read(table1)

# Compare the new table against the one before the change
t1
t1a

######################################################################
### run_targets.R ends here
