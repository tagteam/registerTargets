### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2022 (10:30) 
## Version: 
## Last-Updated: Dec  6 2022 (11:16) 
##           By: Thomas Alexander Gerds
##     Update #: 16
#----------------------------------------------------------------------
## 
### Commentary: 
##  This is a very basic demonstration of the functionality of the R-package 
##  targets. See https://books.ropensci.org/targets/
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
#
# The R-package targets 
#
# The core of a targets-project is the file _targets.R
# It starts by loading the library
library(targets)
#
# Load project specifics
#
# Source functions that are best outsourced in separate files
# such that the file get_this.R defines the R-function 'this'.
# The name of the folder can have different names according to coders taste.
# The following code assumes that all R-code files are saved in a folder called
# ="functions/"=
for (f in list.files("functions/",
                     pattern = "R$",
                     full.names = TRUE)){source(f)}
#
# Targets
#
# Targets are R-objects which consist of a name and a value
# They are created using the function tar_target. For example,
# the following defines a target called NUTS which consists of
# the name 'n' and the value 137.
NUTS <- tar_target(n,{137})
# Here is another target called BARPLOT
BARPLOT <- tar_target(bp,{
    subset = data[age<70]
    subset[,mycol := rep(cbPalette,length.out = .N)]
    ggplot(subset,aes(x = age,fill = mycol))+geom_histogram()
},packages = c("ggplot2","data.table"))
#
# The pipeline
#
# The targets pipeline is a list of targets. The targets can be pre-defined
# like 'NUTS' and BARPLOT above or defined directly in the pipeline (in which case
# target does not have an R-object name:
list(NUTS,
     tar_target(data,{this(n)},packages = "data.table"),
     tar_target(cbPalette,c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")),
     BARPLOT
     )
#
# Continue reading in the file nuts/run_targets.R
# 

######################################################################
### _targets.R ends here
