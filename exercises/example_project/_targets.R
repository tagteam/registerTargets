### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2023 (14:55) 
## Version: 
## Last-Updated: Dec 11 2023 (09:46) 
##           By: Thomas Alexander Gerds
##     Update #: 44
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
#
# This script defines an example targets project
#
#
library(targets)

# R-package dependencies
tar_option_set(packages=c("prodlim","foreach","riskRegression","data.table","lava","Publish"))

# Definition of project functions
tar_source("functions")
# Modified R-package ltmle
source("../Ltmle/Ltmle.R")

# Constants which are not saved as targets
# can be defined here
cbPalette <- c("#999999","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
# Note cbPalette defines color blind friendly colors

#
# The pipeline of this example project has 6 targets:
# 
#   1. sample_size (just a single value)
#   2. rawdata (apply function generate_data)
#   3. data (apply function prepare_data)
#   4. table_1 (few lines of code inside {})
#   5. table_2 (apply function analyse_data)
#   6. figure_1 (apply function make_barplot, load also R-package ggplot2)
#
list(tar_target(sample_size,{137}),
     tar_target(rawdata,{
         generate_data(sample_size)
     }),
     tar_target(data, {
         prepare_data(rawdata = rawdata)
     }),
     tar_target(table_1, {
         formula <- treatment~age+sex+biomarker
         t1 <- summary(utable(formula,data))
         fwrite(t1,"export/table1.csv")
         t1
     }),
     tar_target(figure_1,{
         print("hi")
         subset = data[age<70]
         subset[,mycol := rep(cbPalette,length.out = .N)]
         g = ggplot(subset,aes(x = age,fill = mycol))+geom_histogram()
         ggsave(g,file = "export/figure1.pdf")
         g
     },packages = c("data.table","ggplot2")),
     tar_target(table_2, {
         analyse_data(data = data,
                      formula = Surv(time,event)~sex+age+treatment+biomarker)
     },packages = "survival")
     )



######################################################################
### _targets.R ends here
