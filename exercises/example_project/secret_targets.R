### secret_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2023 (14:55) 
## Version: 
## Last-Updated: Dec 10 2023 (07:47) 
##           By: Thomas Alexander Gerds
##     Update #: 42
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
#
# The R-package targets 
#
# This script defines an example targets project
#
library(targets)

# R-package dependencies
tar_option_set(packages=c("prodlim","foreach","riskRegression","data.table","lava","Publish"))

# Definition of project functions
tar_source("functions")
tar_source("secret_functions")

# Definition of augmented Ltmle functions
tar_source("../Ltmle")

# Constants which are not saved as targets
# can be defined here
cbPalette <- c("#999999","#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
# Note cbPalette defines color blind friendly colors

#
# The pipeline of this example project has 6 targets:
# 1. sample_size (just a single value)
# 2. rawdata (few lines of code inside {})
# 3. data (apply function prepare_data)
# 4. table_1 (apply function get_table_1)
# 5. table_2 (apply function analyse_data)
# 5. figure_1 (apply function make_barplot, load also R-package ggplot2)
#

list(tar_target(sample_size,{137}),
     tar_target(rawdata,{
         generate_data(sample_size)
     }),
     tar_target(data, {
         prepare_data(rawdata = rawdata)
     }),
     tar_target(table_1, {
         t1 <- get_table_1(data = data,
                           formula = treatment~Q(age)+sex+Q(biomarker))
         fwrite(t1,"export/table1.csv")
         t1
     }),
     tar_target(table_2, {
         analyse_data(data = data,
                      formula = Surv(time,event)~sex+age+treatment+biomarker)
     },packages = "survival"),
     tar_target(propensity_score_table,{
         get_propensity_score_table(formula,data)
     }),
     tar_target(figure_1,{
         subset = data[age<70]
         subset[,mycol := rep(cbPalette,length.out = .N)]
         g = ggplot(subset,aes(x = age,fill = mycol))+geom_histogram()
         ggsave(g,file = "export/figure1.pdf")
         g
     },packages = c("ggplot2")),
     tar_target(ltmle_analysis,{
         data[,Treatment := as.numeric(treatment == "0")]
         Ltmle(data = data[,.(sex,age,Treatment,biomarker)],
               Anodes="Treatment",
               Lnodes=c("sex","age"),
               Ynodes=c("biomarker"),
               Yrange=c(0,100),
               abar=list(0,1),
               time_horizon = 1,
               SL.library="glm")
     }),
     tar_target(summary_ltmle,summary(ltmle_analysis))
     )



######################################################################
### secret_targets.R ends here
