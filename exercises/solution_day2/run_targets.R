### run_targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 13 2022 (14:53) 
## Version: 
## Last-Updated: Dec 13 2022 (15:10) 
##           By: Thomas Alexander Gerds
##     Update #: 2
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(targets)
tar_make()
tar_load(tableRR)
tableRR
fwrite(tableRR,file="tables/tableRR.csv")
tar_load(km)
pdf("figures/Kaplan-Meier-plot.pdf")
plot(km)
dev.off()


######################################################################
### run_targets.R ends here
