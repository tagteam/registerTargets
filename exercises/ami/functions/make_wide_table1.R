### make_wide_table1.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 14 2022 (08:44) 
## Version: 
## Last-Updated: Dec 14 2022 (08:57) 
##           By: Thomas Alexander Gerds
##     Update #: 3
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

make_wide_table1 <- function(wide_baseline_pop){
    t1 <- utable(sex~age+ myocardial.infarction + heart.failure + renal.disease + any.malignancy + lymphoma + bb + ccb + statin,
                 data = wide_baseline_pop)
    summary(t1,drop.reference = "binary")
}
######################################################################
### make_wide_table1.R ends here
