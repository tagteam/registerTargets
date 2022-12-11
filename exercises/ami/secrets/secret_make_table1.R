### secret_make_table1.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 10 2022 (15:17) 
## Version: 
## Last-Updated: Dec 11 2022 (19:34) 
##           By: Thomas Alexander Gerds
##     Update #: 19
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
secret_make_table1 <- function(baseline_pop){
    t1 <- utable(sex~age+ myocardial.infarction + heart.failure + peripheral.vascular.disease + cerebrovascular.disease + dementia + chronic.pulmonary.disease + rheumatic.disease + peptic.ulcer.disease + mild.liver.disease + severe.liver.disease + diabetes.without.complications + diabetes.with.complications + hemiplegia.paraplegia + renal.disease + any.malignancy + metastatic.solid.tumor + AIDS.HIV + leukemia + lymphoma,
                 data = baseline_pop)
    summary(t1,drop.reference = "binary")
}

######################################################################
### secret_make_table1.R ends here
