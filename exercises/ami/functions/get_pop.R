### get_pop.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2022 (07:40) 
## Version: 
## Last-Updated: Dec 11 2022 (17:58) 
##           By: Thomas Alexander Gerds
##     Update #: 9
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
get_pop <- function(raw_lpr_file,icd_codes){
    pop <- fread(raw_lpr_file,
                 keepLeadingZeros = TRUE)
    # extract everyone with AMI
    pop <- pop[grep(paste0("^",icd_codes$myocardial.infarction,collapse = "|"),diag)]
    pop <- pop[pop[,.I[1],by=c("pnr")]$V1]
    pop[,.(pnr,index = inddto)]
}


######################################################################
### get_pop.R ends here
