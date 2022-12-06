### get_pop.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2022 (07:40) 
## Version: 
## Last-Updated: Dec  6 2022 (07:48) 
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
get_pop <- function(raw_data_path,icd_codes){
    pop <- fread(paste0(raw_data_path,"/","lpr.csv"),
                 keepLeadingZeros = TRUE)
    pop <- pop[grep(icd_codes$MI,diag)]
    pop[,.(pnr,index = inddto)]
}


######################################################################
### get_pop.R ends here
