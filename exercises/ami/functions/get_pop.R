### get_pop.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2022 (07:40) 
## Version: 
## Last-Updated: Dec  8 2022 (17:59) 
##           By: Thomas Alexander Gerds
##     Update #: 6
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
    # extract everyone with AMI
    pop <- pop[grep(paste0("^",icd_codes$MI),diag)]
    pop <- pop[pop[,.I[1],by=c("pnr")]$V1]
    pop[,.(pnr,index = inddto)]
}


######################################################################
### get_pop.R ends here
