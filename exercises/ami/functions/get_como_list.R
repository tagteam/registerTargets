### get_como_list.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 11 2022 (12:58) 
## Version: 
## Last-Updated: Dec 13 2022 (15:31) 
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
get_como_list <- function(icd_codes){
    lpr <- fread("rawdata/lpr.csv",
                 keepLeadingZeros = TRUE,
                 colClasses = c("character","character","Date"))
    x = lapply(names(icd_codes),function(disease){
        out = lpr[grep(paste0(paste0("^",icd_codes[[disease]]),collapse = "|"),diag)]
        # remove duplicated entries with same admission date
        out <- out[out[,.I[1],by=c("pnr","inddto")]$V1]
        out[,X := disease]
        out[]
    })
    names(x) = names(icd_codes)
    x
}


######################################################################
### get_como_list.R ends here
