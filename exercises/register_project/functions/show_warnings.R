### show_warnings.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 10 2023 (07:01) 
## Version: 
## Last-Updated: Dec 11 2023 (17:00) 
##           By: Thomas Alexander Gerds
##     Update #: 14
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
show_warnings <- function(){
    m = tar_meta()
    data.table::setDT(m)
    m = m[type == "stem"]
    m = m[,.(Target = name,Warnings = warnings,Errors = error)]
    print(m)
    cat("\nReference:\n\nhttps://books.ropensci.org/targets/debugging.html#error-messages\n")
    invisible(m)
}
######################################################################
### show_warnings.R ends here
