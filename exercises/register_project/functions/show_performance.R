### show_performance.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 10 2023 (07:01) 
## Version: 
## Last-Updated: Dec 10 2023 (16:00) 
##           By: Thomas Alexander Gerds
##     Update #: 13
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
show_performance <- function(){
    requireNamespace("gdata")
    m = tar_meta()
    data.table::setDT(m)
    m = m[type == "stem"]
    m[,Memory := gdata::humanReadable(bytes)]
    m[,Minutes := seconds%/%(60)]
    m[,Hours := seconds%/%(60*60)]
    m = m[,.(Target = name,Hours,Minutes,Seconds = seconds,Memory,Storage = format)]
    if (all(m$Hours == 0)) m$Hours = NULL else m$Seconds = NULL
    print(m)
    cat("\nReferences:\n\nhttps://books.ropensci.org/targets/performance.html\n",
        "https://docs.ropensci.org/targets/reference/tar_target.html#storage-formats\n",sep = "")
    invisible(m)
}
######################################################################
### show_performance.R ends here
