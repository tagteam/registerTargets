### get_table_1.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2023 (15:43) 
## Commentary: Create table 1
#----------------------------------------------------------------------
## 
### Code:
get_table_1 <- function(data,formula){
    # create table 1 based on formula and data
    summary(utable(formula,data))
}

######################################################################
### get_table_1.R ends here
