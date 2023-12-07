### analyse_data.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2023 (15:49) 
## Commentary: 
###  Run Cox regression and format as table
## Code:
analyse_data <- function(data,formula){
    fit = coxph(formula,data,x = TRUE,y = TRUE)
    publish(fit,print = FALSE)
}


######################################################################
### analyse_data.R ends here
