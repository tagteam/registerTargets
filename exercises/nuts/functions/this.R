### this.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  6 2022 (10:51) 
## Version: 
## Last-Updated: Dec  8 2022 (13:52) 
##           By: Thomas Alexander Gerds
##     Update #: 21
#----------------------------------------------------------------------
## 
### Commentary: 
##  This is a function which takes one argument, the sample size n,
##  and returns a data.table with two columns: 'id' and 'sex'. 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
this <- function(n){
    d = data.table(id = 1:n,
                   sex = rep(c("female","male"),length.out = n),
                   age = rnorm(n = n,mean = 70,sd =7))
    ## warning("1 = 2")
    ## 1 = 2
    return(d)
}

######################################################################
### this.R ends here
