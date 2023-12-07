### generate_data.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  7 2023 (13:31) 
## Version: 
## Last-Updated: Dec  7 2023 (13:54) 
##           By: Thomas Alexander Gerds
##     Update #: 15
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
generate_data <- function(sample_size){
    m <- lava::lvm()
    lava::distribution(m, ~X1) <- lava::binomial.lvm(p = c(0.3))
    lava::distribution(m, ~X2) <- lava::binomial.lvm(p = c(0.01))
    lava::distribution(m, ~X3) <- lava::normal.lvm(mean = 60,sd = 15)
    lava::distribution(m, ~X4) <- lava::normal.lvm(mean = 50,sd = 25)
    lava::distribution(m, "eventtime") <- lava::coxWeibull.lvm(scale = 1/100)
    lava::distribution(m, "censtime") <- lava::coxWeibull.lvm(scale = 1/1000)
    m <- lava::eventTime(m,time ~ min(eventtime = 1, censtime = 0),"event")
    # effect on treatment variable 
    lava::regression(m) <- X2~ f(X1, 1) + f(X3, 0.1) 
    # effect on biomarker
    lava::regression(m) <- X4~ f(X1, 1) + f(X3, 0.1) + f(X2,-1)
    # effect on event time outcome 
    lava::regression(m) <- eventtime~f(X1, 1) + f(X2, -0.033) + f(X3, 0.01) + f(X4, -0.1)
    # effect on censoring time  
    lava::regression(m) <- censtime~f(X1, .5) + f(X2, 0) + f(X3, 0.01) + f(X4, -0.05)
    d <- sim(m,sample_size)
    d <- data.table::as.data.table(d)
    d[]
}


######################################################################
### generate_data.R ends here
