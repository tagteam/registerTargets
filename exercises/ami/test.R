### test.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 14 2022 (10:10) 
## Version: 
## Last-Updated: Dec 14 2022 (11:02) 
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
library(targets)
library(riskRegression)
list(
    tar_target(d,{
        sampleData(473,outcome = "survival")
    }),
    tar_target(cox_fit,{
        fit = coxph(Surv(time,event)~X1+X8,
                    data = d,x = TRUE)
    },packages = "survival"),
    tar_target(cox_table,{
        cox_fit
        cox_fit$call$data <- d
        publish(cox_fit)
    }, packages = c("survival","Publish"))
)

tar_deps({
    a
})
tar_deps({
    a
    a$v = 1
})

tar_deps({
    b <- a
    b$v = 1
})

######################################################################
### test.R ends here
