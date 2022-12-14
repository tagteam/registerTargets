### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec  7 2022 (11:18) 
## Version: 
## Last-Updated: Dec 14 2022 (18:50) 
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
tar_option_set(packages = c("data.table","matrixStats","ggplot2","Publish","lava"))
for (f in list.files("functions/",pattern = "R$",recursive=TRUE,full.names = TRUE)){source(f)}
list(
    tar_target(N,{10000}),
    tar_target(data,{
        m = sample_statins_data()
        sim(m,N = N)}),
    # prepare data for ltmle
    tar_target(dd,{
        event_node_manipulator(data=data,k=2,outcome="CVD",competing=NULL,censored="Censored",outcome_is_competing=NULL)
    }),
    # run ltmle with glm
    tar_target(ltmle_fit_glm,{
        x <- Ltmle(data=dd,
                   Qform=c("CVD_1"="Q.kplus1 ~ sex + age + ldl + Statins_0 + Statins_1",
                           "CVD_2"="Q.kplus1 ~ sex + age + ldl + Statins_0 + Statins_1"),
                   gform=c("Statins_0"="Statins_0 ~ sex + age + ldl",
                           "Statins_1"="Statins_1 ~ sex + age + ldl + Statins_0",
                           "Censored_1"="Censored_1 ~ sex + age + ldl + Statins_0 + Statins_1",
                           "Statins_2"= "Statins_2 ~ sex + age + ldl + Statins_0 + Statins_1"),
                   Anodes=c("Statins_0","Statins_1","Statins_2"),
                   Lnodes=c("sex","age","ldl"),
                   Ynodes=c("CVD_1","CVD_2"),
                   Cnodes=c("Censored_1"),
                   estimate.time=FALSE,
                   survivalOutcome=TRUE,
                   variance.method="ic",
                   SL.library="glm",
                   abar=list(c(1,1,1),c(0,0,0)))
    }),
    # run ltmle with glmnet
    tar_target(ltmle_fit_glmnet,{
        Ltmle(data=dd,
              Qform=c("CVD_1"="Q.kplus1 ~ sex + age + ldl + Statins_0 + Statins_1","CVD_2"="Q.kplus1 ~ sex + age + ldl + Statins_0 + Statins_1"),
              gform=c("Statins_0"="Statins_0 ~ sex + age + ldl","Statins_1"="Statins_1 ~ sex + age + ldl + Statins_0","Censored_1"="Censored_1 ~ sex + age + ldl + Statins_0 + Statins_1","Statins_2"= "Statins_2 ~ sex + age + ldl + Statins_0 + Statins_1"),
              Anodes=c("Statins_0","Statins_1","Statins_2"),
              Lnodes=c("sex","age","ldl"),
              Ynodes=c("CVD_1","CVD_2"),
              Cnodes=c("Censored_1"),
              estimate.time=FALSE,
              survivalOutcome=TRUE,
              variance.method="ic",
              SL.library="glmnet",
              abar=list(c(1,1,1),c(0,0,0)),
              SL.cvControl=list(alpha=0.5,selector='undersmooth'),
              verbose=TRUE)
    }),
    # user friendly output 
    tar_target(table_ltmle_fit_glm,{
        publish(ltmle_fit_glm)
    }),
    tar_target(table_ltmle_fit_glmnet,{
        publish(ltmle_fit_glmnet)
    })
)



######################################################################
### _targets.R ends here
