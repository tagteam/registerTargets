### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 10 2022 (10:13) 
## Version: 
## Last-Updated: Dec 13 2022 (15:09) 
##           By: Thomas Alexander Gerds
##     Update #: 25
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(survival)
library(targets)
library(prodlim)
library(data.table)
for (f in list.files("functions/",
                     pattern = "R$",
                     full.names = TRUE)){source(f)}
list(
    tar_target(pop,{
        pop <- fread("data/popami.csv",keepLeadingZeros = TRUE,
                     colClasses = c("character","character",rep("Date",5),"numeric"))
        # define event time
        pop[,sex:=factor(sex,levels=c("Male","Female"),labels=c("Male","Female"))]
        pop[,time := as.numeric(end_fup-index)/365.25]
        pop[,event := 0]
        pop[!is.na(death_date),event := 1]
        pop[]
    }),
    # subset data for analysis
    tar_target(pop_male,{
        pop[pop$sex == "Male",c("pnr","time","event","sex","age")]
    }),
    tar_target(pop_female,{
        pop[pop$sex == "Female",c("pnr","time","event","sex","age")]
    }),
    tar_target(rate_female,{
        get_rates(pop_female,event_var = "event",time_var = "time")
    }),
    tar_target(rate_male,{
        get_rates(pop_male,event_var = "event",time_var = "time")
    }), 
    # calculate transplant-free mortality rates 
    tar_target(rate_ratio,{
        rate_ratio <- rate_male/rate_female
        rate_ratio
    }),
    tar_target(cox,{
        # compare with Cox regression under proportional hazard assumption
        coxph(Surv(time,event)~sex,data = pop)
    }),
    tar_target(potential_followup_time,{
        # calculate potential followup time (REF: Michael Schemper and Terry L Smith. A note on quantifying follow-up in
        # studies of. Controlled clinical trials, 17:343--346, 1996.
        # Reverse Kaplan-Meier for probability of not being censored, i.e., probability of
        # potentially being followed during the study period. Potentially, because death and
        # other outcomes may actually stop the followup
        G <- prodlim::prodlim(Hist(time,event)~1,data = pop,reverse = TRUE)
        quantile(G)    
    }),
    tar_target(km,{
        # Kaplan-Meier estimator
        km <- prodlim::prodlim(Hist(time,event)~sex,data = pop)
        km
    }),
    tar_target(kmplot,{
        pdf("figures/Kaplan-Meier-plot.pdf")
        plot(km)
        dev.off()
    }),
    tar_target(oneyear_risk,{predict(km,type = "risk",times = 1,newdata = data.table(sex = c("Male","Female")))}),
    tar_target(fiveyear_risk,{predict(km,type = "risk",times = 5,newdata = data.table(sex = c("Male","Female")))}),
    tar_target(tenyear_risk,{predict(km,type = "risk",times = 10,newdata = data.table(sex = c("Male","Female")))}),
    tar_target(oneyear_risk_ratio,{
        # calculate 1-year risk ratios
        oneyear_risk_ratio <- oneyear_risk[['sex=Male']]/oneyear_risk[['sex=Female']]
        oneyear_risk_ratio    
    }),
    tar_target(fiveyear_risk_ratio,{
        # calculate 5-year risk ratios
        fiveyear_risk_ratio <- fiveyear_risk[['sex=Male']]/fiveyear_risk[['sex=Female']]
        fiveyear_risk_ratio    
    }),
    tar_target(tenyear_risk_ratio,{
        # calculate 10-year risk ratios
        tenyear_risk_ratio <- tenyear_risk[['sex=Male']]/tenyear_risk[['sex=Female']]
        tenyear_risk_ratio    
    }),
    tar_target(tableRR,{
        tableRR <- data.table(Female = c(rate_female,oneyear_risk[["sex=Female"]],fiveyear_risk[["sex=Female"]],tenyear_risk[["sex=Female"]]),
                              Male = c(rate_male,oneyear_risk[["sex=Male"]],fiveyear_risk[["sex=Male"]],tenyear_risk[["sex=Male"]]),
                              Ratio = c(rate_ratio,oneyear_risk_ratio,fiveyear_risk_ratio,tenyear_risk_ratio))
        fwrite(tableRR,file = "tables/tableRR.csv")
        tableRR[]
    })
)




######################################################################
### _targets.R ends here
