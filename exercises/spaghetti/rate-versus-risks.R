### rate-versus-risks.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 10 2022 (08:52) 
## Version: 
## Last-Updated: Dec 10 2022 (15:01) 
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

# the list of packages should be added to the file _targets.R 
library(survival)
library(prodlim)
library(data.table)

# the data file is in the folder exercises/data/
# and needs to bee copied to a new subfolder called data
# in the new targets project folder  
pop <- fread("popami.csv",keepLeadingZeros = TRUE,
             colClasses = c("character","character",rep("Date",5),"numeric"))

# the R-code below should be organized in targets

# define event time
pop[,time := as.numeric(end_fup-index)/365.25]
pop[,event := 0]
pop[!is.na(death_date),event := 1]

# subset data for analysis
pop_male <- pop[pop$sex == "Male",c("pnr","time","event","sex","age")]
pop_female <- pop[pop$sex == "Female",c("pnr","time","event","sex","age")]

# calculate transplant-free mortality rates 
count_events <- function(data,event_var){
    sum(data[[event_var]])
}
count_persontime <- function(data,time_var){
    sum(data[[time_var]])
}
calculate_rate <- function(events, persontime, scale = 1000){
    scale*events/persontime
}


events_male <- count_events(data = pop_male,event_var = "event")
persontime_male <- count_persontime(data = pop_male,time_var = "time")
rate_male <- calculate_rate(events = events_male,persontime = persontime_male)

events_female <- count_events(data = pop_female,event_var = "event")
persontime_female <- count_persontime(data = pop_female,time_var = "time")
rate_female <- calculate_rate(events = events_female,persontime = persontime_female)

rate_ratio <- rate_male/rate_female
rate_ratio

# compare with Cox regression under proportional hazard assumption
Publish::publish(coxph(Surv(time,event)~sex,data = pop))

# calculate potential followup time (REF: Michael Schemper and Terry L Smith. A note on quantifying follow-up in
# studies of. Controlled clinical trials, 17:343--346, 1996.
# Reverse Kaplan-Meier for probability of not being censored, i.e., probability of
# potentially being followed during the study period. Potentially, because death and
# other outcomes may actually stop the followup
G <- prodlim::prodlim(Hist(time,event)~1,data = pop,reverse = TRUE)
quantile(G)
# The median potential followup time (interquartile range): 13.7 years (7.4 years;18.6 years).

# Kaplan-Meier estimator
km <- prodlim::prodlim(Hist(time,event)~sex,data = pop)
pdf("Kaplan-Meier-plot.pdf")
plot(km)
dev.off()

# calculate 5-year absolute risks of death and 5-year risk ratios
fiveyear_risk <- predict(km,times = 5,newdata = data.table(sex = c("Male","Female")))
fiveyear_risk_ratio <- fiveyear_risk[['sex=Male']]/fiveyear_risk[['sex=Female']]
fiveyear_risk_ratio

# calculate 10-year absolute risks of death and 10-year risk ratios
tenyear_risk <- predict(km,times = 10,newdata = data.table(sex = c("Male","Female")))
tenyear_risk_ratio <- tenyear_risk[['sex=Male']]/tenyear_risk[['sex=Female']]
tenyear_risk_ratio

tableRR <- c("Rate ratio" = rate_ratio,
             "5 year risk ratio" =  fiveyear_risk_ratio,
             "10 year risk ratio" =  tenyear_risk_ratio)
fwrite(tableRR,file = "tableRR.csv")

             


######################################################################
### rate-versus-risks.R ends here
