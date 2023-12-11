# outcome model
ltmle_fit_death_1[[1]][[1]]$Ltmle_fit$fit$Q[[1]]
# censoring model
ltmle_fit_death_1[[1]][[1]]$Ltmle_fit$fit$g[[1]][["Drug_0"]]
# propensity score model
ltmle_fit_death_1[[1]][[1]]$Ltmle_fit$fit$g[[1]][["Drug_0"]]

baseline_covariates[,education:=factor(education,levels=c("Basic","Medium","High"),labels=c("Basic","Medium","High"))]
baseline_covariates[,agegroups:=factor(agegroups,levels=c("below 45","45-50","50-55","55-60","60-65","65-70","70-75","75-80","80-85","above 85"),labels=c("below 45","45-50","50-55","55-60","60-65","65-70","70-75","75-80","80-85","above 85"))]
baseline_covariates[,tertile_income:=factor(tertile_income,levels=c("Income_q1","Income_q2","Income_q3"),labels=c("Income_q1","Income_q2","Income_q3"))]
baseline_covariates[,diabetes_duration:=factor(diabetes_duration,levels=c("below 5","5-10","above 10"),labels=c(,"below 5","5-10","above 10"))]

# 4 outcome model
ltmle_fit_death_2[[1]][[1]]$Ltmle_fit$fit$Q[[1]][["Dead_4"]]
ltmle_fit_death_2[[1]][[1]]$Ltmle_fit$fit$Q[[1]][["Dead_3"]]
ltmle_fit_death_2[[1]][[1]]$Ltmle_fit$fit$Q[[1]][["Dead_2"]]
ltmle_fit_death_2[[1]][[1]]$Ltmle_fit$fit$Q[[1]][["Dead_1"]]
# 3 censoring models
ltmle_fit_death_2[[1]][[1]]$Ltmle_fit$fit$g[[1]][["Censored_3"]]
ltmle_fit_death_2[[1]][[1]]$Ltmle_fit$fit$g[[1]][["Censored_2"]]
ltmle_fit_death_2[[1]][[1]]$Ltmle_fit$fit$g[[1]][["Censored_1"]]
# 3 propensity score models
ltmle_fit_death_2[[1]][[1]]$Ltmle_fit$fit$g[[1]][["Drug_3"]]
ltmle_fit_death_2[[1]][[1]]$Ltmle_fit$fit$g[[1]][["Drug_2"]]
ltmle_fit_death_2[[1]][[1]]$Ltmle_fit$fit$g[[1]][["Drug_1"]]
