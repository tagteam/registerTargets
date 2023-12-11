tar_load_everything()
library(ranger)
library(riskRegression)
# avoid that ranger treats our 0-1 variable Drug_0 as numeric
register_data[,Drug_0:=factor(Drug_0)]
# reserve a random tenth of the data for prediction
set.seed(9)
learning <- sample(1:NROW(register_data),size=NROW(register_data)*9/10)
learning_register_data  <- register_data[learning]
test_register_data  <- register_data[!learning]
ps1_ranger <- ranger(Drug_0~ sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration,
                     data = learning_register_data,
                     seed=8, classification = TRUE,
                     probability=FALSE)
# propensity score
propensity1_ranger <- predictRisk(ps1_ranger,newdata=test_register_data)
mean(propensity1_ranger)

mean(propensity1_ranger)
mean(test_register_data[["Drug_0"]]==1)

ps_learn <- glm(Drug_0~ sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration,
          family = "binomial",
          data = learning_register_data)
propensity_test <- predictRisk(ps_learn,newdata=test_register_data)
mean(propensity_test)

library(ggplot2)
test_register_data[,propensity_test_logistic := predictRisk(ps_learn,newdata=test_register_data)]
test_register_data[,propensity_test_ranger := predictRisk(ps1_ranger,newdata=test_register_data)]
ggplot(test_register_data,aes(x= propensity_test_logistic, y= propensity_test_ranger))+geom_point()+xlim(c(0,1))+ylim(c(0,1))

    tar_target(ltmle_SL_ranger_death_1,
               run_Ltmle(name_outcome="Dead",
                         name_censoring = "Censored",
                         censored_label = 0,
                         time_horizon=1,
                         outcome_data=survival_outcome_data,
                         regimen_data=list(Drug = regimen_data),
                         baseline_data=baseline_covariates,
                         timevar_data=time_covariates,
                         abar = list(control = 0,treat = 1),
                         SL.library=c("SL.glm","SL.glm.interaction","SL.ranger2","SL.glmnet"),
                         SL.cvControl = list(V = 2),
                         verbose=TRUE)
               ),
    tar_target(ltmle_summary_SL_ranger_death_1,{
        summary(ltmle_SL_ranger_death_1)
    })
