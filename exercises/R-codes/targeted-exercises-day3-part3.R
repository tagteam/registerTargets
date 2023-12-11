library(glmnet)
tar_load_everything()
X=model.matrix(~ -1 + sex + education + agegroups + tertile_income + index_heart_failure + diabetes_duration,
               data = register_data)
Y=register_data[["Drug_0"]]
set.seed(3)
lasso <- cv.glmnet(x = X,y = Y,alpha = 1)
coef(lasso)
ridge <- cv.glmnet(x = X,y = Y,alpha = 0)
coef(ridge)
elnet <- cv.glmnet(x = X,y = Y,alpha = 0.5)
coef(elnet)

# with some interactions
XX=model.matrix(~ -1 + sex * agegroups + education * tertile_income + index_heart_failure * diabetes_duration,
                data = register_data)
elnet_interaction <- cv.glmnet(x = XX,y = Y,alpha = 0.5)
coef(elnet_interaction)

tar_target(ltmle_glmnet_solo_death_1,
           run_Ltmle(name_outcome="Dead",
                     name_censoring = "Censored",
                     censored_label = 0,
                     time_horizon=1,
                     outcome_data=survival_outcome_data,
                     regimen_data=list(Drug = regimen_data),
                     baseline_data=baseline_covariates,
                     timevar_data=time_covariates,
                     abar = list(control = 0,treat = 1),
                     SL.library="glmnet",
                     SL.cvControl = list(selector="optimize",alpha=0.5),
                     verbose=TRUE)),
tar_target(table_glmnet_solo_death_1,{
  summary(ltmle_glmnet_solo_death_1)
})

tar_load(ltmle_glmnet_solo_death_1)
ltmle_glmnet_solo_death_1[[1]][[1]]$Ltmle_fit$fit$g

tar_target(ltmle_glmnet_death_1,
           run_Ltmle(name_outcome="Dead",
                     name_censoring = "Censored",
                     censored_label = 0,
                     time_horizon=1,
                     outcome_data=survival_outcome_data,
                     regimen_data=list(Drug = regimen_data),
                     baseline_data=baseline_covariates,
                     timevar_data=time_covariates,
                     abar = list(control = 0,treat = 1),
                     SL.library=c("SL.glm","SL.glm.interaction","SL.glmnet"),
                     SL.cvControl = list(V=2),
                     verbose=TRUE))
tar_target(table_glmnet_death_1,{
  summary(ltmle_glmnet_death_1)
})
