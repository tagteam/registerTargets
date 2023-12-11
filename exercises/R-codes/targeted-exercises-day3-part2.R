library(SuperLearner)
X=register_data[,c("sex","education","agegroups","tertile_income","index_heart_failure","diabetes_duration"),with=FALSE]
Y=register_data[["Drug_0"]]
set.seed(8)
# default value for cross-validation is 10-fold
sl_ps_super1 <- SuperLearner(Y=Y,
                             X=X,
                             SL.library=c("SL.glm","SL.glm.interaction"),
                             family="binomial")
# 2-fold is much faster
set.seed(8)
sl_ps_super_V2 <- SuperLearner(Y=Y, X=X,
                               SL.library=c("SL.glm","SL.glm.interaction"),
                               cvControl=list(V=2),
                               family="binomial")

library(SuperLearner)
X=register_data[,c("sex","education","agegroups","tertile_income","index_heart_failure","diabetes_duration"),with=FALSE]
Y=register_data[["Drug_0"]]
set.seed(8)
sl_ps_super2 <- SuperLearner(Y=Y, X=X,
                             # speed up by using 2-fold
                             # cvControl=list(V=2)
                             SL.library=c("SL.mean","SL.glm","SL.glm.interaction","SL.step.interaction"),
                             family="binomial")

print(sl_ps_super1)
print(sl_ps_super2)

tar_target(ltmle_sl_death_1,
           run_Ltmle(name_outcome="Dead",
                     time_horizon=1,
                     outcome_data=survival_outcome_data,
                     regimen_data=list(Drug = regimen_data),
                     baseline_data=baseline_covariates,
                     timevar_data=time_covariates,
                     abar = list(0,1),
                     SL.library=c("glm","glm.interaction"),
                     # speed up you may use 2-fold
                     # SL.cvControl(V=2),
                     verbose=TRUE)),
tar_target(ltmle_sl_summary_death_1,{
  summary(ltmle_sl_death_1)
})
