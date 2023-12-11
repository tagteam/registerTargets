tar_target(ltmle_fit_death_1,
           run_Ltmle(name_outcome="Dead",
                     time_horizon=1,
                     outcome_data=survival_outcome_data,
                     regimen_data=list(Drug = regimen_data),
                     baseline_data=baseline_covariates,
                     timevar_data=time_covariates,
                     abar = list(0,1),
                     SL.library="glm",
                     verbose=TRUE)),
tar_target(table_ltmle_death_1,{
  summary(ltmle_fit_death_1)
})

tar_target(ltmle_fit_death_2,{
  run_Ltmle(name_outcome="Dead",
            time_horizon=4,
            outcome_data=survival_outcome_data,
            regimen_data=list(Drug = regimen_data),
            baseline_data=baseline_covariates,
            timevar_data=time_covariates,
            abar = list(rep(1,4),rep(0,4)),
            SL.library="glm",
            verbose=TRUE)
}),
tar_target(table_ltmle_death_2,{
  summary(ltmle_fit_death_2)
})
