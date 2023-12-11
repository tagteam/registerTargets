tar_target(ltmle_gcomp_mace_2,
           run_Ltmle(name_outcome="mace",
                     name_competing_risk = "Dead",
                     name_censoring = "Censored",
                     censored_label = 0,
                     time_horizon=4,
                     outcome_data=mace_outcome_data,
                     regimen_data=list(Drug = regimen_data),
                     baseline_data=baseline_covariates,
                     timevar_data=time_covariates,
                     abar = list(control = rep(0,4),treat = rep(1,4)),
                     gcomp = TRUE,
                     SL.library="glm",
                     verbose=TRUE)),
tar_target(ltmle_ipw_mace_2,
           run_Ltmle(name_outcome="mace",
                     name_competing_risk = "Dead",
                     name_censoring = "Censored",
                     censored_label = 0,
                     time_horizon=4,
                     outcome_data=mace_outcome_data,
                     regimen_data=list(Drug = regimen_data),
                     baseline_data=baseline_covariates,
                     timevar_data=time_covariates,
                     abar = list(control = rep(0,4),treat = rep(1,4)),
                     iptw.only = TRUE,
                     SL.library="glm",
                     verbose=TRUE)
           )

tar_load_everything()
ltmle_summary_mace_2
ltmle_summary_ipw_mace_2
ltmle_summary_gcomp_mace_2

tar_target(ltmle_gcomp_mace_2_subset,
           run_Ltmle(name_outcome="mace",
                     name_competing_risk = "Dead",
                     name_censoring = "Censored",
                     censored_label = 0,
                     time_horizon=4,
                     sub_set = list(data = baseline_covariates[education == "High",.(pnr)]),
                     outcome_data=mace_outcome_data,
                     regimen_data=list(Drug = regimen_data),
                     baseline_data=baseline_covariates,
                     timevar_data=time_covariates,
                     abar = list(control = rep(0,4),treat = rep(1,4)),
                     gcomp = TRUE,
                     SL.library="glm",
                     verbose=TRUE)),
tar_target(ltmle_summary_gcomp_mace_2_subset,{
  summary(ltmle_gcomp_mace_2_subset)
}),
tar_target(ltmle_ipw_mace_2_subset,
           run_Ltmle(name_outcome="mace",
                     name_competing_risk = "Dead",
                     name_censoring = "Censored",
                     censored_label = 0,
                     time_horizon=4,
                     sub_set = list(data = baseline_covariates[education == "High",.(pnr)]),
                     outcome_data=mace_outcome_data,
                     regimen_data=list(Drug = regimen_data),
                     baseline_data=baseline_covariates,
                     timevar_data=time_covariates,
                     abar = list(control = rep(0,4),treat = rep(1,4)),
                     iptw.only = TRUE,
                     SL.library="glm",
                     verbose=TRUE)),
tar_target(ltmle_summary_ipw_mace_2_subset,{
  summary(ltmle_ipw_mace_2_subset)
}),
tar_target(ltmle_fit_glm_mace_2_subset,
           run_Ltmle(name_outcome="mace",
                     name_competing_risk = "Dead",
                     name_censoring = "Censored",
                     censored_label = 0,
                     time_horizon=4,
                     sub_set = list(data = baseline_covariates[education == "High",.(pnr)]),
                     outcome_data=mace_outcome_data,
                     regimen_data=list(Drug = regimen_data),
                     baseline_data=baseline_covariates,
                     timevar_data=time_covariates,
                     abar = list(control = rep(0,4),treat = rep(1,4)),
                     SL.library="glm",
                     verbose=TRUE)),
tar_target(ltmle_summary_glm_mace_2_subset,{
  summary(ltmle_fit_glm_mace_2_subset)
})

tar_load_everything()
ltmle_summary_gcomp_mace_2_subset
ltmle_summary_ipw_mace_2_subset
ltmle_summary_glm_mace_2_subset
