library(targets)
library(data.table)
tar_source("functions")
tar_source("../Ltmle/")
tar_load_everything()

pl <- prepare_Ltmle(regimen_data=regimen_data,
                    outcome_data=mace_outcome_data,
                    name_outcome="mace",
                    name_regimen="Drug",
                    name_censoring = "Censored",
                    name_competing_risk = "Dead",
                    survivalOutcome = TRUE,
                    censored_label = 0,
                    baseline_data=baseline_covariates,
                    timevar_data=time_covariates,
                    time_horizon=10,
                    subset_id=NULL,
                    SL.library="glm",
                    abar=list(drug=rep(1L,10),control=rep(0L,10)))

names(pl$data)

tar_target(ltmle_fit_glm_mace_2,
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
                     SL.library="glm",
                     verbose=TRUE)),
tar_target(ltmle_summary_mace_2,{
  summary(ltmle_fit_glm_mace_2)
})

tar_make()
tar_load(ltmle_fit_glm_mace_2)
tar_load(ltmle_summary_mace_2)

tar_load(ltmle_summary_mace_2)
tar_load(ltmle_summary_death_2)
