tar_target(ltmle_analysis,{
  ltmle_fit <- Ltmle(data[,.(sex,age,treatment,biomarker)],
                     Anodes="treatment",
                     Lnodes=c("sex","age"),
                     Ynodes=c("biomarker"),
                     Yrange=c(0,100),
                     abar=list(0,1),
                     time_horizon = 1,
                     SL.library="glm")
})

tar_load(ltmle_analysis)

tar_target(summary_ltmle_analysis,{
summary(ltmle_analysis)
})
