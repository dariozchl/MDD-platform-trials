# This is a function to run the simulations

library(doParallel)


source("create_cohort_initial.R")
source("coh_left_check.R")
source("total_n.R")
source("update_alloc_ratio.R")
source("create_cohort_new.R")
source("make_decision_trialNEW.R")
source("make_decision_wrapper.R")
source("simulate_trial_MDD.R")
source("operating_characteristics.R")


cohorts_start <- list("pill"=3, "IV"=2, "nasal"=2)

# probability of a treatment being applicable to PRD and TRD patients
cohorts_start_applic_to_TRD <- list("pill"=3, "IV"=2, "nasal"=2)
cohorts_start_applic_to_PRD <- list("pill"=3, "IV"=2, "nasal"=2)

# names must be given as paste0(way_of_administration, "_Control") or paste0(way_of_administration, "_Treatment")
treatment_effects <- list(
  "TRD"=list("pill_Control"=list(list("mean"=c(32, 20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
             "pill_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   probs=c(0.25,0.25,0.25,0.25)),
             "IV_Control"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
             "IV_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 probs=c(0.25,0.25,0.25,0.25)),
             "nasal_Control"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1), 
             "nasal_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    probs=c(0.25,0.25,0.25,0.25))),
  
  "PRD"=list("pill_Control"=list(list("mean"=c(32, 20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
             "pill_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   probs=c(0.25,0.25,0.25,0.25)),
             "IV_Control"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
             "IV_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 probs=c(0.25,0.25,0.25,0.25)),
             "nasal_Control"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1), 
             "nasal_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    probs=c(0.25,0.25,0.25,0.25)))
  )


start.time <- Sys.time()
cores <- detectCores() * 3/4
cl <- makeCluster(cores)
registerDoParallel(cl)

# setting seed ?

nsim <- 10000
sim_results <- NULL

patients_per_timepoint = list(c(30,30), c(20,20))
n_fin <- list(list("TRD"=100,"PRD"=100), list("TRD"=80,"PRD"=80))
scenarios <- expand.grid("patients_per_timepoint"=patients_per_timepoint, "n_fin"=n_fin, "pvals"=list(c(0.4,0.05), c(0.5,0.1)))


for(i in 1:nrow(scenarios)){
  sim_results_tmp <- foreach(nsim=1:nsim, .combine=rbind, .packages=c("tidyverse", "mvtnorm")) %dopar% {
    single_sim_results <- simulate_trial(cohorts_start=cohorts_start, 
                                         n_int=lapply(scenarios$n_fin[[i]], function(x) x/2), 
                                         n_fin=scenarios$n_fin[[i]],
                                         treatment_effects=treatment_effects, 
                                         ways_of_administration=c("pill", "IV", "nasal"),
                                         alloc_ratio_administration="fixed", 
                                         alloc_ratio_control="fixed",
                                         alloc_ratio_administration_values=NULL, 
                                         alloc_ratio_control_values=0.35,
                                         cohorts_start_applic_to_TRD=cohorts_start_applic_to_TRD, 
                                         cohorts_start_applic_to_PRD=cohorts_start_applic_to_PRD,
                                         sharing_type="concurrent",
                                         patients_per_timepoint=scenarios$patients_per_timepoint[[i]], 
                                         cohorts_per_timepoint=c(0.1,0.05,0.05), 
                                         max_treatments=c(4,3,3), 
                                         trial_end="timepoint", latest_timepoint_treatment_added=60,
                                         #trial_end="pipeline", pipeline_size=c(10,4,4),
                                         p_val_interim=scenarios$pvals[[i]][1], p_val_final=scenarios$pvals[[i]][2])
    
    
    ocs <- data.frame(operating_characteristics(single_sim_results) %>% 
                        rownames_to_column("armID") %>% 
                        mutate(admin = gsub("_.*", "", armID), 
                               treatment_ID = case_when(grepl("Control", armID) ~ "Control", TRUE ~ gsub(".*_Treatment", "", armID))) %>% 
                        relocate(admin, treatment_ID, .before=armID) %>% select(-armID),
                      "nsim"=nsim)
    
    return(ocs)
  }
  sim_results <- rbind(sim_results, sim_results_tmp %>% add_column("scenarioID"=i))
  print(paste0(nsim*i, " Simulations took ", round(difftime(Sys.time(), start.time, units="min"), 2), " minutes"))
}
stopCluster(cl)
sim_results <- as_tibble(sim_results)

# add specific information about the scenarios
# CAUTION: this needs to be adjusted when simulation scenarios are altered
sim_results <- sim_results %>% mutate(N = case_when(scenarioID %in% c(1,2,5,6) ~ 100, TRUE ~ 80),
                                      patients_per_timepoint = case_when(scenarioID %in% c(1,3,5,7) ~ 30, TRUE ~ 20),
                                      pvalues = case_when(scenarioID %in% 1:4 ~ "0.4,0.05", TRUE ~ "0.5,0.1"))

saveRDS(sim_results, "sim_results.rds")




