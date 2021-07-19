
# This is a function to run the simulations


source("create_cohort_initial.R")
source("coh_left_check.R")
source("total_n.R")
source("update_alloc_ratio.R")
source("create_cohort_new.R")
source("make_decision_trialNEW.R")
source("make_decision_wrapper.R")
source("simulate_trial_MDD.R")


cohorts_start <- list("pill"=3, "IV"=2, "nasal"=2)

# probability of a treatment being applicable to PRD and TRD patients
cohorts_start_applic_to_TRD <- list("pill"=3, "IV"=2, "nasal"=2)
cohorts_start_applic_to_PRD <- list("pill"=3, "IV"=2, "nasal"=2)


# names must be given as paste0(way_of_administration, "_Control") or paste0(way_of_administration, "_Treatment")
treatment_effects <- list("TRD"=list("pill_Control"=list(list("mean"=c(5, 10),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=1),
                                     "pill_Treatment"=list(list("mean"=c(6,10),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(10,15),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(10,18),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=c(0.3,0.4,0.3)),
                                     "IV_Control"=list(list("mean"=c(15,10),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=1),
                                     "IV_Treatment"=list(list("mean"=c(15,8),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(15,10),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(15,5),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=c(0.3,0.3,0.4)),
                                     "nasal_Control"=list(list("mean"=c(12,10),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=1), 
                                     "nasal_Treatment"=list(list("mean"=c(10,4),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(15,10),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(15, 5),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=c(0.3,0.2,0.5))),
                          "PRD"=list("pill_Control"=list(list("mean"=c(5, 10),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=1),
                                     "pill_Treatment"=list(list("mean"=c(6,10),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(10,15),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(10,18),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=c(0.3,0.4,0.3)),
                                     "IV_Control"=list(list("mean"=c(10,15),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=1), "IV_Treatment"=list(list("mean"=c(9,10),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(10,15),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(15,18),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=c(0.3,0.3,0.4)),
                                     "nasal_Control"=list(list("mean"=c(12,15),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=1), "nasal_Treatment"=list(list("mean"=c(4,10),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(10,15),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(13,18),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=c(0.3,0.2,0.5))))




results <- simulate_trial(cohorts_start=cohorts_start, n_int=list("TRD"=50,"PRD"=50), n_fin=list("TRD"=100,"PRD"=100),
                          treatment_effects=treatment_effects, ways_of_administration=c("pill", "IV", "nasal"),
                          alloc_ratio_administration="fixed", alloc_ratio_control="fixed",
                          alloc_ratio_administration_values=NULL, alloc_ratio_control_values=0.35,
                          cohorts_start_applic_to_TRD=cohorts_start_applic_to_TRD, cohorts_start_applic_to_PRD=cohorts_start_applic_to_PRD,
                          sharing_type="all",
                          patients_per_timepoint=c(30,30), cohorts_per_timepoint=c(0.1,0.05,0.05), max_cohorts=30)

# View(results)






