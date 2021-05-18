library(mvtnorm)

simulate_trial <- function(cohorts_start, n_int, n_fin,
                           treatment_effects, ways_of_administration,
                           alloc_ratio_administration="fixed", alloc_ratio_control="fixed",
                           alloc_ratio_administration_values=NULL, alloc_ratio_control_values=0.35,
                           cohorts_start_applic_to_TRD, cohorts_start_applic_to_PRD, sharing_type="all",
                           patients_per_timepoint=c(30,30)) {

  ##### Initialization #####

  # dummy to indicate trial stop
  trial_stop <- FALSE

  # Initialize res_list
  res_list <- create_cohort_initial(cohorts_start=cohorts_start, n_int=n_int, n_fin=n_fin,
                                    treatment_effects=treatment_effects, ways_of_administration=ways_of_administration,
                                    alloc_ratio_administration=alloc_ratio_administration, alloc_ratio_control=alloc_ratio_control,
                                    alloc_ratio_administration_values=alloc_ratio_administration_values, alloc_ratio_control_values=alloc_ratio_control_values,
                                    cohorts_start_applic_to_TRD=cohorts_start_applic_to_TRD, cohorts_start_applic_to_PRD=cohorts_start_applic_to_PRD)

  Total_N_Vector <- NULL

  timestamp <- 0

  ##### Running Simulations #####
  while (!trial_stop) {

    # Check which cohorts are recruiting
    cohorts_left <- coh_left_check(x=res_list)

    # update allocation ratio
    res_list <- update_alloc_ratio(res_list, ways_of_administration=ways_of_administration)


    # sample size to be allocated to routes of administration
    # patients_per_timepoint has two entries, one for TRD and one for PRD
    n <- rpois(n=2, lambda=patients_per_timepoint)


    # Get new patients and responders for each cohort
    for(population in 1:2){
      for (i in row.names(cohorts_left)[cohorts_left[,population]]) {

        n_arm <- floor(n[population] * res_list[[population]][[i]]$prob_admin * res_list[[population]][[i]]$alloc_ratio)
        response <- res_list[[population]][[i]]$response

        if(n_arm >= 1){
          draw <- rmvnorm(n=n_arm, mean=response$mean, sigma=response$sigma)
          draw <- cbind(draw, timestamp)
          res_list[[population]][[i]]$endpoint <- rbind(res_list[[population]][[i]]$endpoint, draw)
        }
      }
    }

    timestamp <- timestamp + 1

    TOTAL_N <- total_n(res_list)
    if(TOTAL_N[[1]] > 150 | TOTAL_N[[2]] > 150){trial_stop=TRUE}
  }

  return(res_list)

}







cohorts_start <- list("pill"=3, "IV"=2, "nasal"=2)

# probability of a treatment being applicable to PRD and TRD patients
cohorts_start_applic_to_TRD <- list("pill"=3, "IV"=2, "nasal"=2)
cohorts_start_applic_to_PRD <- list("pill"=3, "IV"=2, "nasal"=2)


# names must be given as paste0(way_of_administration, "_Control") or paste0(way_of_administration, "_Treatment")
treatment_effects <- list("TRD"=list("pill_Control"=list(list("mean"=c(5, 10),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=1),
                                     "pill_Treatment"=list(list("mean"=c(6,10),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(10,15),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(10,18),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=c(0.3,0.4,0.3)),
                                     "IV_Control"=list(list("mean"=c(10,15),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=1),
                                     "IV_Treatment"=list(list("mean"=c(9,10),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(10,15),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(15,18),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=c(0.3,0.3,0.4)),
                                     "nasal_Control"=list(list("mean"=c(12,15),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=1), "nasal_Treatment"=list(list("mean"=c(4,10),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(10,15),"sigma"=matrix(c(5,2,2,5), ncol=2)), list("mean"=c(13,18),"sigma"=matrix(c(5,2,2,5), ncol=2)), probs=c(0.3,0.2,0.5))),
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
                          patients_per_timepoint=c(30,30))

View(results)


