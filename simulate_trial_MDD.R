# this is the main function

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
    res_list <- update_alloc_ratio(res_list)

    
    # check whether new cohort is added
    # if yes, add new cohort

    
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
    
    make_decision <- function(res_list) { }
    
    if(TOTAL_N[[1]] > 150 | TOTAL_N[[2]] > 150){trial_stop=TRUE}
  }

  return(res_list)

}



