# this is the main function to simulate everything

library(mvtnorm)

simulate_trial <- function(cohorts_start, n_int, n_fin,
                           treatment_effects, ways_of_administration,
                           alloc_ratio_administration="fixed", alloc_ratio_control="fixed",
                           alloc_ratio_administration_values=NULL, alloc_ratio_control_values=0.35,
                           cohorts_start_applic_to_TRD, cohorts_start_applic_to_PRD, sharing_type="all",
                           patients_per_timepoint=c(30,30), cohorts_per_timepoint, max_treatments, latest_timepoint_treatment_added,
                           p_val_interim, p_val_final) {

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
    
    
    # check whether new cohort should be added
    add_new_cohort <- rbinom(n=3, size=1, prob=cohorts_per_timepoint)
    
    for(i in 1:length(ways_of_administration)){
      current_treatments <- sum(rowSums(cohorts_left[grep(ways_of_administration[i], row.names(cohorts_left)),]) >= 1) - 1 # counts how many treatments per admin are active either in PRD or in TRD. One cohort_left is always control, so -1
      if(add_new_cohort[i] == 1 & (current_treatments < max_treatments[i]) & timestamp<latest_timepoint_treatment_added){ 
        res_list <- create_cohort_new(res_list, n_int=n_int, n_fin=n_fin, sharing_type=sharing_type, 
                                      treatment_effects=treatment_effects,way_of_administration=ways_of_administration[i], 
                                      applicable_to_TRD=TRUE, applicable_to_PRD=TRUE)
      }
    }
    
    
    
    # sample size to be allocated to routes of administration
    # patients_per_timepoint has two entries, one for TRD and one for PRD
    n <- rpois(n=2, lambda=patients_per_timepoint)
    
    
    # Get new patients and responders for each cohort
    for(population in 1:2){
      
      
      all_alloc_ratios <- sapply(X=row.names(cohorts_left)[cohorts_left[,population]], FUN=function(X) return(res_list[[population]][[X]]$alloc_ratio))
      all_prob_admins <- sapply(X=row.names(cohorts_left)[cohorts_left[,population]], FUN=function(X) return(res_list[[population]][[X]]$prob_admin))
      
      if(sum(all_alloc_ratios * all_prob_admins) > 0){ n_all_arms <- rmultinom(n=1, size=n[population], prob=all_alloc_ratios * all_prob_admins) } else {n_all_arms <- 0}
      
      for (i in row.names(n_all_arms)) {
        
        n_arm <- n_all_arms[grep(paste0("^", i, "$"), rownames(n_all_arms))] # ^ asserts that we are at the start. $ asserts that we are at the end. If there were treatment1 and treatment10, then "treatment1" would be found twice.
        response <- res_list[[population]][[i]]$response
        
        if(n_arm >= 1){
          draw <- rmvnorm(n=n_arm, mean=response$mean, sigma=response$sigma)
          draw <- cbind(draw, timestamp)
          res_list[[population]][[i]]$data <- rbind(res_list[[population]][[i]]$data, draw)
        }
      }
    }
    
    timestamp <- timestamp + 1
    
    TOTAL_N <- total_n(res_list)
    
    res_list <- make_decision_wrapper(res_list=res_list, p_val_final=p_val_final, p_val_interim=p_val_interim, n_fin=n_fin, n_int=n_int)
    
    if(all(colSums(coh_left_check(res_list)) == 3)){trial_stop=TRUE}
    if(TOTAL_N[[1]] > 1e4 | TOTAL_N[[2]] > 1e4){trial_stop=TRUE}
    
    print(timestamp)
  }
    
    return(res_list)

}



