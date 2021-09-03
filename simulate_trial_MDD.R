# this is the main function to simulate everything

library(mvtnorm)

simulate_trial <- function(cohorts_start, n_int, n_fin,
                           treatment_effects, ways_of_administration,
                           cohorts_start_applic_to_TRD, cohorts_start_applic_to_PRD, sharing_type,
                           patients_per_timepoint, prob_new_compound, 
                           max_treatments, # should be of length length(ways_of_administration) if number_of_compounds_cap=separate, otherwise of length 1
                           trial_end, # can be either "pipeline" or "timepoint"
                           number_of_compounds_cap, # can either be "separate" or "global"
                           pipeline_size, 
                           latest_timepoint_treatment_added, # must always be specified to avoid running forever
                           p_val_interim, p_val_final, sided) {

  ##### Initialization #####
  
  ways_of_administration <<- ways_of_administration
  
  # dummy to indicate trial stop
  trial_stop <- FALSE

  # Initialize res_list
  res_list <- create_cohort_initial(cohorts_start=cohorts_start, n_int=n_int, n_fin=n_fin,
                                    treatment_effects=treatment_effects, ways_of_administration=ways_of_administration,
                                    cohorts_start_applic_to_TRD=cohorts_start_applic_to_TRD, cohorts_start_applic_to_PRD=cohorts_start_applic_to_PRD)

  Total_N_Vector <- NULL

  timestamp <- 0

  ##### Running Simulations #####
  while (!trial_stop) {
    
    # check whether new compound is available and if yes, add it to res_list
    res_list <- check_new_compound(res_list=res_list, number_of_compounds_cap=number_of_compounds_cap, max_treatments=max_treatments, prob_new_compound=prob_new_compound, 
                                   trial_end=trial_end, timestamp=timestamp, latest_timepoint_treatment_added=latest_timepoint_treatment_added, ways_of_administration=ways_of_administration)
    
    # some arms may have been added, so check again which cohorts are recruiting.
    cohorts_left <- coh_left_check(x=res_list) 
    
    # update allocation ratio
    res_list <- update_alloc_ratio(res_list, ways_of_administration=ways_of_administration)
    
    
    # sample size to be allocated to routes of administration
    # patients_per_timepoint has two entries, one for TRD and one for PRD
    n <- rpois(n=2, lambda=patients_per_timepoint)
    
    
    # Get new patients and responders for each cohort
    for(population in 1:2){
      
      # get vectors with allocation ratios within domain and allocation ratios for each domain
      all_alloc_ratios <- sapply(X=row.names(cohorts_left)[cohorts_left[,population]], FUN=function(X) return(res_list[[population]][[X]]$alloc_ratio))
      all_prob_admins <- sapply(X=row.names(cohorts_left)[cohorts_left[,population]], FUN=function(X) return(res_list[[population]][[X]]$prob_admin))
      
      # sample from multinomial distribution with size of available patients how many patients are allocated to each treatment arm
      # with probability derived from the allocation ratios within and across each way of administration
      if(sum(all_alloc_ratios * all_prob_admins) > 0){ 
        # allocate all available patients according to the probability all_alloc_ratios * all_prob_admins
        n_all_arms <- rmultinom(n=1, size=n[population], prob=all_alloc_ratios * all_prob_admins) 
      } else {
        n_all_arms <- 0
      }
      
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
    
    res_list <- make_decision_wrapper(res_list=res_list, p_val_final=p_val_final, p_val_interim=p_val_interim, sided=sided, n_fin=n_fin, n_int=n_int, timestamp=timestamp)
    
    if(all(colSums(coh_left_check(res_list)) == length(ways_of_administration)) & timestamp>=latest_timepoint_treatment_added){trial_stop=TRUE}

    print(timestamp)
  }
    
    return(res_list)

}



