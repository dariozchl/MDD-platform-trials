# this is the main function

library(mvtnorm)

simulate_trial <- function(cohorts_start, n_int, n_fin,
                           treatment_effects, ways_of_administration,
                           alloc_ratio_administration="fixed", alloc_ratio_control="fixed",
                           alloc_ratio_administration_values=NULL, alloc_ratio_control_values=0.35,
                           cohorts_start_applic_to_TRD, cohorts_start_applic_to_PRD, sharing_type="all",
                           patients_per_timepoint=c(30,30), cohorts_per_timepoint, max_cohorts) {

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
    add_new_cohort <- rbinom(n=3, size=1, prob=cohorts_per_timepoint)
    for(i in 1:length(ways_of_administration)){
      if(add_new_cohort[i] == 1 & nrow(cohorts_left) <= max_cohorts){
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
      
      for (i in row.names(cohorts_left)[cohorts_left[,population]]) {

          n_arm <- n_all_arms[grep(i, rownames(n_all_arms))]
          response <- res_list[[population]][[i]]$response
          
          if(n_arm >= 1){
            draw <- rmvnorm(n=n_arm, mean=response$mean, sigma=response$sigma)
            draw <- cbind(draw, timestamp)
            res_list[[population]][[i]]$endpoint <- rbind(res_list[[population]][[i]]$endpoint, draw)
          }
        }
      }
    }

    timestamp <- timestamp + 1

    TOTAL_N <- total_n(res_list)
    
    # a dummy make_decision function that randomly makes decisions
    make_decision <- function(res_list) { 
      for(population in 1:2){
        for(i in 1:length(ways_of_administration)){
          arms_within_administration <- grep(ways_of_administration[i], names(res_list[[population]]), value=TRUE)[-1] # first one is always control, for which no decision should be made
          for(j in arms_within_administration){
            if(!is.null(res_list[[population]][[j]]$endpoint)){
              if(nrow(res_list[[population]][[j]]$endpoint) > 100 & res_list[[population]][[j]]$decision[2] == "none"){
                res_list[[population]][[j]]$decision[2] <- sample(c("SUCCESS", "FAILURE"), size=1)
              } else if(nrow(res_list[[population]][[j]]$endpoint) > 50 & res_list[[population]][[j]]$decision[1] == "none"){
                res_list[[population]][[j]]$decision[1] <- sample(c("CONTINUE", "STOP"), size=1)
              }
            }
          }
        }
      }
      return(res_list)
    }
    
    res_list <- make_decision(res_list)
    
    if(all(colSums(coh_left_check(res_list)) == 3)){trial_stop=TRUE}
    
    
    if(TOTAL_N[[1]] > 1e4 | TOTAL_N[[2]] > 1e4){trial_stop=TRUE}
    
    print(timestamp)
    
  }

  return(res_list)

}



