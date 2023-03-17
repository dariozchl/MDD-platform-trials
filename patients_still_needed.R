#calculates the number of patients still needed to finish the arms that are currently recruiting
patients_still_needed <- function(res_list,
                                  applicable_to_PRD,
                                  population,
                                  way_of_administration, 
                                  rand_type,
                                  control_cap
                                  
){
  population <- 1 # currently only implemented for one domain and one population
  cohorts_left <- coh_left_check(res_list, applicable_to_PRD)
  
  # indices of the currently recruiting arms in this way of administration
  active_arms_in_admin_index <- grep(way_of_administration,
                                     ((names(res_list[[population]])))[cohorts_left[,population]])
  # names of the currently recruiting arms in this way of administration
  active_treatments_in_admin <- (((names(res_list[[population]])))[cohorts_left[,population]])[active_arms_in_admin_index][-c(1)]
  # number of active treatment arms (without control)
  k <- length(active_treatments_in_admin)
  
  n_still_needed <- 0
  if(k > 0){
    # adds the patients that are still needed to finish the currently running arms
    for (j in 1:k) {
      if (is.null(res_list[[population]][[active_treatments_in_admin[j]]]$data)) {
        n_still_needed <- n_still_needed + 80
      } else {
        #patients still needed for this treatment arm
        n_still_needed <- n_still_needed + 
          (80 - 
             nrow(res_list[[population]][[active_treatments_in_admin[j]]]$data))
      }
    }
    
    # estimates the controls still needed and adds them to the needed sample size
    n_needed_with_control <- 0
    if(rand_type == "block_1"){
      n_needed_with_control <- n_still_needed + n_still_needed/k
    } else if(rand_type == "block_k"){
      n_needed_with_control <- n_still_needed + n_still_needed
    } else if(rand_type == "block_sqrt"){
      n_needed_with_control <- n_still_needed + n_still_needed/k*sqrt(k)
    } else if(rand_type == "block_sqrt_cap" || rand_type == "full"){
      if((sqrt(k)/(k + sqrt(k))) > control_cap){
        n_needed_with_control <- n_still_needed + n_still_needed/k*sqrt(k)
      } else {
        n_needed_with_control <- n_still_needed + n_still_needed*control_cap/(1-control_cap)
      }  
    }
  } else {n_needed_with_control <- 0}
  
  return(n_needed_with_control)
} # end function