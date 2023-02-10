check_new_compound <- function(res_list, 
                               number_of_compounds_cap, 
                               max_treatments, 
                               prob_new_compound, 
                               trial_end, 
                               timestamp, 
                               latest_timepoint_treatment_added, 
                               ways_of_administration,
                               applicable_to_PRD
                               ){
  
  cohorts_left <- coh_left_check(res_list, applicable_to_PRD)
  
  if(number_of_compounds_cap == "separate"){ # should each compound have their own cap
    
    # check whether new compound is available for each admin
    add_new_cohort <- rbinom(n=length(ways_of_administration), size=1, prob=cohorts_per_timepoint)
    
    for(i in 1:length(ways_of_administration)){
      # counts how many treatments per admin are active either in PRD or in TRD. One cohort_left is always control, so -1
      current_treatments <- sum(rowSums(cohorts_left[grep(ways_of_administration[i], 
                                                          row.names(cohorts_left)),]) >= 1) - 1 
      if(trial_end == "pipeline"){
        # counts how many treatments have already been included per admin in the platform in PRD or in TRD. One cohort_left is always control, so -1
        treatments_in_platform <- length(grep(ways_of_administration[i], 
                                              row.names(cohorts_left))) - 1 
        if((add_new_cohort[i] == 1) & 
           (current_treatments < max_treatments[i]) & 
           (treatments_in_platform<pipeline_size[i])){ 
              res_list <- create_cohort_new(res_list, 
                                            n_int=n_int,
                                            n_fin=n_fin, 
                                            sharing_type=sharing_type, 
                                            treatment_effects=treatment_effects,
                                            way_of_administration=ways_of_administration[i],  
                                            applicable_to_TRD=TRUE, 
                                            applicable_to_PRD=applicable_to_PRD, 
                                            timestamp=timestamp)
        }
      }
      if(trial_end == "timepoint"){
        if((add_new_cohort[i] == 1) & 
           (current_treatments < max_treatments[i]) & 
           (timestamp<latest_timepoint_treatment_added)){ 
              res_list <- create_cohort_new(res_list, 
                                            n_int=n_int, 
                                            n_fin=n_fin, 
                                            sharing_type=sharing_type, 
                                            treatment_effects=treatment_effects,
                                            way_of_administration=ways_of_administration[i],  
                                            applicable_to_TRD=TRUE, 
                                            applicable_to_PRD=applicable_to_PRD, 
                                            timestamp=timestamp)
        }
      }
    }
  }
          
          

  
  if(number_of_compounds_cap == "global"){ # one single cap, regardless of current number of treatment in each admin
    
    # check whether new compound is available and if yes, for which admin
    new_compound <- sample(size=1, 
                           x=c(ways_of_administration,"none"), 
                           prob = c(prob_new_compound, 
                                    1-sum(prob_new_compound)))
    
    # counts how many treatments are active in total either in PRD or in TRD. length(ways_of_administration) to get rid of controls
    current_treatments <- sum(rowSums(cohorts_left) >= 1) - length(ways_of_administration) 
    
    add_compound <<- (new_compound != "none") & 
                     (current_treatments < max_treatments) & 
                     (timestamp<latest_timepoint_treatment_added)
    
    
    if(trial_end == "pipeline"){
      return("The option number_of_compounds_cap='global' can only be combined with trial_end='timepoint'")
    }
    if(trial_end == "timepoint"){
      if(add_compound == TRUE){ 
            res_list <- create_cohort_new(res_list, 
                                          n_int=n_int, 
                                          n_fin=n_fin, 
                                          sharing_type=sharing_type, 
                                          treatment_effects=treatment_effects,
                                          way_of_administration=new_compound, 
                                          applicable_to_TRD=TRUE,
                                          applicable_to_PRD=applicable_to_PRD,
                                          timestamp=timestamp)
      }
    }
  }
  
  
  return(res_list)
}
