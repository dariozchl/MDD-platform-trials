
#' Helper Function: Create new cohort initially
#' @export
create_cohort_initial <- function(cohorts_start, n_int, n_fin, treatment_effects, ways_of_administration=c("pill", "IV", "nasal"),
                                  alloc_ratio_administration="fixed", alloc_ratio_control="fixed",
                                  alloc_ratio_administration_values=NULL, alloc_ratio_control_values=0.35,
                                  cohorts_start_applic_to_TRD, cohorts_start_applic_to_PRD, sharing_type="all") {

  alloc_ratio_control_values <<- alloc_ratio_control_values # global for further use in nested functions
  sharing_type <<- sharing_type # global for further use in nested functions

  cohorts_start_TRD <- list(); cohorts_start_PRD <- list(); cohorts_per_administration_TRD <- list(); cohorts_per_administration_PRD <- list()
  # sample whether cohorts are applicable to TRD and PRD
  for(i in 1:length(ways_of_administration)){
    cohorts_start_TRD[[i]] <- cohorts_start_applic_to_TRD[[ways_of_administration[i]]]
    cohorts_start_PRD[[i]] <- cohorts_start_applic_to_PRD[[ways_of_administration[i]]]
    names(cohorts_start_TRD)[i] <- paste0(ways_of_administration[i])
    names(cohorts_start_PRD)[i] <- paste0(ways_of_administration[i])

    cohorts_per_administration_TRD[[i]] <- rep(ways_of_administration[i], sum(cohorts_start_TRD[[i]]))
    cohorts_per_administration_PRD[[i]] <- rep(ways_of_administration[i], sum(cohorts_start_PRD[[i]]))
  }

  res_list <- list("TRD"=rep(list(c(list(decision = rep("none", 2), alloc_ratio = NULL, n_thresh = NULL, start_n = 0, response = NULL, endpoint = NULL, n = NULL))),
                             sum(unlist(cohorts_start_TRD))+length(ways_of_administration)), # +length(ways_of_administration) to add a control for each way of administration
                   "PRD"=rep(list(c(list(decision = rep("none", 2), alloc_ratio = NULL, n_thresh = NULL, start_n = 0, response = NULL, endpoint = NULL, n = NULL))),
                             sum(unlist(cohorts_start_PRD))+length(ways_of_administration)))

  # naming the cohorts appropriately for both TRD and PRD
  for (i in 1:length(res_list$TRD)) {
    if(i %in% (1:length(ways_of_administration))) { # the first entries are reserved for the controls
      name_placeholder <- paste0(ways_of_administration[i], "_Control")
      names(res_list$TRD)[i] <- name_placeholder
    } else {
      name_placeholder <- paste0(unlist(cohorts_per_administration_TRD)[i-length(ways_of_administration)], "_Treatment")
      names(res_list$TRD)[i] <- paste0(name_placeholder, length(grep(name_placeholder, names(res_list$TRD)))+1) # check how many treatments there are already for this way of administration and add 1 to set the index
    }
    res_list$TRD[[i]]$response <- treatment_effects$TRD[[paste(name_placeholder)]][[sample(x=1:(length(treatment_effects$TRD[[paste(name_placeholder)]])-1), size=1)]]
  }

  for (i in 1:length(res_list$PRD)) {
    if(i %in% (1:length(ways_of_administration))) { # the first entries are reserved for the controls
      name_placeholder <- paste0(ways_of_administration[i], "_Control")
      names(res_list$PRD)[i] <- name_placeholder
    } else {
      name_placeholder <- paste0(unlist(cohorts_per_administration_PRD)[i-length(ways_of_administration)], "_Treatment")
      names(res_list$PRD)[i] <- paste0(name_placeholder, length(grep(name_placeholder, names(res_list$PRD)))+1) # check how many treatments there are already for this way of administration and add 1 to set the index
    }
    res_list$PRD[[i]]$response <- treatment_effects$PRD[[paste(name_placeholder)]][[sample(x=1:(length(treatment_effects$PRD[[paste(name_placeholder)]])-1), size=1)]]
  }

  # Update allocation ratio
  #res_list <- update_alloc_ratio(res_list)

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



res_list <- create_cohort_initial(cohorts_start=cohorts_start, n_int=list("TRD"=50,"PRD"=50), n_fin=list("TRD"=100,"PRD"=100), 
                                  treatment_effects=treatment_effects, 
                                  cohorts_start_applic_to_TRD=cohorts_start_applic_to_TRD, cohorts_start_applic_to_PRD=cohorts_start_applic_to_PRD)
View(res_list)



