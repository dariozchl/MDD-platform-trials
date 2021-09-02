
#' Helper Function: Create new cohort initially
#' @export
create_cohort_initial <- function(cohorts_start, n_int, n_fin, treatment_effects, ways_of_administration,
                                  cohorts_start_applic_to_TRD, cohorts_start_applic_to_PRD) {

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

  res_list <- list("TRD"=rep(list(c(list(decision = rep("none", 2), alloc_ratio = NULL, n_thresh = NULL, start_timestamp = 0, end_timestamp = NULL, response = NULL, data = NULL, n = NULL))),
                             sum(unlist(cohorts_start_TRD))+length(ways_of_administration)), # +length(ways_of_administration) to add a control for each way of administration
                   "PRD"=rep(list(c(list(decision = rep("none", 2), alloc_ratio = NULL, n_thresh = NULL, start_timestamp = 0, end_timestamp = NULL, response = NULL, data = NULL, n = NULL))),
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

  return(res_list)

}


