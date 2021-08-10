
#' Helper Function: Create new cohort and update allocation ratio
#' @export
create_cohort_new <- function(res_list, n_int, n_fin, sharing_type, treatment_effects, way_of_administration, applicable_to_TRD=TRUE, applicable_to_PRD=TRUE, timestamp=timestamp) {


  name_placeholder <- paste0(way_of_administration,"_Treatment") # required for randomly drawing response

  if(applicable_to_TRD & applicable_to_PRD){
    new_list <- list(list(decision = rep("none", 2), alloc_ratio = NULL, n_thresh = NULL, start_timestamp = timestamp,
                          response = treatment_effects$TRD[[paste(name_placeholder)]][[sample(x=1:(length(treatment_effects$TRD[[paste(name_placeholder)]])-1), size=1)]],
                          data = NULL,
                          n = rep(NA, length(res_list$TRD[[paste0(way_of_administration, "_Control")]]$n))))
    # assign name to the new treatment: find highest already existing treatment index among this way of administration and add +1
    names_of_treatments <- c(names(res_list$TRD), names(res_list$PRD))[grep(paste0(way_of_administration,"_Treatment"),c(names(res_list$TRD), names(res_list$PRD)))]
    number_treatments_max <- max(as.numeric(gsub(pattern=paste0(way_of_administration,"_Treatment"), replacement="", x=names_of_treatments[grep(way_of_administration, names_of_treatments)])))
    new_name <- paste0(way_of_administration,"_Treatment",number_treatments_max+1)

    res_list$TRD <- c(res_list$TRD, new_list)
    names(res_list$TRD)[length(res_list$TRD)] <- new_name
    
    ### repeat the same for PRD
    new_list <- list(list(decision = rep("none", 2), alloc_ratio = NULL, n_thresh = NULL, start_timestamp = timestamp,
                          response = treatment_effects$PRD[[paste(name_placeholder)]][[sample(x=1:(length(treatment_effects$PRD[[paste(name_placeholder)]])-1), size=1)]],
                          data = NULL,
                          n = rep(NA, length(res_list$PRD[[paste0(way_of_administration, "_Control")]]$n))))
    
    res_list$PRD <- c(res_list$PRD, new_list)
    names(res_list$PRD)[length(res_list$PRD)] <- new_name
    
  }

    # Update allocation ratio
  res_list <- update_alloc_ratio(res_list)

  return(res_list)
}

