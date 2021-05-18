library(mvtnorm)


#' Helper Function: Create new cohort initially
create_cohort_initial <- function(cohorts_start, n_int, n_fin, treatment_effects, ways_of_administration,
                                  alloc_ratio_administration, alloc_ratio_control,
                                  alloc_ratio_administration_values, alloc_ratio_control_values,
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


#' Helper Function: Create new cohort and update allocation ratio
create_cohort_new <- function(res_list, n_int, n_fin, sharing_type, treatment_effects, way_of_administration, applicable_to_TRD=TRUE, applicable_to_PRD=TRUE) {


  name_placeholder <- paste0(way_of_administration,"_Treatment") # required for randomly drawing response

  if(applicable_to_TRD){
    new_list <- list(list(decision = rep("none", 2), alloc_ratio = NULL, n_thresh = NULL, start_n =  total_n(res_list$TRD),
                          response = treatment_effects$TRD[[paste(name_placeholder)]][[sample(x=1:(length(treatment_effects$TRD[[paste(name_placeholder)]])-1), size=1)]],
                          endpoint = NULL,
                          n = rep(NA, length(res_list$TRD[[paste0(way_of_administration, "_Control")]]$n))))
    # assign name to the new treatment: find highest already existing treatment index among this way of administration and add +1
    names_of_treatments <- c(names(res_list$TRD), names(res_list$PRD))[grep(paste0(way_of_administration,"_Treatment"),c(names(res_list$TRD), names(res_list$PRD)))]
    number_treatments_max <- max(as.numeric(gsub(pattern=paste0(way_of_administration,"_Treatment"), replacement="", x=names_of_treatments[grep(way_of_administration, names_of_treatments)])))
    new_name <- paste0(way_of_administration,"_Treatment",number_treatments_max+1)

    res_list$TRD <- c(res_list$TRD, new_list)
    names(res_list$TRD)[length(res_list$TRD)] <- new_name
  }

  if(applicable_to_PRD){
    new_list <- list(list(decision = rep("none", 2), alloc_ratio = NULL, n_thresh = NULL, start_n =  total_n(res_list$PRD),
                          response = treatment_effects$PRD[[paste(name_placeholder)]][[sample(x=1:(length(treatment_effects$PRD[[paste(name_placeholder)]])-1), size=1)]],
                          endpoint = NULL,
                          n = rep(NA, length(res_list$PRD[[paste0(way_of_administration, "_Control")]]$n))))

    # assign name to the new treatment: find highest already existing treatment index among this way of administration and add +1
    names_of_treatments <- c(names(res_list$TRD), names(res_list$PRD))[grep(paste0(way_of_administration,"_Treatment"),c(names(res_list$TRD), names(res_list$PRD)))]
    number_treatments_max <- max(as.numeric(gsub(pattern=paste0(way_of_administration,"_Treatment"), replacement="", x=names_of_treatments[grep(way_of_administration, names_of_treatments)])))
    new_name <- paste0(way_of_administration,"_Treatment",number_treatments_max+1)

    res_list$PRD <- c(res_list$PRD, new_list)
    names(res_list$PRD)[length(res_list$PRD)] <- new_name

  }

  # Update allocation ratio
  res_list <- update_alloc_ratio(res_list, ways_of_administration=ways_of_administration)

  return(res_list)
}


#' Helper Function: Total Sample Size
total_n <- function(x) {
  list("TRD"=sum(unlist(sapply(x$TRD, function(y) nrow(y$endpoint)))),
       "PRD"=sum(unlist(sapply(x$PRD, function(y) nrow(y$endpoint)))))
}


#' Helper Function: Update Allocation Ratio
update_alloc_ratio <- function(res_list, ways_of_administration) {

  for(population in 1:2){
    cohorts_left <- coh_left_check(res_list)
    active_admin <- unique(gsub(pattern="_.*", "", rownames(cohorts_left)[cohorts_left[,population]]))
    number_of_active_admin <- length(active_admin)
    for(i in 1:length(ways_of_administration)){

      active_arms_in_admin_index <- grep(ways_of_administration[i], names(res_list[[population]])[cohorts_left[,population]])
      active_arms_in_admin <- names(res_list[[population]])[active_arms_in_admin_index]

      inactive_arms_in_admin_index <- grep(ways_of_administration[i], names(res_list[[population]])[!cohorts_left[,population]])
      inactive_arms_in_admin <- names(res_list[[population]])[inactive_arms_in_admin_index]

      k <- length(active_arms_in_admin)-1 # number of treatment arms
      alloc_ratio <- c(1/sqrt(k), rep(1/k,k)) / sum(c(1/sqrt(k), rep(1/k,k))) # standard 1:(1/sqrt(k)) allocation

      if(length(active_arms_in_admin_index) >= 1){
        for(j in active_arms_in_admin_index){
          res_list[[population]][[j]]$alloc_ratio <- alloc_ratio[which(j==active_arms_in_admin_index)]
          res_list[[population]][[j]]$prob_admin <- rep(1/number_of_active_admin, number_of_active_admin)[i]
        }
      }
      if(length(inactive_arms_in_admin_index) >= 1){
        for(j in inactive_arms_in_admin_index){
          res_list[[population]][[j]]$alloc_ratio <- NULL
          res_list[[population]][[j]]$prob_admin <- rep(1/number_of_active_admin, number_of_active_admin)[i]
        }
      }
    }
  }

  return(res_list)
}


#' Helper Function: Check which cohorts are left
coh_left_check <- function(x) {
  cohorts_left_TRD <- sapply(x$TRD, function(y) {y$decision[1] %in% c("none", "PROMISING", "CONTINUE") & y$decision[2] == "none"})
  cohorts_left_PRD <- sapply(x$PRD, function(y) {y$decision[1] %in% c("none", "PROMISING", "CONTINUE") & y$decision[2] == "none"})
  rbind(t(as.data.frame(cohorts_left_TRD)), t(as.data.frame(cohorts_left_PRD)))

  return(cbind(as.data.frame(cohorts_left_TRD), as.data.frame(cohorts_left_PRD)))
}



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


