library(mvtnorm)
library(tibble)


N_perGroup <- 90  # patients per arm
N_interim_perGroup <- ceiling((N_perGroup*0.5))  # number of patients per arm in the interim analysis

interim_futility_threshold <- 0.5 # threshold for the p-value at interim analysis
# type 1 error is strictly conservative when only stopping for futility at interim is considered
final_efficacy_threshold <- 0.1  # chose unadjusted alpha for a non-binding futility stopping rule
efficacy_threshold <- 0.1  # alpha for the final analysis of the group sequential design

# parameters for the simulation of the patient response
# comparison to baseline measure (multivariate normal distribution)
ctrl_effect_mean <- c(32,20)
ctrl_effect_sigma <- matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)
treat_effect_mean <- c(32,16)
# specify means for different cohen's ds in the simulation
treat_effect_mean_d022 <- c(32,17.5)
treat_effect_mean_d035 <- c(32,16)
treat_effect_mean_d05 <- c(32,14.3)
treat_effect_sigma <- matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)
#"pill_Control"=list(list("mean"=c(32, 20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
#"pill_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
#                      list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
#                      list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
#                      list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
#                      probs=c(0.25,0.25,0.25,0.25))
# note: if you want to estimate the type 1 error for different thresholds set control and treatment effect to the same value

n_sim <- 10000


################################ FUNCTION SIMULATE SEQUENTIAL #########################################

# simulates a group sequential design with 1:1 randomization to control group and treatment group
#   interim stopping for futility (no stopping for efficacy)
#   uses comparison between baseline measure and follow-up measure (assumed to be continuous endpoints) and calculates an ANCOVA
simulate_group_sequential_design <- function(n_sim, N_perGroup, N_interim_perGroup, interim_futility_threshold, ctrl_effect_mean, ctrl_effect_sigma, treat_effect_mean, treat_effect_sigma){
  trial <- numeric(n_sim)
  p_interim <- numeric(n_sim)
  p_final <- numeric(n_sim)
  success_interim <- numeric(n_sim)
  overall_success <- numeric(n_sim)
  alloc_estimate_interim <- numeric(n_sim)
  alloc_estimate_final <- numeric(n_sim)
  
  for (i in 1:n_sim) {
    trial[i] <- i
    
    patient_id_vec <- seq(1, by = 1, len = 2*N_perGroup) # create a list of patient IDs
    
    # this allocation sequence should not be used in a real RCT, but for the purpose of these simulations it will work fine.  
    # There are no real patients or clinicians created in these simulations, and therefore no worry about someone guessing the next treatment assignment.
    # treatment=1+rbinom(nPatients, 1, 0.5) # this randomly assigns each new patient to receive treatment 1 or 2 with 50% probability each time
    # Using "exactly-equal-allocation" means we will be *slightly* over-estimating the trial power by assuming exactly equal allocation
    # Also worth noting, most people doing conventional power calculations (without simulation) assume exactly equal allocation
    treat_alloc_vec = rep(1:2, N_perGroup) # this creates a vector of "treatment allocations" which is actually just a sequence alternating between 1 and 2
    
    results_vec <- matrix(rep(0, 4*N_perGroup), ncol = 2)  # initializes results matrix. first column is for the baseline measure, second column for the treatment measure
    trialdata <- data.frame(cbind(patient_id_vec, treat_alloc_vec, results_vec))
    for (j in 1:(2*N_interim_perGroup)) {  # simulates the responses of the patients up to the interim analysis
      if(trialdata[j,]$treat_alloc_vec == 1){  # simulates values for the control group
        # multivariate normal distribution -> draw baseline and treatment value with specified means and covariance matrix
        results_vec[j,] <- rmvnorm(1, ctrl_effect_mean, ctrl_effect_sigma)
      } else if(trialdata[j,]$treat_alloc_vec == 2){  # simulates values for the treatment group
        results_vec[j,] <- rmvnorm(1, treat_effect_mean, treat_effect_sigma)
      }
    }
    
    # creates a data frame with patient ID, treatment allocation, and outcome
    trialdata <- data.frame(cbind(patient_id_vec, treat_alloc_vec, results_vec)) 
    interim_data <- subset(trialdata, patient_id_vec <= (2*N_interim_perGroup))
    
    # ANCOVA with treatment measure as dependent variable and baseline measure and treatment allocation as independent variables  
    ancova_interim <- summary(lm(formula = V4 ~ V3 + treat_alloc_vec, data = interim_data))
    # grabs interim p-value for the treatment allocation
    p_interim[i] <- round(ancova_interim$coefficients["treat_alloc_vec",4],4)
    # treatment allocation effect estimate at interim
    alloc_estimate_interim[i] <- ancova_interim$coefficients["treat_alloc_vec",1]
    # continue at interim only if the p-value is smaller then the threshold (e.g. 0.4) and if the treatment has a negative effect on the score
    success_interim[i] <- ifelse(p_interim[i] < interim_futility_threshold & ancova_interim$coefficients["treat_alloc_vec",1] < 0, 1, 0) 
    
    if(success_interim[i] == 0) {
      overall_success[i] <- 0
    } else if (success_interim[i] == 1){
      for (k in (2*N_interim_perGroup+1):(2*N_perGroup)) {  # simulates the responses of the stage 2 participants
        if(trialdata[k,]$treat_alloc_vec == 1){  # simulates values for the control group
          # multivariate normal distribution -> draw baseline and treatment value with specified means and covariance matrix
          results_vec[k,] <- rmvnorm(1, ctrl_effect_mean, ctrl_effect_sigma)
        } else if(trialdata[k,]$treat_alloc_vec == 2){  # simulates values for the treatment group
          results_vec[k,] <- rmvnorm(1, treat_effect_mean, treat_effect_sigma)
        }
      }
      
      # update results in the data frame
      trialdata <- data.frame(cbind(patient_id_vec, treat_alloc_vec, results_vec)) 
      
      # ANCOVA with follow-up measure as dependent variable and baseline measure and treatment allocation as independent variables  
      ancova_final <- summary(lm(formula = V4 ~ V3 + treat_alloc_vec, data = trialdata))
      # grabs p-value for the treatment allocation
      p_final[i] <- round(ancova_final$coefficients["treat_alloc_vec",4],4)
      # treatment allocation effect estimate
      alloc_estimate_final[i] <- ancova_final$coefficients["treat_alloc_vec",1]
      # reject null hypothesis if p value is smaller then the threshold (e.g. 0.05) and the treatment has a negative effect on the score
      overall_success[i] <- ifelse(p_final[i] < final_efficacy_threshold & ancova_final$coefficients["treat_alloc_vec",1] < 0, 1, 0) 
      
    }
  }
  
  trialresults <- as.data.frame(cbind(trial, p_interim,alloc_estimate_interim, success_interim, p_final,alloc_estimate_final, overall_success))
  return(trialresults)
}
  
################################ FUNCTION SIMULATE PARALLEL ###########################################

# simulates a parallel group design with 1:1 randomization to control group and treatment group
#   no interim analysis
#   uses comparison between baseline measure and follow-up measure (assumed to be continuous endpoints) and calculates an ANCOVA
simulate_parallel_group_design <- function(n_sim, N_perGroup, efficacy_threshold, ctrl_effect_mean, ctrl_effect_sigma, treat_effect_mean, treat_effect_sigma){
  trial <- numeric(n_sim)
  p_value <- numeric(n_sim)
  alloc_estimate <- numeric(n_sim)
  overall_success <- numeric(n_sim)
  
  for (i in 1:n_sim) {
    trial[i] <- i
    
    patient_id_vec <- seq(1, by = 1, len = 2*N_perGroup) # create a list of patient IDs

# worth noting: this allocation sequence should not be used in a real RCT, but for the purpose of these simulations it will work fine.  
# There are no real patients or clinicians created in these simulations, and therefore no worry about someone guessing the next treatment assignment.
# treatment=1+rbinom(nPatients, 1, 0.5) # this randomly assigns each new patient to receive treatment 1 or 2 with 50% probability each time
# Using "exactly-equal-allocation" means we will be *slightly* over-estimating the trial power by assuming exactly equal allocation
# Also worth noting, most people doing conventional power calculations (without simulation) assume exactly equal allocation
    treat_alloc_vec = rep(1:2, N_perGroup) # this creates a vector of "treatment allocations" which is actually just a sequence alternating between 1 and 2

    results_vec <- matrix(rep(0, 4*N_perGroup), ncol = 2)  # initializes results matrix. first column is for the baseline measure, second column for the follow-up measure
    trialdata <- data.frame(cbind(patient_id_vec, treat_alloc_vec, results_vec))
    for (j in 1:(2*N_perGroup)) {  # simulates the responses of the patients
      if(trialdata[j,]$treat_alloc_vec == 1){  # simulates values for the control group
        # multivariate normal distribution -> draw baseline and follow-up value with specified means and covariance matrix
        results_vec[j,] <- rmvnorm(1, ctrl_effect_mean, ctrl_effect_sigma)
      } else if(trialdata[j,]$treat_alloc_vec == 2){  # simulates values for the treatment group
        results_vec[j,] <- rmvnorm(1, treat_effect_mean, treat_effect_sigma)
      }
    }
    
    # creates a data frame with patient ID, treatment allocation, and outcome
    trialdata <- data.frame(cbind(patient_id_vec, treat_alloc_vec, results_vec)) 
    
    # ANCOVA with follow-up measure as dependent variable and baseline measure and treatment allocation as independent variables  
    ancova_final <- summary(lm(formula = V4 ~ V3 + treat_alloc_vec, data = trialdata))
    # grabs p-value for the treatment allocation
    p_value[i] <- round(ancova_final$coefficients["treat_alloc_vec",4],4)
    # treatment allocation effect estimate
    alloc_estimate[i] <- ancova_final$coefficients["treat_alloc_vec",1]
    # reject null hypothesis if p value is smaller then the threshold (e.g. 0.05) and the treatment has a negative effect on the score
    overall_success[i] <- ifelse(p_value[i] < efficacy_threshold & ancova_final$coefficients["treat_alloc_vec",1] < 0, 1, 0) 
    
  }
  
  trialresults <- as.data.frame(cbind(trial, p_value, alloc_estimate, overall_success))
  return(trialresults)
}

################################ ANALYZE FUNCTIONS ####################################################

analyze_group_sequential_design <- function(trials_withFutCheck, N_interim_perGroup, N_perGroup){
  n_stopped <- sum(trials_withFutCheck["success_interim"] == 0)
  p_stopped <- n_stopped / nrow(trials_withFutCheck)
  n_success <- sum(trials_withFutCheck["overall_success"] == 1)
  p_success <- n_success / nrow(trials_withFutCheck)

  patients_total <- (n_stopped * N_interim_perGroup + (nrow(trials_withFutCheck) - n_stopped) * N_perGroup)*2
  patients_mean <- patients_total / nrow(trials_withFutCheck)

  ocs <- data.frame(cbind(p_success, p_stopped, patients_mean))
  return(ocs)
}

analyze_parallel_group_design <- function(trials_parallel, N_perGroup){
  n_success <- sum(trials_parallel["overall_success"] == 1)
  p_success <- n_success / nrow(trials_parallel)
  
  patients_total <- nrow(trials_parallel) * N_perGroup*2
  patients_mean <- N_perGroup*2
  
  ocs <- data.frame(cbind(p_success, patients_mean))
  return(ocs)
}

######################################### SEQUENTIAL ##################################################

trials_withFutCheck_d0 <- simulate_group_sequential_design(n_sim, N_perGroup, N_interim_perGroup, interim_futility_threshold, ctrl_effect_mean, ctrl_effect_sigma, ctrl_effect_mean, ctrl_effect_sigma)
trials_withFutCheck_d022 <- trials_withFutCheck <- simulate_group_sequential_design(n_sim, N_perGroup, N_interim_perGroup, interim_futility_threshold, ctrl_effect_mean, ctrl_effect_sigma, treat_effect_mean_d022, treat_effect_sigma)
trials_withFutCheck_d035 <- trials_withFutCheck <- simulate_group_sequential_design(n_sim, N_perGroup, N_interim_perGroup, interim_futility_threshold, ctrl_effect_mean, ctrl_effect_sigma, treat_effect_mean_d035, treat_effect_sigma)
trials_withFutCheck_d05 <- trials_withFutCheck <- simulate_group_sequential_design(n_sim, N_perGroup, N_interim_perGroup, interim_futility_threshold, ctrl_effect_mean, ctrl_effect_sigma, treat_effect_mean_d05, treat_effect_sigma)

alpha_group_sequential <- analyze_group_sequential_design(trials_withFutCheck_d0, N_interim_perGroup, N_perGroup)
power_group_sequential_d022 <- analyze_group_sequential_design(trials_withFutCheck_d022, N_interim_perGroup, N_perGroup)
power_group_sequential_d035 <- analyze_group_sequential_design(trials_withFutCheck_d035, N_interim_perGroup, N_perGroup)
power_group_sequential_d05 <- analyze_group_sequential_design(trials_withFutCheck_d05, N_interim_perGroup, N_perGroup)

#trials_withFutCheck_d0
#trials_withFutCheck_d022 
#trials_withFutCheck_d035 
#trials_withFutCheck_d05

alpha_group_sequential 
power_group_sequential_d022
power_group_sequential_d035
power_group_sequential_d05

####################################### PARALLEL ######################################################

trials_parallel_d0 <- simulate_parallel_group_design(n_sim, N_perGroup, efficacy_threshold, ctrl_effect_mean, ctrl_effect_sigma, ctrl_effect_mean, ctrl_effect_sigma)
trials_parallel_d022 <- simulate_parallel_group_design(n_sim, N_perGroup, efficacy_threshold, ctrl_effect_mean, ctrl_effect_sigma, treat_effect_mean_d022, treat_effect_sigma)
trials_parallel_d035 <- simulate_parallel_group_design(n_sim, N_perGroup, efficacy_threshold, ctrl_effect_mean, ctrl_effect_sigma, treat_effect_mean_d035, treat_effect_sigma)
trials_parallel_d05 <- simulate_parallel_group_design(n_sim, N_perGroup, efficacy_threshold, ctrl_effect_mean, ctrl_effect_sigma, treat_effect_mean_d05, treat_effect_sigma)

alpha_parallel <- analyze_parallel_group_design(trials_parallel_d0, N_perGroup)
power_parallel_d022 <- analyze_parallel_group_design(trials_parallel_d022, N_perGroup)
power_parallel_d035 <- analyze_parallel_group_design(trials_parallel_d035, N_perGroup)
power_parallel_d05 <- analyze_parallel_group_design(trials_parallel_d05, N_perGroup)

alpha_parallel 
power_parallel_d022
power_parallel_d035
power_parallel_d05 


