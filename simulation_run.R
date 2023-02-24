library(tidyverse)
library(doParallel)
library(openxlsx)


source("create_cohort_initial.R")
source("coh_left_check.R")
source("check_new_compound.R")
source("total_n.R")
source("update_alloc_ratio.R")
source("generate_rand_list.R")
source("create_cohort_new.R")
source("make_decision_trialNEW.R")
source("make_decision_wrapper.R")
source("simulate_trial_MDD.R")
source("operating_characteristics.R")

#####################################
#specify possible effects
# names must be given as paste0(way_of_administration, "_Control") or paste0(way_of_administration, "_Treatment")
treatment_effects <- list(
  "TRD"=list("pill_Control"=list(list("mean"=c(32, 20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
             "pill_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,17.75),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   probs=c(0.25,0.25,0.25,0.25)),
             "IV_Control"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
             "IV_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,17.75),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 probs=c(0.25,0.25,0.25,0.25)),
             "nasal_Control"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1), 
             "nasal_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,17.75),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    probs=c(0.25,0.25,0.25,0.25))),
  
  "PRD"=list("pill_Control"=list(list("mean"=c(32, 20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
             "pill_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,17.75),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                   probs=c(0.25,0.25,0.25,0.25)),
             "IV_Control"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
             "IV_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,17.75),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                 probs=c(0.25,0.25,0.25,0.25)),
             "nasal_Control"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1), 
             "nasal_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,17.75),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    list("mean"=c(32,14.3),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                    probs=c(0.25,0.25,0.25,0.25)))
)

# specify cohorts and initial compounds
ways_of_administration=c("pill")
#ways_of_administration=c("pill", "nasal", "IV")
cohorts_start <- list("pill"=3)
#cohorts_start <- list("pill"=3, "IV"=1, "nasal"=1)
rand_type="block_sqrt_cap"

# specify probability of a new treatment becoming available per way of administration
prob_new_compound=c(0.2)
#prob_new_compound=c(0.2,0.1,0.1)

# specify initial compounds per population
cohorts_start_applic_to_TRD <- cohorts_start
applicable_to_PRD=FALSE
cohorts_start_applic_to_PRD <- list("pill"=0)
#cohorts_start_applic_to_PRD <- list("pill"=0, "IV"=0, "nasal"=0)
#applicable_to_PRD=TRUE
#cohorts_start_applic_to_PRD <- cohorts_start

################# SIMULATION ##################################################


start.time <- Sys.time()
cores <- detectCores() * 3/4
cl <- makeCluster(cores)
registerDoParallel(cl)

# setting seed ?

nsim <- 1000
sim_results <- NULL

patients_per_timepoint = list(c(7,7)
                              )
n_fin <- list(#list("TRD"=40,"PRD"=40), 
              #list("TRD"=60,"PRD"=60), 
              list("TRD"=80,"PRD"=80)#, 
              #list("TRD"=100,"PRD"=100), 
              #list("TRD"=120,"PRD"=120)
              )
rand_type <- list(#"block_1"#, 
                  #"block_k", 
                  #"block_sqrt",
                  "block_sqrt_cap"#,
                  #"full"
                  )

control_cap <- list(0.275,
                    0.3,
                    0.325,
                    0.35,
                    0.375,
                    0.4,
                    0.425,
                    0.45,
                    0.475,
                    0.5
                    )
ancova_period <- list(#TRUE,
                      FALSE
                      )
scenarios <- expand.grid("patients_per_timepoint"=patients_per_timepoint, 
                         "n_fin"=n_fin, 
                         "rand_type"=rand_type,
                         "control_cap"=control_cap,
                         "ancova_period"=ancova_period,
                         "pvals"=list(c(1,0.1)#,
                                      #c(1,0.1),
                                      #c(0.5,0.1)
                                      ))


for(i in 1:nrow(scenarios)){
  sim_results_tmp <- foreach(nsim=1:nsim, .combine=rbind, .packages=c("tidyverse", "mvtnorm")) %dopar% {
    single_sim_results <- simulate_trial(cohorts_start=cohorts_start, 
                                         n_int=lapply(scenarios$n_fin[[i]], function(x) ceiling(x/2)), 
                                         n_fin=scenarios$n_fin[[i]],
                                         treatment_effects=treatment_effects, 
                                         ways_of_administration=ways_of_administration,
                                         cohorts_start_applic_to_TRD=cohorts_start,
                                         applicable_to_PRD=applicable_to_PRD,
                                         cohorts_start_applic_to_PRD=cohorts_start_applic_to_PRD,
                                         sharing_type="concurrent",
                                         var_trend=0,
                                         control_cap=scenarios$control_cap[[i]],
                                         rand_type=scenarios$rand_type[[i]],
                                         patients_per_timepoint=scenarios$patients_per_timepoint[[i]], 
                                         prob_new_compound=prob_new_compound,
                                         new_compounds="single",
                                         max_treatments=c(6), 
                                         number_of_compounds_cap="global",
                                         #trial_end="pipeline", pipeline_size=c(10,4,4),
                                         trial_end="timepoint", 
                                         latest_timepoint_treatment_added=60*4,
                                         p_val_interim=scenarios$pvals[[i]][1], 
                                         p_val_final=scenarios$pvals[[i]][2],
                                         ancova_period=scenarios$ancova_period[[i]],
                                         sided="two_sided")
    
    ocs <- data.frame(operating_characteristics(single_sim_results) %>% 
                        rownames_to_column("armID") %>% 
                        mutate(admin = gsub("_.*", "", armID), 
                               treatment_ID = case_when(grepl("Control", armID) ~ "Control", TRUE ~ gsub(".*_Treatment", "", armID))) %>% 
                        relocate(admin, treatment_ID, .before=armID) %>% 
                        select(-armID),
                      "nsim"=nsim) %>% 
      # add specific information about the scenarios
      mutate(N = scenarios$n_fin[[i]][[1]], 
             patients_per_timepoint = scenarios$patients_per_timepoint[[i]][1],
             rand_type = scenarios$rand_type[[i]],
             control_cap = scenarios$control_cap[[i]],
             ancova_period = scenarios$ancova_period[[i]],
             pvalues = paste(scenarios$pvals[[i]], collapse=","))
    
    return(ocs)
  }
  sim_results <- rbind(sim_results, sim_results_tmp %>% 
                         add_column("scenarioID"=i))
  print(paste0(nsim*i, 
               " Simulations took ", 
               round(difftime(Sys.time(), start.time, units="min"), 2), 
               " minutes"))
}
stopCluster(cl)
sim_results <- as_tibble(sim_results)

############
sim_results %>% filter(treatment_ID != "Control") %>%
  select(nsim, rand_type, decisions_TRD, d_TRD, d_TRD_est) %>%
  mutate(Allocation =factor(rand_type, levels=c("block_1", "block_k", 
                                                "block_sqrt",
                                                "block_sqrt_cap",
                                                "full"))) %>% 
  mutate(decisions_TRD = factor(decisions_TRD, levels=c("success", "stopped early", "failure")),
         d = factor(round(d_TRD,2))) %>% 
  group_by(d, decisions_TRD, Allocation, .drop=FALSE) %>% summarise(n=n()) %>%
  group_by(d, Allocation) %>% mutate(percentage = 100 * n / sum(n)) %>% filter(decisions_TRD == "success")

########

#write.xlsx(sim_results, "alloc_test_fixed_n8.xlsx")
write.xlsx(sim_results, "control_cap_start3_fut.xlsx")

saveRDS(sim_results, "control_cap_start3_fut.rds")

