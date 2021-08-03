# This is a function to run the simulations

library(doParallel)
library(tidyverse)

source("create_cohort_initial.R")
source("coh_left_check.R")
source("total_n.R")
source("update_alloc_ratio.R")
source("create_cohort_new.R")
source("make_decision_trialNEW.R")
source("make_decision_wrapper.R")
source("simulate_trial_MDD.R")
source("operating_characteristics.R")


cohorts_start <- list("pill"=3, "IV"=2, "nasal"=2)

# probability of a treatment being applicable to PRD and TRD patients
cohorts_start_applic_to_TRD <- list("pill"=3, "IV"=2, "nasal"=2)
cohorts_start_applic_to_PRD <- list("pill"=3, "IV"=2, "nasal"=2)

# names must be given as paste0(way_of_administration, "_Control") or paste0(way_of_administration, "_Treatment")
treatment_effects <- list("TRD"=list("pill_Control"=list(list("mean"=c(32, 20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
                                     "pill_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                           list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                           list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                           probs=c(0.3,0.4,0.3)),
                                     "IV_Control"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
                                     "IV_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                         list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                         list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                         probs=c(0.3,0.4,0.3)),
                                     "nasal_Control"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1), 
                                     "nasal_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                            list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                            list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                            probs=c(0.3,0.4,0.3))),
                          
                          "PRD"=list("pill_Control"=list(list("mean"=c(15, 10),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
                                     "pill_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                           list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                           list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                           probs=c(0.3,0.4,0.3)),
                                     "IV_Control"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1),
                                     "IV_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                         list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                         list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                         probs=c(0.3,0.4,0.3)),
                                     "nasal_Control"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), probs=1), 
                                     "nasal_Treatment"=list(list("mean"=c(32,20),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                            list("mean"=c(32,17.5),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                            list("mean"=c(32,16),"sigma"=matrix(c(31.75, 13.4, 13.4, 123.96), ncol=2)), 
                                                            probs=c(0.3,0.4,0.3))))




start.time <- Sys.time()
cores <- detectCores() * 3/4
cl <- makeCluster(cores)
registerDoParallel(cl)

# setting seed ?

nsim <- 100

sim_results <- foreach(nsim=1:nsim, .combine=rbind, .packages=c("tidyverse", "mvtnorm")) %dopar% {
  single_sim_results <- simulate_trial(cohorts_start=cohorts_start, n_int=list("TRD"=50,"PRD"=50), n_fin=list("TRD"=100,"PRD"=100),
                                      treatment_effects=treatment_effects, ways_of_administration=c("pill", "IV", "nasal"),
                                      alloc_ratio_administration="fixed", alloc_ratio_control="fixed",
                                      alloc_ratio_administration_values=NULL, alloc_ratio_control_values=0.35,
                                      cohorts_start_applic_to_TRD=cohorts_start_applic_to_TRD, cohorts_start_applic_to_PRD=cohorts_start_applic_to_PRD,
                                      sharing_type="all",
                                      patients_per_timepoint=c(30,30), cohorts_per_timepoint=c(0.5,0.2,0.2), max_treatments=c(4,3,3), 
                                      latest_timepoint_treatment_added=60, p_val_interim=0.4, p_val_final=0.1)
  
  ocs <- data.frame(operating_characteristics(single_sim_results) %>% 
                      rownames_to_column("armID") %>% 
                      mutate(admin = gsub("_.*", "", armID), 
                             treatment_ID = case_when(grepl("Control", armID) ~ "Control", TRUE ~ gsub(".*_Treatment", "", armID))) %>% 
                      relocate(admin, treatment_ID, .before=armID) %>% select(-armID),
                    "nsim"=nsim)
  
  return(ocs)
}
#stop cluster
stopCluster(cl)
print(paste0(nsim, " Simulations took ", round(difftime(Sys.time(), start.time, units="min"), 2), " minutes"))

sim_results <- as_tibble(sim_results)




# decisions
sim_results %>% 
  filter(decisions_TRD != "ERROR") %>% 
  mutate(decisions_TRD = factor(decisions_TRD, levels=c("success", "stopped early", "failure")),
         cohens_d = factor(round(cohens_d_TRD,2))) %>% 
  group_by(admin, cohens_d, decisions_TRD, .drop=FALSE) %>% summarise(n=n()) %>%
  group_by(admin, cohens_d) %>% mutate(percentage = 100 * n / sum(n)) %>% 
  ggplot(., aes(admin, percentage, color=decisions_TRD)) + geom_point(size=2, alpha = 1, position=position_dodge(width=0.5)) +
  scale_y_continuous(limits=c(0,100), breaks = seq(0,100,by=20), minor_breaks = seq(0,100,by=10)) + 
  xlab("Way of administration") + 
  # scale_x_discrete(labels=newlabels, limits=positions, position=c("bottom")) + 
  scale_color_viridis_d(begin = 0.1, end = 0.9) +
  ylab("Percentage") + #ggtitle(title) +
  #geom_errorbar(aes(Prior, y=Proportion, ymin=lower, ymax=upper), size=0.5, position=position_dodge(width=0.5)) + 
  facet_grid(~ cohens_d) +
  theme_bw()


# sample size with boxplots


# boxplot for estimation of Cohen's d


# duration


# number of treatment arms per admin