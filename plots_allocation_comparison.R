## comparison between the different allocation methods
# fixed: N = 80, 

library(tidyverse)
library(openxlsx)
library(ggplot2)
library(ggpubr)

sim_results_7 <- sim_results %>% filter(patients_per_timepoint==7)

####################### SAMPLE SIZES ###########################################

# analyse patients needed per platform with regard to the allocation method
n_perPlatform <- sim_results_1 %>% select(nsim, rand_type, admin, n_TRD) %>% 
  mutate(Allocation =factor(rand_type, levels=c("block_1", "block_k", 
                                                "block_sqrt",
                                                "block",
                                                "full"))) %>%
  group_by(nsim, Allocation) %>% summarise(n_platform = sum(n_TRD))

n_perPlatform_mean <- n_perPlatform %>% 
  group_by(Allocation) %>% 
  summarise(n_platform_mean = sum(n_platform) / max(sim_results_7$nsim))
write.xlsx(n_perPlatform_mean, "n_perPlatform_mean.xlsx")

##
p_nTotal <- 
  n_perPlatform_mean %>% ggplot(aes(x = Allocation, y = n_platform_mean, group =1))+ 
  ylim(1500, 2500)+
  #geom_line() + 
  geom_point(size = 2) +
  theme_bw()+
  ylab("Sample size per platform") + xlab("Allocation method") +
  ggtitle("Size of platform trial")
ggsave("N_per_platform.tiff", device = "tiff", width=9, height=4)

# sample size per platform with boxplots
ggplot(n_perPlatform,
       aes(x=n_platform,
           y=Allocation)) +
  scale_y_discrete(labels=c("block_1"="1:1", "block_k"="1:k", "block_sqrt"="1:sqrt(k)")) +
  geom_boxplot(fill="#69b3a2") +
  xlim(1400, 2500)+
  coord_flip() +
  xlab("Sample size per platform") + ylab("Allocation method") +
  ggtitle("Size of platform trial") +
  theme_bw()
ggsave("SampleSize_withBoxes.tiff", device = "tiff", width=7, height=4)

########## NUMBER OF CONTROLS

tibble_n_control_platform <- sim_results_1 %>% 
  filter(treatment_ID == "Control") %>% 
  select(nsim, rand_type, n_TRD) %>% 
  mutate(Allocation =factor(rand_type, levels=c("block_1", "block_k", 
                                                "block_sqrt",
                                                "block",
                                                "full"))) %>%
  group_by(nsim, Allocation) %>% summarise(n_control_platform = sum(n_TRD))

ggplot(tibble_n_control_platform,
       aes(x=n_control_platform,
           y=Allocation)) +
  scale_y_discrete(labels=c("block_1"="1:1", "block_k"="1:k", "block_sqrt"="1:sqrt(k)")) +
  geom_boxplot(fill="#69b3a2") +
  coord_flip() +
  xlab("Control size per platform") + ylab("Allocation method") +
  ggtitle("Number of controls per platform trial") +
  theme_bw()
ggsave("ControlSize_withBoxes.tiff", device = "tiff", width=7, height=4)

n_perPlatform_v2 <- n_perPlatform %>% add_column(type = "platform") %>% rename(size=n_platform)
tibble_n_control_platform <- tibble_n_control_platform %>% add_column(type = "control") %>% rename(size=n_control_platform)

tibble_ctrlAndTotal <- bind_rows(n_perPlatform_v2, tibble_n_control_platform)

## compared with total number of individuals in platform
ggplot(tibble_ctrlAndTotal, aes(x = size, y = Allocation,
                                #group = type, 
                                color = type)) +
  scale_y_discrete(labels=c("block_1"="1:1", "block_k"="1:k", "block_sqrt"="1:sqrt(k)")) +
  coord_flip() +
  geom_boxplot(width=0.5, position=position_dodge(width=0)#, outlier.shape = NA, #coef=0
  )  +
  xlab("Sample size in platform") + ylab("Allocation method") +
  ggtitle("Sample size of the complete platform trial and the overall controls") +
  theme_bw()
ggsave("Control_and_total.tiff", device = "tiff", width=7, height=4)

tibble_n_onTreatment <- sim_results_1 %>% filter(treatment_ID != "Control") %>% 
  select(nsim, rand_type, admin, n_TRD) %>% 
  mutate(Allocation =factor(rand_type, levels=c("block_1", "block_k", 
                                                "block_sqrt",
                                                "block",
                                                "full"))) %>%
  group_by(nsim, Allocation) %>% summarise(n_onTreatment = sum(n_TRD)) %>% 
  add_column(type = "on treatment") %>% rename(size=n_onTreatment)

tibble_ctrlAndTreatments <- bind_rows(tibble_ctrlAndTotal, tibble_n_onTreatment)

#compared with total number of individuals in platform
ggplot(tibble_ctrlAndTreatments, aes(x = size, y = Allocation,
                                     #group = type, 
                                     color = type)) +
  scale_y_discrete(labels=c("block_1"="1:1", "block_k"="1:k", "block_sqrt"="1:sqrt(k)")) +
  coord_flip() +
  geom_boxplot(width=0.5, #outlier.shape = NA, 
               position=position_dodge(width=0), #coef=0
  )  +
  xlim(0, 2500) +
  xlab("Sample size in platform") + ylab("Allocation method") +
  ggtitle("Sample size of the complete platform trial and the overall treatments and controls") +
  theme_bw()
ggsave("Control_treatment_and_total.tiff", device = "tiff", width=9, height=4)

####### ON TRIAL LEVEL
tibble_trial_level <- sim_results_alloc %>% 
  mutate(Allocation =factor(rand_type, levels=c("block_1", "block_k", 
                                                "block_sqrt",
                                                "block",
                                                "full"))) %>%
  filter(treatment_ID != "Control") %>%
  select(nsim, Allocation, n_TRD, n_control_comparators_TRD)

# on active treatment
ggplot(tibble_trial_level, aes(x=n_TRD, y=Allocation)) +
  geom_boxplot(fill="#69b3a2"#,width =0.25
  ) +
  scale_y_discrete(labels=c("block_1"="1:1", "block_k"="1:k", "block_sqrt"="1:sqrt(k)")) +
  coord_flip() +
  theme_bw() +
  xlim(70, 100) +
  xlab("Patients per arm") + ylab("Allocation method") +
  ggtitle("Patients per experimental treatment arm") 
ggsave("n_per_treatment.png", device = "png", width=7, height=5)

# control comparators per decision
ggplot(tibble_trial_level, aes(x=n_control_comparators_TRD, y=Allocation)) +
  geom_boxplot(fill="#69b3a2"#,width =0.25
  ) +
  scale_y_discrete(labels=c("block_1"="1:1", "block_k"="1:k", "block_sqrt"="1:sqrt(k)")) +
  coord_flip() +
  theme_bw() +
  xlim(0, 600) +
  xlab("Controls per comparison") + ylab("Allocation method") +
  ggtitle("Control comparators per experimental treatment arm") 
ggsave("ControlComparators.png", device = "png", width=7, height=5)

####################### ARMS ###################################################

# data number of arms per admin (excluding control)
tibble_arms <- sim_results_7 %>% 
  filter(treatment_ID != "Control") %>% 
  group_by(nsim, rand_type) %>% count() %>%
  mutate(Allocation =factor(rand_type, levels=c("block_1", "block_k", 
                                                "block_sqrt",
                                                "block",
                                                "full"))) %>%
  group_by(nsim, Allocation) %>% summarise(arms = sum(n))

number_of_arms_mean <- tibble_arms %>% 
  group_by(Allocation) %>% 
  summarise(arms_mean = sum(arms) / max(sim_results_7$nsim))
write.xlsx(number_of_arms_mean, "number_of_arms.xlsx")

# plot arms per platform (excluding control)
ggplot(tibble_arms,
       aes(x=arms,
           y=Allocation)) +
  scale_y_discrete(labels=c("block_1"="1:1", "block_k"="1:k", "block_sqrt"="1:sqrt(k)")) +
  geom_boxplot(fill="#69b3a2") +
  xlim(10, 30)+
  coord_flip() +
  xlab("Treatments per platform") + ylab("Allocation method") +
  ggtitle("Number of arms per platform trial") +
  theme_bw()
ggsave("Arms_with_boxes.tiff", device = "tiff", width=9, height=4)


####################### DURATION ###############################################

tibble_duration <- sim_results_1 %>%  
  filter(treatment_ID != "Control") %>%
  select(nsim, rand_type, admin, duration_TRD) %>% 
  mutate(Allocation =factor(rand_type, levels=c("block_1", "block_k", 
                                                "block_sqrt",
                                                "block",
                                                "full"))) 
#minimum duartion tibble_duration$duration_TRD
#tibble_duration %>% filter(N =="40") #%>% #select(duration_TRD) %>% 
#summarise(min_val=min(duration_TRD))

#median duration 
tibble_median_duration <-  tibble_duration %>% group_by(Allocation) %>% summarise(median_duration= mean(duration_TRD))
write.xlsx(tibble_median_duration, "median_duration.xlsx")

ggplot(tibble_duration, aes(x=duration_TRD, y=Allocation)) +
  geom_boxplot(fill="#69b3a2"#,width =0.25
               ) +
  scale_y_discrete(labels=c("block_1"="1:1", "block_k"="1:k", "block_sqrt"="1:sqrt(k)")) +
  coord_flip() +
  theme_bw() +
  xlim(0, 200) +
  xlab("Duration per treatment arm in weeks") + ylab("Allocation method") +
  ggtitle("Average duration of treatment arms over all possible effect sizes") 
ggsave("Duration_withBoxes.tiff", device = "tiff", width=7, height=4)


####################### DECISIONS ##############################################

scenario_labeller <- labeller(
  `d` = c(`0` = "d = 0", `0.2` = "d = 0.2", `0.35` = "d = 0.35", `0.5` = "d = 0.50"),
  `rand_type_o` = c('block_1' = "1:1", 'block_k' = "1:k", 'block_sqrt' = "1:sqrt(k)", 'block' = "with cap", 'full' = "full")
)
# decisions
sim_results %>% filter(patients_per_timepoint==7) %>% 
  filter(treatment_ID != "Control") %>% 
  mutate(decisions_TRD = factor(decisions_TRD, levels=c("success", #"stopped early", 
                                                        "failure")),
         d = factor(round(d_TRD,2)),
         rand_type_o = factor(rand_type, levels = c('block_1', 'block_k', 'block_sqrt', 'block', 'full'))
         ) %>% 
  group_by(admin, d, decisions_TRD, rand_type_o, .drop=FALSE) %>% summarise(n=n()) %>%
  group_by(admin, d, rand_type_o) %>% mutate(percentage = 100 * n / sum(n)) %>% 
  ggplot(., aes(admin, 
                percentage, 
                color=decisions_TRD)) + 
  geom_point(size=2, alpha = 1, position=position_dodge(width=0.5)) +
  scale_y_continuous(limits=c(0,100), 
                     breaks = seq(0,100,by=20), 
                     minor_breaks = seq(0,100,by=10)) + 
  xlab("Way of administration") + 
  scale_color_viridis_d(begin = 0.1, 
                        end = 0.9, 
                        name="Decision") +
  ylab("Percentage") + 
  geom_hline(data = data.frame(y = c(5, 80, 80, 80), 
                               d = as.factor(c(0,0.2,0.35,0.5))), 
             aes(yintercept=y), linetype="dotted") +
  facet_grid(rand_type_o ~ d, labeller = scenario_labeller) +
  theme_bw() + 
  ggtitle("Percentage of each decision for various true effect sizes and under different allocation methods")

ggsave("decisions_TRD.tiff", device = "tiff", width=12, height=12)

####################### POWER ##################################################

# data decisions in platform per admin and effect size
pow <- sim_results %>% filter(treatment_ID != "Control") %>%
  select(nsim, rand_type, decisions_TRD, d_TRD, d_TRD_est) %>%
  mutate(Allocation =factor(rand_type, levels=c("block_1", "block_k", 
                                                "block_sqrt",
                                                "block",
                                                "full"))) %>% 
  mutate(decisions_TRD = factor(decisions_TRD, levels=c("success", "stopped early", "failure")),
         d = factor(round(d_TRD,2))) %>% 
  group_by(d, decisions_TRD, Allocation, .drop=FALSE) %>% summarise(n=n()) %>%
  group_by(d, Allocation) %>% mutate(percentage = 100 * n / sum(n)) %>% filter(decisions_TRD == "success")
write.xlsx(pow, "power.xlsx")

# plot for d = 0.5
p_pow05 <- pow %>% filter(d ==0.5) %>%
  ggplot(aes(x = Allocation, y = percentage,
             group = d, 
             color = d,
             linetype = d
  )) + 
  geom_point(size = 2) +
  theme_bw() +
  ggtitle("d = 0.5") + theme(plot.title=element_text(hjust=0.5))+ 
  ylim(0, 100) +
  geom_hline(yintercept = 80, linetype="dotted") +
  scale_x_discrete(labels=c("block_1"="1:1", "block_k"="1:k", "block_sqrt"="1:sqrt(k)")) +
  labs(y="Power in %")

## 0.35
p_pow035 <- pow %>% filter(d ==0.35) %>%
  ggplot(aes(x = Allocation, y = percentage,
             group = d, 
             color = d,
             linetype = d
  )) + 
  geom_point(size = 2) +
  theme_bw() +
  ggtitle("d = 0.35") + theme(plot.title=element_text(hjust=0.5))+ 
  ylim(0, 100) +
  geom_hline(yintercept = 80, linetype="dotted") +
  scale_x_discrete(labels=c("block_1"="1:1", "block_k"="1:k", "block_sqrt"="1:sqrt(k)")) +
  labs(y="Power in %")

## 0.2
p_pow02 <- pow %>% filter(d ==0.2) %>%
  ggplot(aes(x = Allocation, y = percentage,
             group = d, 
             color = d,
             linetype = d
  )) + 
  geom_point(size = 2) +
  theme_bw() +
  ggtitle("d = 0.2") + theme(plot.title=element_text(hjust=0.5))+ 
  ylim(0, 100) +
  #geom_hline(yintercept = 80, linetype="dotted") +
  scale_x_discrete(labels=c("block_1"="1:1", "block_k"="1:k", "block_sqrt"="1:sqrt(k)")) +
  labs(y="Power in %")

## 0
p_pow0 <- pow %>% filter(d ==0) %>%
  ggplot(aes(x = Allocation, y = percentage,
             group = d, 
             color = d,
             linetype = d
  )) + 
  geom_point(size = 2) +
  theme_bw() +
  ggtitle("d = 0") + theme(plot.title=element_text(hjust=0.5))+ 
  ylim(0, 100) +
  geom_hline(yintercept = 5, linetype="dotted") +
  scale_x_discrete(labels=c("block_1"="1:1", "block_k"="1:k", "block_sqrt"="1:sqrt(k)")) +
  labs(y="Type I error in %")

#arrange the 4 plots together
p_successRates <- ggarrange(p_pow0, p_pow02, p_pow035, p_pow05,
                            ncol = 2, nrow = 2,
                            legend = "none"
                            #common.legend = TRUE,
                            #legend = "bottom" 
                            #legend.grob = 
)
annotate_figure(p_successRates,
                top = text_grob("Success rates for the different allocation ratios in the platform",
                                #color = "red", 
                                #face = "bold", 
                                size = 14)
)
ggsave("Success_rates.tiff", device = "tiff", width=9, height=7)


#arrange the 3 plots together
p_successRates_v2 <- ggarrange(p_pow02, p_pow035, p_pow05,
                            ncol = 3, nrow = 1,
                            legend="none" 
)
annotate_figure(p_successRates_v2,
                top = text_grob("Power for the different allocation ratios in the platform",
                                #color = "red", 
                                #face = "bold", 
                                size = 14)
)
ggsave("Power_3.tiff", device = "tiff", width=9, height=4)


##################################### estimation of effect ####################

effect_size <- sim_results %>% filter(patients_per_timepoint==7) %>%
  filter(treatment_ID != "Control") %>% 
  mutate(d = factor(round(d_TRD,2)),
         Allocation =factor(rand_type, levels=c("block_1", "block_k", 
                                                "block_sqrt",
                                                "block",
                                                "full"))
  ) %>% select(nsim, Allocation, d, d_TRD_est) %>% 
  group_by(d, Allocation) %>% summarise(est = mean(d_TRD_est))
write.xlsx(effect_size, "effect_size.xlsx")

scenario_labeller <- labeller(
  `d` = c(`0` = "d = 0", `0.2` = "d = 0.2", `0.35` = "d = 0.35", `0.5` = "d = 0.50"),
  `rand_type_o` = "Allocation"#c('block_1' = "1:1", 'block_k' = "1:k", 'block_sqrt' = "1:sqrt(k)", 'block' = "with cap", 'full' = "full")
)
sim_results %>% filter(patients_per_timepoint==7) %>%
  filter(treatment_ID != "Control") %>% 
  mutate(d = factor(round(d_TRD,2)),
         rand_type_o = factor(rand_type, levels = c('block_1', 'block_k', 'block_sqrt', 'block', 'full'))
         ) %>%
  ggplot(.) + 
  geom_violin(aes(x=rand_type_o, 
                  y=d_TRD_est, 
                  fill=rand_type_o)) +
  geom_hline(data = data.frame(y = c(0, 0.2, 0.35, 0.5), 
                               d = as.factor(c(0,0.2,0.35, 0.5))), 
             aes(yintercept=y), linetype="dotted") +
  scale_fill_discrete("Allocation",
                      breaks=c('block_1', 'block_k', 'block_sqrt', 'block', 'full'),
                      labels=c("1:1", "1:k", "1:sqrt(k)", "with cap", "full")
                      )+
  scale_x_discrete(breaks=c())+
  facet_wrap(~d, nrow=1, 
             labeller = scenario_labeller) +
  theme_bw() + 
  ylab("Estimated Effect Size") + 
  xlab("Allocation") +
  ggtitle("Estimation of effect size") #+ 
  #guides(fill=guide_legend(title="Allocation"))
ggsave("effect_size.tiff", device = "tiff", width=12, height=5)

