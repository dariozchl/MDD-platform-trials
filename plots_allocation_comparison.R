## comparison between the different allocation methods
# fixed: N = 80, 

library(tidyverse)
library(openxlsx)
library(ggplot2)
library(vioplot)
library(ggpubr)
library(xtable)

sim_results_rct_s <-  sim_results_rct %>% add_column(Allocation = "rcts") %>% select(nsim, Allocation, admin, n_TRD)
rct_add <- sim_results_rct %>% add_column(Allocation ="rcts") %>% #select(nsim, allocation, admin, n_TRD) %>%
  mutate(Allocation = factor(allocation, levels = c("rcts")))# %>%
  #group_by(nsim, Allocation) %>% summarise(n_platform = sum(n_TRD))

sim_results_plat_2 <- sim_results %>% mutate(Allocation =factor(rand_type, levels=c("block_1", "block_k", 
                                                                                     "block_sqrt",
                                                                                     "block_sqrt_cap",
                                                                                     "full")))

n_comp <- bind_rows(sim_results_rct_s, n_perPlatform)

sim_results_both <- bind_rows(rct_add,sim_results_plat_2)


####################### SAMPLE SIZES ###########################################

# analyse patients needed per platform with regard to the allocation method
n_platform <- sim_results  %>% select(nsim, rand_type, admin, n_TRD) %>% 
  mutate(Allocation =factor(rand_type, levels=c("block_1", "block_k", 
                                                "block_sqrt",
                                                "block_sqrt_cap",
                                                "full")))
n_perPlatform <- sim_results %>%
  mutate(Allocation =factor(rand_type, levels=c("block_1", "block_k", 
                                                "block_sqrt",
                                                "block_sqrt_cap",
                                                "full"))) %>%
  group_by(nsim, Allocation) %>% summarise(n_platform = sum(n_TRD))

n_perPlatform_mean <- n_perPlatform %>% 
  group_by(Allocation) %>% 
  summarise(n_platform_mean = sum(n_platform) / max(sim_results $nsim))
write.xlsx(n_perPlatform_mean, "n_perPlatform_mean_equal.xlsx")

##
p_nTotal <- 
  n_perPlatform_mean %>% ggplot(aes(x = Allocation, y = n_platform_mean, group =1))+ 
  ylim(1500, 2500)+
  #geom_line() + 
  geom_point(size = 2) +
  theme_bw()+
  ylab("Sample size per platform") + xlab("Allocation method") +
  ggtitle("Size of platform trial")
#ggsave("N_per_platform.tiff", device = "tiff", width=9, height=4)

# sample size per platform with boxplots
ggplot(n_perPlatform,
       aes(x=n_platform,
           y=Allocation)) +
  scale_y_discrete(labels=c("rcts" = "sequential rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block_sqrt_cap"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +  #geom_boxplot(fill="#69b3a2") +
  geom_violin(aes(fill = Allocation), trim = TRUE, width=1) +
  geom_boxplot(width = 0.1, alpha=0.6 #aes(fill = Allocation), color = "white"
               ) +
  #theme_classic() +
  theme_bw()+
  theme(legend.position = "none")+
  #xlim(1400, 2500)+
  coord_flip() +
  xlab("Sample size per platform") + ylab("Allocation method") +
  ggtitle("Size of platform trial") 
  
ggsave("SampleSize_equal_all_025.png", device = "png", width=8, height=5)

########## NUMBER OF CONTROLS

tibble_n_control_platform <- sim_results_both %>% 
  filter(treatment_ID == "Control") %>% 
  group_by(nsim, Allocation) %>% summarise(n_control_platform = sum(n_TRD))

ggplot(tibble_n_control_platform,
       aes(x=n_control_platform,
           y=Allocation)) +
  scale_y_discrete(labels=c("rcts" = "sequential rcts","block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +  geom_boxplot(fill="#69b3a2") +
  coord_flip() +
  xlab("Control size per platform") + ylab("Allocation method") +
  ggtitle("Number of controls per platform trial") +
  theme_bw()
ggsave("ControlSize_withBoxes.png", device = "png", width=7, height=4)

n_perPlatform_v2 <- n_both %>% add_column(type = "platform") %>% rename(size=n_platform)
tibble_n_control_platform <- tibble_n_control_platform %>% add_column(type = "control") %>% rename(size=n_control_platform)

tibble_ctrlAndTotal <- bind_rows(tibble_n_control_platform, n_perPlatform_v2)

## compared with total number of individuals in platform
ggplot(tibble_ctrlAndTotal, aes(x = size, y = Allocation,
                                #group = type, 
                                color = type)) +
  scale_y_discrete(labels=c("rcts" = "sequential rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +  coord_flip() +
  geom_boxplot(width=0.5, position=position_dodge(width=0)#, outlier.shape = NA, #coef=0
  )  +
  xlab("Sample size in platform") + ylab("Allocation method") +
  ggtitle("Sample size of the complete platform trial and the overall controls") +
  theme_bw()
ggsave("Control_and_total.png", device = "png", width=7, height=5)

tibble_n_onTreatment <- sim_results_both %>% filter(treatment_ID != "Control") %>% 
  group_by(nsim, Allocation) %>% summarise(n_onTreatment = sum(n_TRD)) %>% 
  add_column(type = "on treatment") %>% rename(size=n_onTreatment)

tibble_ctrlAndTreatments <- bind_rows(tibble_ctrlAndTotal, tibble_n_onTreatment)

#compared with total number of individuals in platform
ggplot(tibble_ctrlAndTreatments, aes(x = size, y = Allocation,
                                     #group = type, 
                                     color = type#, fill = type
                                     )) +
  scale_y_discrete(labels=c("rcts" = "sequential rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +  coord_flip() +
  geom_boxplot(width=0.5, #outlier.shape = NA, 
               position=position_dodge(width=0), #coef=0
  )  +
  #geom_violin(trim = FALSE, width=4.75, position=position_dodge(width=0)) +
  #geom_boxplot(width = 0.1, alpha=0.6 #aes(fill = Allocation), color = "white"
  #) +
  xlim(0, 2500) +
  xlab("Sample size in platform") + ylab("Allocation method") +
  ggtitle("Sample size of the complete platform trial and the overall treatments and controls") +
  theme_bw()
ggsave("Control_treatment_and_total_box.png", device = "png", width=9, height=4)

####### ON TRIAL LEVEL
tibble_trial_level <- sim_results_both  %>% 
  filter(treatment_ID != "Control") %>%
  select(nsim, Allocation, n_TRD, n_control_comparators_TRD)

tibble_trial_level_v2 <- tibble_trial_level %>% filter(Allocation %in% c("block", "full"))

# on active treatment
ggplot(tibble_trial_level, aes(x=n_TRD, y=Allocation)) +
  geom_violin(fill="#69b3a2", trim = TRUE) +
  geom_boxplot(width =0.25, alpha=0.6
  ) +
  scale_y_discrete(labels=c("rcts" = "sequential rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +  coord_flip() +
  theme_bw() +
  #xlim(75, 87) +
  xlab("Patients per arm") + ylab("Allocation method") +
  ggtitle("Patients per experimental treatment arm") 
ggsave("n_per_treatment_comp.png", device = "png", width=8, height=5)

# control comparators per decision
ggplot(tibble_trial_level, aes(x=n_control_comparators_TRD, y=Allocation)) +
  #geom_violin(fill="#69b3a2", trim = TRUE, width=1,position=position_dodge(width=0)) +
  geom_boxplot(width =0.75, alpha=0.6
  ) +
  scale_y_discrete(labels=c("rcts" = "sequential rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +  coord_flip() +
  theme_bw() +
  xlim(0, 600) +
  xlab("Controls per comparison") + ylab("Allocation method") +
  ggtitle("Control comparators per experimental treatment arm") 
ggsave("ControlComparators_box.png", device = "png", width=8, height=5)

####################### ARMS ###################################################

# data number of arms per admin (excluding control)
tibble_arms <- sim_results %>% 
  mutate(Allocation =factor(rand_type, levels=c("block_1", "block_k", 
                                                "block_sqrt",
                                                "block_sqrt_cap",
                                                "full"))) %>%
  filter(treatment_ID != "Control") %>% 
  group_by(nsim, Allocation) %>% count() %>%
  group_by(nsim, Allocation) %>% summarise(arms = sum(n))

number_of_arms_mean <- tibble_arms %>% 
  group_by(Allocation) %>% 
  summarise(arms_mean = sum(arms) / max(sim_results$nsim))
write.xlsx(number_of_arms_mean, "number_of_arms.xlsx")

# plot arms per platform (excluding control)
ggplot(tibble_arms,
       aes(x=arms,
           y=Allocation)) +
  scale_y_discrete(labels=c("rcts" = "sequential rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +  
  #geom_violin(fill="#69b3a2", trim = TRUE, width=1) +
  geom_boxplot(width =0.1, alpha=0.6
  ) +
  #geom_boxplot(fill="#69b3a2") +
  #xlim(10, 30)+
  coord_flip() +
  xlab("Treatments per platform") + ylab("Allocation method") +
  ggtitle("Number of arms per platform trial") +
  theme_bw()
ggsave("Arms_comp.png", device = "png", width=9, height=5)


####################### DURATION ###############################################

tibble_duration <- sim_results_both %>%  
  filter(treatment_ID != "Control")
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
  geom_boxplot(fill="#69b3a2") +
  #geom_violin(fill="#69b3a2", trim = TRUE, width=1) +
  #geom_boxplot(width =0.1, alpha=0.6
  #) +
  scale_y_discrete(labels=c("rcts" = "sequential rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +
  coord_flip() +
  theme_bw() +
  xlim(0, 200) +
  xlab("Duration per treatment arm in weeks") + ylab("Allocation method") +
  ggtitle("Average duration of treatment arms over all possible effect sizes") 
ggsave("Duration_boxes.png", device = "png", width=8, height=5)


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

ggsave("decisions_TRD.png", device = "png", width=12, height=12)

####################### POWER ##################################################

# data decisions in platform per admin and effect size
pow <- sim_results_both %>% filter(treatment_ID != "Control") %>%
  select(nsim, Allocation, decisions_TRD, d_TRD, d_TRD_est) %>%
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
  scale_x_discrete(labels=c("rcts" = "sequential \n rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +
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
  scale_x_discrete(labels=c("rcts" = "sequential \n rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +
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
  scale_x_discrete(labels=c("rcts" = "sequential \n rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +
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
  scale_x_discrete(labels=c("rcts" = "sequential \n rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +
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
ggsave("Success_rates_comp.png", device = "png", width=9, height=7)


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
ggsave("Power_3.png", device = "png", width=14, height=4)


##################################### estimation of effect ####################

effect_size <- sim_results_both %>% filter(patients_per_timepoint==7) %>%
  filter(treatment_ID != "Control") %>% 
  mutate(d = factor(round(d_TRD,2))
  ) %>% select(nsim, Allocation, d, d_TRD_est) %>% 
  group_by(d, Allocation) %>% summarise(est = mean(d_TRD_est))
write.xlsx(effect_size, "effect_size.xlsx")

scenario_labeller <- labeller(
  `d` = c(`0` = "d = 0", `0.2` = "d = 0.2", `0.35` = "d = 0.35", `0.5` = "d = 0.50"),
  `Allocation_o` = "Allocation"#c('block_1' = "1:1", 'block_k' = "1:k", 'block_sqrt' = "1:sqrt(k)", 'block' = "with cap", 'full' = "full")
)
sim_results_both %>% filter(patients_per_timepoint==7) %>%
  filter(treatment_ID != "Control") %>% 
  mutate(d = factor(round(d_TRD,2)),
         Allocation_o = factor(Allocation, levels = c('rcts', 'block_1', 'block_k', 'block_sqrt', 'block', 'full'))
         ) %>%
  ggplot(.) + 
  geom_violin(aes(x=Allocation_o, 
                  y=d_TRD_est, 
                  fill=Allocation_o)) +
  geom_hline(data = data.frame(y = c(0, 0.2, 0.35, 0.5), 
                               d = as.factor(c(0,0.2,0.35, 0.5))), 
             aes(yintercept=y), linetype="dotted") +
  scale_fill_discrete("Allocation",
                      breaks=c('rcts','block_1', 'block_k', 'block_sqrt', 'block', 'full'),
                      labels=c("seqential rcts","balanced", "k", "sqrt(k)", "sqrt(k) with cap", "sqrt(k) with cap,\n no block")
                      )+
  scale_x_discrete(breaks=c())+
  facet_wrap(~d, nrow=1, 
             labeller = scenario_labeller) +
  theme_bw() + 
  ylab("Estimated Effect Size") + 
  xlab("Allocation") +
  ggtitle("Estimation of effect size") #+ 
  #guides(fill=guide_legend(title="Allocation"))
ggsave("effect_size.png", device = "png", width=12, height=5)

## Characteristic 4: Rejected arms per 1000 patients


# data decisions in platform per effect size
pow05 <- sim_results %>% filter(treatment_ID != "Control") %>%
  select(nsim, rand_type, decisions_TRD, d_TRD, d_TRD_est) %>%
  mutate(decisions_TRD = factor(decisions_TRD, levels=c("success", "stopped early", "failure")),
         d = factor(round(d_TRD,2))) %>% 
  filter(d==0.5) %>%
  group_by(d, decisions_TRD, rand_type, .drop=FALSE) %>% summarise(n=n()) %>%
  group_by(d, rand_type) %>% mutate(percentage = 100 * n / sum(n)) %>% filter(decisions_TRD == "success")
pow035 <- sim_results %>% filter(treatment_ID != "Control") %>%
  select(nsim, rand_type, decisions_TRD, d_TRD, d_TRD_est) %>%
  mutate(decisions_TRD = factor(decisions_TRD, levels=c("success", "stopped early", "failure")),
         d = factor(round(d_TRD,2))) %>% 
  filter(d==0.35) %>%
  group_by(d, decisions_TRD, rand_type, .drop=FALSE) %>% summarise(n=n()) %>%
  group_by(d, rand_type) %>% mutate(percentage = 100 * n / sum(n)) %>% filter(decisions_TRD == "success")
pow02 <- sim_results %>% filter(treatment_ID != "Control") %>%
  select(nsim, rand_type, decisions_TRD, d_TRD, d_TRD_est) %>%
  mutate(decisions_TRD = factor(decisions_TRD, levels=c("success", "stopped early", "failure")),
         d = factor(round(d_TRD,2))) %>% 
  filter(d==0.2) %>%
  group_by(d, decisions_TRD, rand_type, .drop=FALSE) %>% summarise(n=n()) %>%
  group_by(d, rand_type) %>% mutate(percentage = 100 * n / sum(n)) %>% filter(decisions_TRD == "success")
pow0 <- sim_results %>% filter(treatment_ID != "Control") %>%
  select(nsim, rand_type, decisions_TRD, d_TRD, d_TRD_est) %>%
  mutate(decisions_TRD = factor(decisions_TRD, levels=c("success", "stopped early", "failure")),
         d = factor(round(d_TRD,2))) %>% 
  filter(d==0) %>%
  group_by(d, decisions_TRD, rand_type, .drop=FALSE) %>% summarise(n=n()) %>%
  group_by(d, rand_type) %>% mutate(percentage = 100 * n / sum(n)) %>% filter(decisions_TRD == "success")


# data for the average number of rejections per sample size
rejections_cap <- 
  sim_results %>% 
  group_by(nsim, rand_type) %>% mutate(n_total = sum(n_TRD)) %>% #ungroup() %>% summarise(mean_n=mean(n_total)) #%>%
  filter(treatment_ID != "Control") %>% #select(nsim, n_total)
  mutate(decisions_TRD = factor(decisions_TRD, levels=c("success", "stopped early", "failure")),
         d = factor(round(d_TRD,2))) %>%
  filter(decisions_TRD == "success") %>%
  group_by(nsim, rand_type, d) %>% mutate(n_rejections=n()) %>% select(nsim, d, n_rejections, n_total) %>%
  #group_by(d) %>% mutate(mean_rejections = mean(n_rejections)) %>% unique() %>% 
  group_by(rand_type, d) %>% summarize(rpp=mean(n_rejections)/mean(n_total)*1000)

# date for the number of arms per effect size
arms_cap <- 
  sim_results %>% 
  group_by(nsim, rand_type) %>% mutate(n_total = sum(n_TRD)) %>% #ungroup() %>% summarise(mean_n=mean(n_total)) #%>%
  filter(treatment_ID != "Control") %>% #select(nsim, n_total)
  mutate(decisions_TRD = factor(decisions_TRD, levels=c("success", "stopped early", "failure")),
         d = factor(round(d_TRD,2))) %>%
  #filter(decisions_TRD == "success") %>%
  group_by(nsim, rand_type, d) %>% mutate(n_rejections=n()) %>% select(nsim, d, n_rejections, n_total) %>%
  group_by(rand_type, d) %>% summarize(rpp=mean(n_rejections)/mean(n_total)*1000)


# create combined plots of arms, rejections and power per effect size

## 0.5
data_05 <- rejections_cap %>% filter(d == "0.5") #%>%
arms_05 <- arms_cap %>% filter(d == "0.5")
p_05 <-  
  ggplot() + 
  geom_point(data=data_05, aes(x = rand_type, y = rpp,
                               group = rand_type
  ), color= "red", size = 2) +
  geom_point(data=arms_05, aes(x = rand_type, y = rpp,
                               group = rand_type, 
                               color = rand_type
  ), color="blue", size = 2) +
  theme_bw() +
  ggtitle("d=0.5") + theme(plot.title=element_text(hjust=0.5))+ 
  geom_point(data=pow05, aes(x = rand_type, y = percentage/20), color="lightblue") +
  geom_line(data=pow05, aes(x = rand_type, y = percentage/20), color="lightblue") +
  scale_x_discrete(labels=c("rcts" = "sequential \n rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block_sqrt_cap"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +
  labs(x="Allocation method") +
  scale_y_continuous(
    name = "Arms per 1000 patients",
    limits = c(0,5),
    sec.axis = sec_axis(~.*20, name="Power in %")
  ) 

## 0.35
data_035 <- rejections_cap %>% filter(d == "0.35") #%>%
arms_035 <- arms_cap %>% filter(d == "0.35")
p_035 <-  
  ggplot() + 
  geom_point(data=data_035, aes(x = rand_type, y = rpp,
                                group = rand_type
  ), color= "red", size = 2) +
  geom_point(data=arms_035, aes(x = rand_type, y = rpp,
                                group = rand_type, 
                                color = rand_type
  ), color="blue", size = 2) +
  theme_bw() +
  ggtitle("d=0.35") + theme(plot.title=element_text(hjust=0.5))+ 
  geom_point(data=pow035, aes(x = rand_type, y = percentage/20), color="lightblue") +
  geom_line(data=pow035, aes(x = rand_type, y = percentage/20), color="lightblue") +
  scale_x_discrete(labels=c("rcts" = "sequential \n rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block_sqrt_cap"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +
  labs(x="Allocation method") +
  scale_y_continuous(
    name = "Arms per 1000 patients",
    limits = c(0,5),
    sec.axis = sec_axis(~.*20, name="Power in %")
  ) 

## 0.2
data_02 <- rejections_cap %>% filter(d == "0.2") #%>%
arms_02 <- arms_cap %>% filter(d == "0.2")
p_02 <-  
  ggplot() + 
  geom_point(data=data_02, aes(x = rand_type, y = rpp,
                               group = rand_type
  ), color= "red", size = 2) +
  geom_point(data=arms_02, aes(x = rand_type, y = rpp,
                               group = rand_type, 
                               color = rand_type
  ), color="blue", size = 2) +
  theme_bw() +
  ggtitle("d=0.2") + theme(plot.title=element_text(hjust=0.5))+ 
  geom_point(data=pow02, aes(x = rand_type, y = percentage/20), color="lightblue") +
  geom_line(data=pow02, aes(x = rand_type, y = percentage/20), color="lightblue") +
  scale_x_discrete(labels=c("rcts" = "sequential \n rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block_sqrt_cap"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +
  labs(x="Allocation method") +
  scale_y_continuous(
    name = "Arms per 1000 patients",
    limits = c(0,5),
    sec.axis = sec_axis(~.*20, name="Power in %")
  ) 

## 0
data_0 <- rejections_cap %>% filter(d == "0") #%>%
arms_0 <- arms_cap %>% filter(d == "0")
p_0 <-  
  ggplot() + 
  geom_point(data=data_0, aes(x = rand_type, y = rpp,
                              group = rand_type
  ), color= "red", size = 2) +
  geom_point(data=arms_0, aes(x = rand_type, y = rpp,
                              group = rand_type, 
                              color = rand_type
  ), color="blue", size = 2) +
  theme_bw() +
  ggtitle("d=0") + theme(plot.title=element_text(hjust=0.5))+ 
  geom_point(data=pow0, aes(x = rand_type, y = percentage/20), color="lightblue") +
  geom_line(data=pow0, aes(x = rand_type, y = percentage/20), color="lightblue") +
  scale_x_discrete(labels=c("rcts" = "sequential \n rcts", "block_1"="balanced", "block_k"="k", "block_sqrt"="sqrt(k)", "block_sqrt_cap"="sqrt(k)\n with cap", "full"="sqrt(k) with cap,\n no block")) +
  labs(x="Allocation method") +
  scale_y_continuous(
    name = "Arms per 1000 patients",
    limits = c(0,5),
    sec.axis = sec_axis(~.*20, name="Power in %")
  ) 

#arrange the 4 plots together
p_rejectRates_caps <- ggarrange(p_0, p_02, p_035, p_05,
                                ncol = 2, nrow = 2,
                                #legend = "none",
                                common.legend = TRUE,
                                legend = "bottom" 
                                #legend.grob = 
)
annotate_figure(p_rejectRates_caps,
                top = text_grob("Arms and rejections per 1000 patients for different control caps",
                                #color = "red", 
                                #face = "bold", 
                                size = 14)
)
ggsave("Rejection_rates_arms_power.png", device = "png", width=9, height=7)

