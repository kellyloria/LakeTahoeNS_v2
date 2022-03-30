rm(list=ls())


# load packages
library(tidyverse)
library(rstan)
library(loo)
library(patchwork)
library(lubridate)
library(shinystan)
## source
source("stan_utility.R")

lake <- "BWNS1" 

year <- c(2021)
# stan settings
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
# read data in
data <- read_rdump(paste("./ModelInputs/BWNS1_2021_sonde_list.R",sep=""))

# set reference temperature
data$temp_ref <- 18.4
## add parameters that can be modified
data$sig_b0 <- 0.01 #pmax smoothing parameter
data$sig_r <- 0.01  #respiration smoothing parameter
data$sig_i0 <- 0.2  #light saturation smoothing parameter

## define model location
model <- "o2_model_inhibition.stan" #Steele 2 param inhibition
model_path <- paste0("./stan/",model)

# set specifications for stan
chains <- 3 # was 6
iter <-6000 #was 1000
warmup <- 3000 #was 1000
adapt_delta <- 0.99
max_treedepth <- 15
thin <- 1


sm <- stan_model(file = model_path)
stanfit <- sampling(sm, data = data, chains = chains, cores = chains, iter = iter, warmup = min(iter*0.5,warmup),
                    control = list(adapt_delta = adapt_delta, max_treedepth = max_treedepth), 
                    seed=194838,thin = thin,save_warmup=FALSE)


fit_summary <- summary(stanfit, probs=c(0.025,0.5,0.975))$summary %>% 
  {as_tibble(.) %>%
      mutate(var = rownames(summary(stanfit)$summary))}


check_n_eff(stanfit)
check_rhat(stanfit)
check_div(stanfit)
check_treedepth(stanfit,max_treedepth)
check_energy(stanfit)

# export path
output_path <- paste0("./ouput/")
# save model full output
saveRDS(stanfit, paste0(output_path,"/",lake,"_fit.rds"))

fit_clean <- fit_summary %>%
  rename(lower = '2.5%', middle = `50%`,upper = '97.5%')  %>%
  mutate(name = strsplit(var, "\\[|\\]|,") %>% map_chr(~.x[1]),
         index = strsplit(var, "\\[|\\]|,") %>% map_int(~as.integer(.x[2])),
         day = ifelse(name %in% c("GPP","ER","NEP","AIR","Flux","GPP_m2","ER_m2","NEP_m2","b0","r","i0","b"), 
                      index, 
                      data$map_days[index])) %>%
  select(name, index, day, middle,lower,upper)


# data
sonde_data <- read_csv(paste0("./ModelInputs/sonde_prep_BWNS1_2021.csv"))

out <- fit_clean %>%
  filter(name %in% c("GPP","ER","NEP")) %>%
  rename(unique_day = day) %>% 
  left_join(sonde_data %>% select(unique_day,yday,year) %>% distinct()) %>% 
  full_join(sonde_data %>% expand(year,yday,name=c("GPP","ER","NEP"))) %>% 
  mutate(middle = ifelse(name=="ER",-middle,middle),
         lower = ifelse(name=="ER",-lower,lower),
         upper = ifelse(name=="ER",-upper,upper),
         name = factor(name, levels=c("GPP","NEP","ER")))
out2 <- fit_clean %>%
  filter(name %in% c("GPP_m2","ER_m2","NEP_m2"))%>%
  rename(unique_day = day) %>% 
  left_join(sonde_data %>% select(unique_day,yday,year) %>% distinct()) %>% 
  full_join(sonde_data %>% expand(year,yday,name=c("GPP_m2","ER_m2","NEP_m2"))) %>% 
  mutate(middle = ifelse(name=="ER_m2",-middle,middle),
         lower = ifelse(name=="ER_m2",-lower,lower),
         upper = ifelse(name=="ER_m2",-upper,upper),
         name = factor(name, levels=c("GPP_m2","NEP_m2","ER_m2")))

out3 <- rbind(out,out2)

#==========
#==========  Export
#==========

# export
write_csv(out3, paste0(output_path,"/",lake,"_","_daily_full.csv"))
write_csv(fit_clean, paste0(output_path,"/",lake,"_","_summary_clean.csv"))


#plot primary parameters
p1 <- fit_clean %>%  
  filter(name=="b0" | name == "r" | name == "i0" ) %>% 
  rename(unique_day = index) %>% 
  left_join(sonde_data %>% select(unique_day,yday,year) %>% distinct()) %>%
  ggplot(aes(x=yday,y=middle,color=factor(year))) + 
  geom_point(size=0.5) +
  geom_line(alpha=0.5) +
  facet_wrap(vars(name),ncol=1,scales="free_y") +
  theme_bw() +
  labs(y="Mean Estimated Value",color="year",x="Day of Year")
p1

ggsave(plot = p1,filename = paste("./graphics/BWNS1fit_2021.png",sep=""),width=8,height=4.5,dpi=300)

#plot time series of estimates
p2 <- ggplot(data = out %>% drop_na(year),aes(yday, middle, color = name))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = name),
              linetype = 0, alpha = 0.2)+
  geom_line()+
  #geom_point(data = out %>% left_join(c14),aes(x=yday,y=(p80/12.011),color="C14")) +
  scale_color_manual(values = c("green","black","dodgerblue","firebrick")) +
  scale_fill_manual(values = c("dodgerblue","firebrick","black")) +
  theme_bw() +
  labs(y=expression(mmol~O[2]~m^-3~d^-1)) +
  facet_wrap(vars(year))
p2

ggsave(plot = p2,filename = paste("./graphics/BWNS1metab_2021.png",sep=""),width=8,height=4.5,dpi=300)
