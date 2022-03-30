
# load packages
library(tidyverse)
library(rstan)
library(loo)
library(patchwork)
library(lubridate)
library(shinystan)


outputdat = list.files(paste("./PlotData/",sep=""), full.names = T) %>%
  lapply(read_csv) %>%
  bind_rows()
# if(lake == "GBNS3") sonde <- sonde %>% drop_na(datetime)
# unique(sonde$year)



# select out all _m2 values
outputdat <- outputdat %>% filter(name %in% c("GPP", "ER", "NEP"))
unique(outputdat$name)
unique(outputdat$site)

p2 <- ggplot(data = outputdat %>% drop_na(year),aes(yday, middle, color = name))+
  geom_hline(yintercept = 0, size = 0.3, color = "gray50")+
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = name),
              linetype = 0, alpha = 0.2)+
  geom_line()+
  #geom_point(data = out %>% left_join(c14),aes(x=yday,y=(p80/12.011),color="C14")) +
  scale_color_manual(values = c("dodgerblue", "green","black")) +
  scale_fill_manual(values = c("#026db0","#358f15","#4d4d4d")) +
  theme_bw() +
  labs(y=expression(mmol~O[2]~m^-3~d^-1)) +
  facet_wrap(("site"))
p2


ggsave(plot = p2,filename = paste("./graphics/2021NSmetabolism.png",sep=""),width=10,height=6,dpi=300)
