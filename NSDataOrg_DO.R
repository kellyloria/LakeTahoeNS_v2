## ---------------------------
## Data aggregation of all miniDOT data from the Near shore of Lake Tahoe
##
## Author: Kelly A. Loria
## Date Created: 2022-01-31
## Email: kelly.loria@nevada.unr.edu

if (dir.exists('/Users/kellyloria/Documents/UNR/LakeTahoeNearShore')){
  inputDir<- '/Users/kellyloria/Documents/UNR/LakeTahoeNearShore/'
  outputDir<- '/Users/kellyloria/Documents/UNR/LakeTahoeNearShore/Figures' 
}

## ---------------------------
#Load packages
library(tidyverse)
library(ggplot2)
library(cowplot)
library(lubridate)
library(reshape2)
library(scales)
library(gridExtra)

## ---------------------------
## Read in Data:
  # Blackwood nearshore 1: BWNS1
getwd()

BWNS1 <- read.delim("/Users/kellyloria/Documents/UNR/LakeTahoeNearShore/RawData/DOdat/Blackwood/NS1/20211014/7450-195441/Cat.TXT",
                    sep = ',',
                    skip=7)[, -c(1:2)]

head(BWNS1)

colnames(BWNS1) <- c("PCT","V","Temp","DO","DOT.sat","Q")
BWNS1$serial <- c(195441)
BWNS1$site <- c("BWNS1")
str(BWNS1) # all values as chr


# correct time
BWNS1$PCT <- as.POSIXct(BWNS1$PCT, origin="1970-01-01")
head(BWNS1)

# limit time for deployment time cutoff first and last days
BWNS1 <- subset(BWNS1, 
                PCT > "2021-06-11 00:00:00" & 
                  PCT < "2021-09-30 00:00:00",
                Q>0.7) # sensor quality minimum threshold

# round to nearest 5 min
BWNS1$timestamp <- as.POSIXct(
  round_date(as.POSIXct(
    BWNS1$PCT, format="%Y-%m-%d %H:%M:%OS"), 
    hour, unit="5 minutes"))

## Visualize
plot_grid(
  ggplot(BWNS1, aes(PCT, DO)) + geom_point(),
  ggplot(BWNS1, aes(PCT, Temp)) + geom_point(),
  ggplot(BWNS1, aes(PCT, Q)) + geom_point(),
  ncol=1, align="hv")

## ---------------------------
## Read in Data:
# Blackwood nearshore 2: BWNS2
BWNS2 <- read.delim("/Users/kellyloria/Documents/UNR/LakeTahoeNearShore/RawData/DOdat/Blackwood/NS2/20211014/7450-336792/Cat.TXT",
                    sep = ',',
                    skip=7)[, -c(1:2)]
head(BWNS2)

colnames(BWNS2) <- c("PCT","V","Temp","DO","DOT.sat","Q")
BWNS2$serial <- c(336792)
BWNS2$site <- c("BWNS2")
str(BWNS2) # all values as chr


# correct time
BWNS2$PCT <- as.POSIXct(BWNS2$PCT, origin="1970-01-01")
head(BWNS2)

# limit time for deployment time cutoff first and last days
BWNS2 <- subset(BWNS2, 
                PCT > "2021-06-11 00:00:00" & 
                  PCT < "2021-09-30 00:00:00",
                Q>0.7) # sensor quality minimum threshold

# round to nearest 5 min
BWNS2$timestamp <- as.POSIXct(
  round_date(as.POSIXct(
    BWNS2$PCT, format="%Y-%m-%d %H:%M:%OS"), 
    hour, unit="5 minutes"))

## Visualize
plot_grid(
  ggplot(BWNS2, aes(PCT, DO)) + geom_point(),
  ggplot(BWNS2, aes(PCT, Temp)) + geom_point(),
  ggplot(BWNS2, aes(PCT, Q)) + geom_point(),
  ncol=1, align="hv")


## ---------------------------
## Read in Data:
# Blackwood nearshore 3: BWNS3
BWNS3 <- read.delim("/Users/kellyloria/Documents/UNR/LakeTahoeNearShore/RawData/DOdat/Blackwood/NS3/20211014/7450-162475/Cat.TXT",
                    sep = ',',
                    skip=7)[, -c(1:2)]
head(BWNS3)

colnames(BWNS3) <- c("PCT","V","Temp","DO","DOT.sat","Q")
BWNS3$serial <- c(162475)
BWNS3$site <- c("BWNS3")
str(BWNS3) # all values as chr


# correct time
BWNS3$PCT <- as.POSIXct(BWNS3$PCT, origin="1970-01-01")
head(BWNS3)

# limit time for deployment time cutoff first and last days
BWNS3 <- subset(BWNS3, 
                PCT > "2021-06-11 00:00:00" & 
                  PCT < "2021-09-30 00:00:00",
                Q>0.7) # sensor quality minimum threshold

# round to nearest 5 min
BWNS3$timestamp <- as.POSIXct(
  round_date(as.POSIXct(
    BWNS3$PCT, format="%Y-%m-%d %H:%M:%OS"), 
    hour, unit="5 minutes"))

## Visualize
plot_grid(
  ggplot(BWNS3, aes(PCT, DO)) + geom_point(),
  ggplot(BWNS3, aes(PCT, Temp)) + geom_point(),
  ggplot(BWNS3, aes(PCT, Q)) + geom_point(),
  ncol=1, align="hv")


## ---------------------------
## R bind all BW data:

BWNS <- rbind(BWNS1,BWNS2,BWNS3)

qplot(PCT, Temp, data = BWNS, geom="point", color = factor(site)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=c("#90BE6D", "#43AA8B", "#33637d")) +
  scale_x_datetime(labels = date_format("%b-%d"), 
                   breaks = date_breaks("168 hours")) +
  theme_classic()

#### ---------------------------
## Read in Data:
# Sunnyside nearshore 1: SSNS1
SSNS1 <- read.delim("/Users/kellyloria/Documents/UNR/LakeTahoeNearShore/RawData/DOdat/Sunnyside/20211014/7450-529728/Cat.TXT",
                    sep = ',',
                    skip=7)[, -c(1:2)]

head(SSNS1)

colnames(SSNS1) <- c("PCT","V","Temp","DO","DOT.sat","Q")
SSNS1$serial <- c(529728)
SSNS1$site <- c("SSNS1")
str(SSNS1) # all values as chr


# correct time
SSNS1$PCT <- as.POSIXct(SSNS1$PCT, origin="1970-01-01")
head(SSNS1)

# limit time for deployment time cutoff first and last days
SSNS1 <- subset(SSNS1, 
                PCT > "2021-08-26 00:00:00" & 
                  PCT < "2021-09-30 00:00:00",
                Q>0.7) # sensor quality minimum threshold

# round to nearest 5 min
SSNS1$timestamp <- as.POSIXct(
  round_date(as.POSIXct(
    SSNS1$PCT, format="%Y-%m-%d %H:%M:%OS"), 
    hour, unit="5 minutes"))

## Visualize
plot_grid(
  ggplot(SSNS1, aes(PCT, DO)) + geom_point(),
  ggplot(SSNS1, aes(PCT, Temp)) + geom_point(),
  ggplot(SSNS1, aes(PCT, Q)) + geom_point(),
  ncol=1, align="hv")
##
##
## ---------------------------

WNS <- rbind(BWNS1,BWNS2,BWNS3, SSNS1)

qplot(PCT, Temp, data = WNS, geom="point", color = factor(site)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=c("#90BE6D", "#43AA8B", "#33637d", "#775b8a")) +
  scale_x_datetime(labels = date_format("%b-%d"), 
                   breaks = date_breaks("168 hours")) +
  theme_classic()

## ---------------------------
## Read in Data:
# Glenbrook nearshore 1: GBNS1
GBNS1 <- read.delim("/Users/kellyloria/Documents/UNR/LakeTahoeNearShore/RawData/DOdat/Glenbrook/Nearshore1/7450-193411/Cat.TXT",
                    sep = ',',
                    skip=7)[, -c(1:2)]

head(GBNS1)

colnames(GBNS1) <- c("PCT","V","Temp","DO","DOT.sat","Q")
GBNS1$serial <- c(193411)
GBNS1$site <- c("GBNS1")
str(GBNS1) # all values as chr


# correct time
GBNS1$PCT <- as.POSIXct(GBNS1$PCT, origin="1970-01-01")
head(GBNS1)

# limit time for deployment time cutoff first and last days
GBNS1 <- subset(GBNS1, 
                PCT > "2021-06-11 00:00:00" & 
                  PCT < "2021-09-30 00:00:00",
                Q>0.7) # sensor quality minimum threshold

# round to nearest 5 min
GBNS1$timestamp <- as.POSIXct(
  round_date(as.POSIXct(
    GBNS1$PCT, format="%Y-%m-%d %H:%M:%OS"), 
    hour, unit="5 minutes"))

## Visualize
plot_grid(
  ggplot(GBNS1, aes(PCT, DO)) + geom_point(),
  ggplot(GBNS1, aes(PCT, Temp)) + geom_point(),
  ggplot(GBNS1, aes(PCT, Q)) + geom_point(),
  ncol=1, align="hv")


## ---------------------------
## Read in Data:
# Glenbrook nearshore 2: GBNS2
GBNS2 <- read.delim("/Users/kellyloria/Documents/UNR/LakeTahoeNearShore/RawData/DOdat/Glenbrook/Nearshore2/7450-265933/Cat.TXT",
                    sep = ',',
                    skip=7)[, -c(1:2)]

head(GBNS2)

colnames(GBNS2) <- c("PCT","V","Temp","DO","DOT.sat","Q")
GBNS2$serial <- c(265933)
GBNS2$site <- c("GBNS2")
str(GBNS2) # all values as chr


# correct time
GBNS2$PCT <- as.POSIXct(GBNS2$PCT, origin="1970-01-01")
head(GBNS2)

# limit time for deployment time cutoff first and last days
GBNS2 <- subset(GBNS2, 
                PCT > "2021-06-11 00:00:00" & 
                  PCT < "2021-09-30 00:00:00",
                Q>0.7) # sensor quality minimum threshold

# round to nearest 5 min
GBNS2$timestamp <- as.POSIXct(
  round_date(as.POSIXct(
    GBNS2$PCT, format="%Y-%m-%d %H:%M:%OS"), 
    hour, unit="5 minutes"))

## Visualize
plot_grid(
  ggplot(GBNS2, aes(PCT, DO)) + geom_point(),
  ggplot(GBNS2, aes(PCT, Temp)) + geom_point(),
  ggplot(GBNS2, aes(PCT, Q)) + geom_point(),
  ncol=1, align="hv")



## ---------------------------
## Read in Data:
# Glenbrook nearshore 3: GBNS3
GBNS3 <- read.delim("/Users/kellyloria/Documents/UNR/LakeTahoeNearShore/RawData/DOdat/Glenbrook/Nearshore3/7450-224208/Cat.TXT",
                    sep = ',',
                    skip=7)[, -c(1:2)]

head(GBNS3)

colnames(GBNS3) <- c("PCT","V","Temp","DO","DOT.sat","Q")
GBNS3$serial <- c(224208)
GBNS3$site <- c("GBNS3")
str(GBNS3) # all values as chr


# correct time
GBNS3$PCT <- as.POSIXct(GBNS3$PCT, origin="1970-01-01")
head(GBNS3)

# limit time for deployment time cutoff first and last days
GBNS3 <- subset(GBNS3, 
                PCT > "2021-06-11 00:00:00" & 
                  PCT < "2021-09-30 00:00:00",
                Q>0.7) # sensor quality minimum threshold

# round to nearest 5 min
GBNS3$timestamp <- as.POSIXct(
  round_date(as.POSIXct(
    GBNS3$PCT, format="%Y-%m-%d %H:%M:%OS"), 
    hour, unit="5 minutes"))

## Visualize
plot_grid(
  ggplot(GBNS3, aes(PCT, DO)) + geom_point(),
  ggplot(GBNS3, aes(PCT, Temp)) + geom_point(),
  ggplot(GBNS3, aes(PCT, Q)) + geom_point(),
  ncol=1, align="hv")


## ---------------------------
## R bind all BW data:

GBNS <- rbind(GBNS1,GBNS2,GBNS3)

GBNS_plot <- qplot(PCT, Temp, data = GBNS, geom="point", color = factor(site)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=alpha(c("#cfbd1d","#d6733a", "#b0492a"),0.5)) +
  scale_x_datetime(labels = date_format("%b-%d"), 
                   breaks = date_breaks("168 hours")) +
  theme_classic()

BWNS_plot <- qplot(PCT, Temp, data = WNS, geom="point", color = factor(site)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=alpha(c("#90BE6D", "#43AA8B", "#33637d", "#775b8a"),0.5)) +
  scale_x_datetime(labels = date_format("%b-%d"), 
                   breaks = date_breaks("168 hours")) +
  theme_classic()

NS_temp<- grid.arrange(GBNS_plot,
                       BWNS_plot,
                       nrow = 2,
                       top = "Near-shore water temperature")

#ggsave(paste0(outputDir,"/NS_temp.jpeg"), NS_temp, scale = 0.75, width =42, height = 25, units = c("cm"), dpi = 500)



GBNS_DOplot <- qplot(PCT, DO, data = GBNS, geom="point", color = factor(site)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=alpha(c("#cfbd1d","#d6733a", "#b0492a"),0.5)) +
  scale_x_datetime(labels = date_format("%b-%d"), 
                   breaks = date_breaks("168 hours")) +
  theme_classic()

BWNS_DOplot <- qplot(PCT, DO, data = WNS, geom="point", color = factor(site)) +
  theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0))+
  scale_color_manual(values=alpha(c("#90BE6D", "#43AA8B", "#33637d", "#775b8a"),0.5)) +
  scale_x_datetime(labels = date_format("%b-%d"), 
                   breaks = date_breaks("168 hours")) +
  theme_classic()

NS_DO<- grid.arrange(GBNS_DOplot,
                       BWNS_DOplot,
                       nrow = 2,
                       top = "Near-shore dissloved oxygen")

#ggsave(paste0(outputDir,"/NS_DO.jpeg"), NS_DO, scale = 0.75, width =42, height = 25, units = c("cm"), dpi = 500)

