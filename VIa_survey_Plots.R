######################################################################
# Scripts plot extracted NASC values and further nice stuff
####################################################################

# required libraries for Area and NASC plots
library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)
library(mapdata)
library(rgdal)
library(broom)
library(marmap)


# set directory for results of previous script and read in merged NASC
setwd(pathOut)
NASC_HER <- read.csv("D:/ICES/ICES WGIPS/2019/2018 HERAS Post Cruise/HERAS2018_HER_5nmi.csv")
NASC_SPR <- read.csv("D:/ICES/ICES WGIPS/2019/2018 HERAS Post Cruise/HERAS2018_SPR_5nmi.csv")
TRANSECTS <- read.csv("D:/ICES/ICES WGIPS/2019/2018 HERAS Post Cruise/mergedNASCS9_HER2018.csv")

# rename SA with HER (and SPR respectively)
names(NASC_HER)[names(NASC_HER)=="SA"] <- "HER"
names(NASC_SPR)[names(NASC_SPR)=="SA"] <- "SPR"

#this changes Vessel Codes to Country Names
revalue(NASC_HER$CRUISE, c("06SL750" = "GER", "0918S" = "SCO", "26D4201806" = "DK", "45CE2018WESPAS_2" = "IE", "NLHERAS2018" = "NL")) -> NASC_HER$Cruise
revalue(NASC_SPR$Cruise, c("06SL750" = "GER", "0918S" = "SCO", "26D4201806" = "DK", "45CE2018WESPAS_2" = "IE", "NLHERAS2018" = "NL")) -> NASC_SPR$Cruise

#remove logs from outside the survey area/intertransects
NASC_HER <- NASC_HER %>% 
  select(Cruise, LOG, ACLON, ACLAT, HER, Transect) %>% 
  filter(!is.na(Transect)) # NOTE: This will remove all transects west of 6° W...

NASC_SPR <- NASC_SPR %>% 
  select(Cruise, LOG, ACLON, ACLAT, SPR, Transect) %>% 
  filter(!is.na(Transect)) 


# read in Strata-Shapefile
HERAS_Strata <-readOGR("D:/ICES/ICES HERAS/Mapstuff/HERAS_Strata", "HERAS2017_StoX_polygons") #set to wd where you stored your shp-files
MSAS_Strata <-readOGR("D:/ICES/ICES HERAS/Mapstuff/HERAS_Strata", "MSHAS2017_polygon") #set to wd where you stored your shp-files

#convert shapefile into dataframe to be used by ggplot2
HERAS_Strata_df <- tidy(HERAS_Strata)
MSHAS_Strata_df <- tidy(MSAS_Strata)

# split dataset

#Remove zero values (empty intervals) from HER and SPR NASC dataset (enhances visual style for plotting empty EDSUs later)
NASC_HERZRM <- filter(NASC_HER, HER>0)
NASC_SPRZRM <- filter(NASC_SPR, SPR>0)

#Identify zero values (empty intervals) from HER and SPR NASC dataset
NASC_HER0 <- filter(NASC_HER, HER==0)
NASC_SPR0 <- filter(NASC_SPR, SPR ==0)

# Area Map
Area <- map_data("worldHires", region =c("Norway", "Germany", "Denmark", "Netherlands", "Belgium", "Ireland", "Sweden", "UK"))

# AREA AND TRANSECT LINES PLOT

# this plots the position of EACH log distance sampled per participant
ggplot(NASC_HER, aes(ACLON, ACLAT)) +
  theme_bw()+
  geom_polygon(data=Area, aes(long, lat, group=group))+
  geom_point(aes(colour=Country))+
  geom_polygon(data=HERAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red", fill=NA)+
  geom_polygon(data=MSHAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red", fill=NA)+
  coord_quickmap(xlim=c(-12.0,12.5), ylim=c(51.5,62))+
  scale_x_continuous(breaks=seq(-12,12,2))+
  scale_y_continuous(breaks=seq(51,62,1), sec.axis=dup_axis(name=NULL, labels=NULL))+
  labs(x = "Longitude °E", 
       y = "Latitude °N")+
  theme(legend.justification=c(0,1), legend.position=c(0,1))+
  theme(legend.background = element_rect(size=0.3, linetype="solid",colour ="black"))+
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=14, color="black", face="bold"),
         legend.title = element_text(size=14, face="bold"),
         legend.text = element_text(size=12, face="bold"))

# this plots intervals (5nmi) and strata from HERAS 2018 with colour codes for transect spacing
# per stratum

HERAS_Strata <- read.csv("D:/ICES/ICES WGIPS/2019/2018 HERAS Post Cruise/HERAS_Strata2018.csv")
MSHAS_Strata <- read.csv("D:/ICES/ICES WGIPS/2019/2018 HERAS Post Cruise/MSHAS_Strata2018.csv")

SpacingColors <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(NASC_HER, aes(ACLON, ACLAT)) +
  theme_bw()+
  geom_polygon(data=Area, aes(long, lat, group=group))+
  geom_polygon(data=HERAS_Strata, aes(x = long, y = lat, group = group, fill=SpacingLevel), colour="black")+
  geom_polygon(data=MSHAS_Strata, aes(x = long, y = lat, group = group, fill=SpacingLevel), colour="black")+
  scale_fill_manual(values =SpacingColors, 
                    name="Transect\nSpacing\n(nmi)", 
                    labels=c("10", "13","15","17.5","23","25","30","ZigZag"))+
  geom_point(color="black", size=0.01)+
  coord_quickmap(xlim=c(-12.0,12.5), ylim=c(51.5,62))+
  scale_x_continuous(breaks=seq(-12,12,2))+
  scale_y_continuous(breaks=seq(51,62,1), sec.axis=dup_axis(name=NULL, labels=NULL))+
  labs(x = "Longitude °E", 
       y = "Latitude °N")+
  theme(legend.justification=c(0,1), legend.position=c(0,1))+
  theme(legend.background = element_rect(size=0.3, linetype="solid",colour ="black"))+
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=14, color="black", face="bold"),
         legend.title = element_text(size=14, face="bold"),
         legend.text = element_text(size=12, face="bold"))



# NASC BUBBLE PLOTS

# Herring
ggplot(NASC_HERZRM, aes(ACLON, ACLAT)) +
  theme_bw()+
  geom_polygon(data=Area, aes(long, lat, group=group))+
  geom_point(data=NASC_HER0, aes(ACLON, ACLAT), size=0.1, shape=3, col="darkgrey")+
  geom_point(aes(size = HER),alpha = 0.5, colour = "darkgreen")+
  scale_size_area(max_size = 20, breaks = c(500, 1000, 2500, 5000, 10000), name=bquote('NASC'~(m^2~nm^-2)))+
  geom_path(data=HERAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red")+
  geom_path(data=MSHAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red")+
  coord_quickmap(xlim=c(-12.0,12.5), ylim=c(51.5,62))+
  scale_x_continuous(breaks=seq(-12,12,2))+
  scale_y_continuous(breaks=seq(51,62,1), sec.axis=dup_axis(name=NULL, labels=NULL))+
  labs(
    #title = "HERAS Herring mean NASC per 5 nmi EDSU", 
    x = "Longitude °E", 
    y = "Latitude °N")+
  theme(legend.justification=c(0,1), legend.position=c(0,1))+
  theme(legend.background = element_rect(size=0.3, linetype="solid",colour ="black"))+
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=14, color="black", face="bold"),
         legend.title = element_text(size=14, face="bold"),
         legend.text = element_text(size=12, face="bold"))

# Sprat
ggplot(NASC_SPRZRM, aes(ACLON, ACLAT)) +
  theme_bw()+
  geom_polygon(data=Area, aes(long, lat, group=group))+
  geom_point(data=NASC_SPR0, aes(ACLON, ACLAT), size=0.1, shape=3, col="darkgrey")+
  geom_point(aes(size = SPR),alpha = 0.5, colour = "blue")+
  scale_size_area(max_size = 20, breaks = c(100, 500, 1000, 5000, 10000), name=bquote('NASC'~(m^2~nm^-2)))+
  geom_path(data=HERAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red")+
  coord_quickmap(xlim=c(-7.5,12.5), ylim=c(51.5,62))+
  scale_x_continuous(breaks=seq(-6,12,2))+
  scale_y_continuous(breaks=seq(51,62,1), sec.axis=dup_axis(name=NULL, labels=NULL))+
  labs(
    #title = "HERAS Sprat mean NASC per 5 nmi EDSU", 
    x = "Longitude °E", 
    y = "Latitude °N") +
  theme(legend.justification=c(0,1), legend.position=c(0,1))+
  theme(legend.background = element_rect(size=0.3, linetype="solid",colour ="black"))+
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=14, color="black", face="bold"),
         legend.title = element_text(size=14, face="bold"),
         legend.text = element_text(size=12, face="bold"))


### Abundance of Mature and Immature Herring as BUBBLE PLOTS (NASC as proxy)

#read data
MAT_HER <- read.csv("D:/ICES/ICES WGIPS/2019/2018 HERAS Post Cruise/HERAS2018_HER_Maturity.csv")

# split dataset
#Select only Mature and Immature herring from the dataset (enhances visual style for plotting empty EDSUs later)
MAT_HERZRM <- filter(MAT_HER, MAT>0)
IMMAT_HERZRM <- filter(MAT_HER, IMM>0)

#Identify zero values for both MAT and IMMAT Herring
MAT0 <- filter(MAT_HER, MAT==0)
IMMAT0 <- filter(MAT_HER, IMM ==0)

# Abundance BUBBLE PLOTS

# MATURE Herring
ggplot(MAT_HERZRM, aes(ACLON, ACLAT)) +
  theme_bw()+
  geom_polygon(data=Area, aes(long, lat, group=group))+
  geom_point(data=MAT0, aes(ACLON, ACLAT), size=0.1, shape=3, col="darkgrey")+
  geom_point(aes(size = MAT),alpha = 0.5, colour = "darkred")+
  scale_size_area(max_size = 15, breaks = c(25, 50, 100, 250, 500), name=bquote('Number'~(millions~nm^-2)))+
  geom_path(data=HERAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red")+
  geom_path(data=MSHAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red")+
  coord_quickmap(xlim=c(-12.0,12.5), ylim=c(51.5,62))+
  scale_x_continuous(breaks=seq(-12,12,2))+
  scale_y_continuous(breaks=seq(51,62,1), sec.axis=dup_axis(name=NULL, labels=NULL))+
  labs(
    #title = "HERAS Herring mean NASC per 5 nmi EDSU", 
    x = "Longitude °E", 
    y = "Latitude °N")+
  theme(legend.justification=c(0,1), legend.position=c(0,1))+
  theme(legend.background = element_rect(size=0.3, linetype="solid",colour ="black"))+
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=14, color="black", face="bold"),
         legend.title = element_text(size=14, face="bold"),
         legend.text = element_text(size=12, face="bold"))

# IMMATURE Herring
ggplot(IMMAT_HERZRM, aes(ACLON, ACLAT)) +
  theme_bw()+
  geom_polygon(data=Area, aes(long, lat, group=group))+
  geom_point(data=IMMAT0, aes(ACLON, ACLAT), size=0.1, shape=3, col="darkgrey")+
  geom_point(aes(size = IMM),alpha = 0.5, colour = "cyan4")+
  scale_size_area(max_size = 15, breaks = c(25, 50, 100, 250, 500), name=bquote('Number'~(millions~nm^-2)))+
  geom_path(data=HERAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red")+
  geom_path(data=MSHAS_Strata_df, aes(x = long, y = lat, group = group), colour= "red")+
  coord_quickmap(xlim=c(-12.0,12.5), ylim=c(51.5,62))+
  scale_x_continuous(breaks=seq(-12,12,2))+
  scale_y_continuous(breaks=seq(51,62,1), sec.axis=dup_axis(name=NULL, labels=NULL))+
  labs(
    #title = "HERAS Herring mean NASC per 5 nmi EDSU", 
    x = "Longitude °E", 
    y = "Latitude °N")+
  theme(legend.justification=c(0,1), legend.position=c(0,1))+
  theme(legend.background = element_rect(size=0.3, linetype="solid",colour ="black"))+
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=14, color="black", face="bold"),
         legend.title = element_text(size=14, face="bold"),
         legend.text = element_text(size=12, face="bold"))

### ABUNDANCE AT AGE PER YEAR PLOTS FOR REPORT

# required libraries for AbundanceAge/Year plots
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

#read in data from different stocks
# format: Excel.csv (for now): Columns: Year, Age, Mio
NSAS <- read.csv("D:/ICES/ICES WGIPS/2019/2018 HERAS Post Cruise/AbundanceIndices/NSAS_AbundanceAge.csv")
WBSS <- read.csv("D:/ICES/ICES WGIPS/2019/2018 HERAS Post Cruise/AbundanceIndices/WBSS_AbundanceAge.csv")
WSAS <- read.csv("D:/ICES/ICES WGIPS/2019/2018 HERAS Post Cruise/AbundanceIndices/WSAS_AbundanceAge.csv")
MS <- read.csv("D:/ICES/ICES WGIPS/2019/2018 HERAS Post Cruise/AbundanceIndices/MS_AbundanceAge.csv")
SPR4 <- read.csv("D:/ICES/ICES WGIPS/2019/2018 HERAS Post Cruise/AbundanceIndices/SPR4_AbundanceAge.csv")
SPR3 <- read.csv("D:/ICES/ICES WGIPS/2019/2018 HERAS Post Cruise/AbundanceIndices/SPR3_AbundanceAge.csv")


# adding a year class to the tables
NSASa <- mutate(NSAS, yc =Year-Age)
WBSSa <- mutate(WBSS, yc =Year-Age)
WSASa <- mutate(WSAS, yc =Year-Age)
MSa <- mutate(MS, yc =Year-Age)
SPR4a <- mutate(SPR4, yc =Year-Age)
SPR3a <- mutate(SPR3, yc =Year-Age)

NSASAgeClass <- c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7", "8" = "8", "9" = "9+") 
WBSSAgeClass <- c("1" = "1", "2" = "2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7", "8" = "8+")
SPRAgeClass <- c("0" = "0", "1" = "1", "2" = "2", "3" = "3+")


# from here on, the codes plot the Abundance per Year class and year for different stocks

### NSAS
nNSAS <- length(unique(NSASa$yc))
PAIRED <- rep(brewer.pal(12,"Paired"),100)

ggplot(NSASa,aes(Year,Mio,fill=factor(yc))) +
  theme_bw() +
  geom_bar(stat="identity") + 
  facet_grid(Age ~ .,scale="free_y", labeller = labeller(Age = NSASAgeClass)) +
  scale_fill_manual(values=PAIRED[1:nNSAS])  + 
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(1986, 2018, 2)) +
  # scale_y_continuous(breaks=seq(0, 25000, 5000)) +
  labs(
    #title = "NSAS Survey indices by age and year class", 
    x = NULL, 
    y = "n (Millions)") +
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=12, color="black", face="bold"),
         axis.ticks = element_line(size=1),
         axis.ticks.length=unit(.25, "cm"),
         strip.text.y = element_text(size = 12, colour = "black", face="bold",  angle=0),
         panel.grid = element_blank())

### WBSS
nWBSS <- length(unique(NSASa$yc))
PAIRED <- rep(brewer.pal(12,"Paired"),100)

ggplot(WBSSa,aes(Year,Mio,fill=factor(yc))) +
  theme_bw() +
  geom_bar(stat="identity") + 
  facet_grid(Age ~ .,scale="free_y", labeller = labeller(Age = WBSSAgeClass)) +
  scale_fill_manual(values=PAIRED[1:nNSAS])  + 
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(1992, 2018, 2)) +
  # scale_y_continuous(breaks=seq(0, 25000, 5000)) +
  labs(
    #title = "WBSSH: HERAS survey indices by age and year class", 
    x = NULL, 
    y = "n (Millions)") +
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=12, color="black", face="bold"),
         axis.ticks = element_line(size=1),
         axis.ticks.length=unit(.25, "cm"),
         strip.text.y = element_text(size = 12, colour = "black", face="bold",  angle=0),
         panel.grid = element_blank())

### WOS
nWSAS <- length(unique(WSASa$yc))
PAIRED <- rep(brewer.pal(12,"Paired"),100)

ggplot(WSASa,aes(Year,Mio,fill=factor(yc))) +
  theme_bw() +
  geom_bar(stat="identity") + 
  facet_grid(Age ~ .,scale="free_y", labeller = labeller(Age = NSASAgeClass)) +
  scale_fill_manual(values=PAIRED[1:nWSAS])  + 
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(1994, 2018, 2)) +
  # scale_y_continuous(breaks=seq(0, 25000, 5000)) +
  labs(
    #title = "WBSSH: HERAS survey indices by age and year class", 
    x = NULL, 
    y = "n (Millions)") +
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=12, color="black", face="bold"),
         axis.ticks = element_line(size=1),
         axis.ticks.length=unit(.25, "cm"),
         strip.text.y = element_text(size = 12, colour = "black", face="bold",  angle=0),
         panel.grid = element_blank())
# scale_y_continuous(NULL,NULL)

### MS
nMS <- length(unique(MSa$yc))
PAIRED <- rep(brewer.pal(12,"Paired"),100)

ggplot(MSa,aes(Year,Mio,fill=factor(yc))) +
  theme_bw() +
  geom_bar(stat="identity") + 
  facet_grid(Age ~ .,scale="free_y", labeller = labeller(Age = NSASAgeClass)) +
  scale_fill_manual(values=PAIRED[1:nMS])  + 
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(2008, 2018, 1)) +
  # scale_y_continuous(breaks=seq(0, 25000, 5000)) +
  labs(
    #title = "WBSSH: HERAS survey indices by age and year class", 
    x = NULL, 
    y = "n (Millions)") +
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=12, color="black", face="bold"),
         axis.ticks = element_line(size=1),
         axis.ticks.length=unit(.25, "cm"),
         strip.text.y = element_text(size = 12, colour = "black", face="bold",  angle=0),
         panel.grid = element_blank())
# scale_y_continuous(NULL,NULL)

### SPR4
nSPR4 <- length(unique(SPR4a$yc))
PAIRED <- rep(brewer.pal(12,"Paired"),100)

ggplot(SPR4a,aes(Year,Mio,fill=factor(yc))) +
  theme_bw() +
  geom_bar(stat="identity") + 
  facet_grid(Age ~ .,scale="free_y", labeller = labeller(Age = SPRAgeClass)) +
  scale_fill_manual(values=PAIRED[1:nSPR4])  + 
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(2004, 2018, 1)) +
  # scale_y_continuous(breaks=seq(0,70000, 20000)) +
  labs(
    #title = "WBSSH: HERAS survey indices by age and year class", 
    x = NULL, 
    y = "n (Millions)") +
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=12, color="black", face="bold"),
         axis.ticks = element_line(size=1),
         axis.ticks.length=unit(.25, "cm"),
         strip.text.y = element_text(size = 12, colour = "black", face="bold",  angle=0),
         panel.grid = element_blank())
# scale_y_continuous(NULL,NULL)

### SPR3a
nSPR3 <- length(unique(SPR3a$yc))
PAIRED <- rep(brewer.pal(12,"Paired"),100)

ggplot(SPR3a,aes(Year,Mio,fill=factor(yc))) +
  theme_bw() +
  geom_bar(stat="identity") + 
  facet_grid(Age ~ .,scale="free_y", labeller = labeller(Age = SPRAgeClass)) +
  scale_fill_manual(values=PAIRED[1:nSPR3])  + 
  theme(legend.position = "none") +
  scale_x_continuous(breaks=seq(2006, 2018, 1)) +
  #scale_y_continuous(breaks=seq(0, 6000, 1000)) +
  labs(
    #title = "WBSSH: HERAS survey indices by age and year class", 
    x = NULL, 
    y = "n (Millions)") +
  theme (axis.title = element_text(size=16, face="bold"),
         axis.text = element_text(size=12, color="black", face="bold"),
         axis.ticks = element_line(size=1),
         axis.ticks.length=unit(.25, "cm"),
         strip.text.y = element_text(size = 12, colour = "black", face="bold",  angle=0),
         panel.grid = element_blank())
# scale_y_continuous(NULL,NULL)
