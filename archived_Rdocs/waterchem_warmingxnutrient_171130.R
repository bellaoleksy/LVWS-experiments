library(dplyr)
library(ggplot2)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)
#To print all 4 onto one page 

setwd("/Users/isabellaoleksy/Desktop/Bella's Folder/Colorado State University/Research/Data/2017 data/Experiments/Warming X Nutrient Experiment/R")
expchem <- read.csv("waterchem_warmingxnutrient_171206.csv")

expchem <- as.tbl(expchem)
glimpse(expchem)

expchem <- expchem %>%
  mutate(changeDOC_mgL=initial_DOC_mgL-DOC_mgL,
         changeTDN_uM=initial_TDN_uM-TDN_uM,
         changePO4_uM=initial_PO4_uM-PO4_uM,
         changeNH4_mgL=initial_NH4_mgL-NH4_mgL)
glimpse(expchem)
  

# expchem_summary <- expchem %>%
#   group_by(tempxtrt, treatment, temperature) %>%
#   summarize(meanNO3=mean(NO3_mgL),
#             sdNO3=sd(NO3_mgL),
#             meanDOC=mean(DOC_mgL),
#             sdDOC=sd(DOC_mgL),
#             meanPO4=mean(PO4_mgL),
#             sdPO4=sd(PO4_mgL),
#             n()) %>%
#   arrange(temperature)
# expchem_summary
expchem_trim <- expchem %>%
  select(sample_id, temperature, treatment, tempxtrt, rep, chl_a_phyto, changeDOC_mgL, changeTDN_uM, changePO4_uM, changeNH4_mgL)
glimpse(expchem_trim)

expchem_changesummary <- expchem %>%
  group_by(tempxtrt, treatment, temperature) %>%
  summarize(meanchangeDOC_mgL=mean(changeDOC_mgL),
            sdchangeDOC_mgL=sd(changeDOC_mgL),
            meanchangeTDN_uM=mean(changeTDN_uM),
            sdchangeTDN_uM=sd(changeTDN_uM),
            meanchangePO4_uM=mean(changePO4_uM),
            sdchangePO4_uM=sd(changePO4_uM),
            meanchangeNH4_mgL=mean(changeNH4_mgL),
            sdchangeNH4_mgL=sd(changeNH4_mgL),
            meanphytochla=mean(chl_a_phyto),
            sdphytochla=sd(chl_a_phyto),
            n()) %>%
  arrange(temperature)
expchem_changesummary

#Specify temperature as factor
expchem_changesummary$temperature <- as.factor(expchem_changesummary$temperature)
expchem_trim$temperature <- as.factor(expchem_trim$temperature)

#Order the factors for graphing
expchem_changesummary$treatment <- factor(expchem_changesummary$treatment, levels = c("C", "N", "P", "NP"))


################

# NUTRIENT UPTAKE BAR PLOTS  ----------------------------------------------


###
# # Error bars represent standard deviation of the mean
#Now switching the plots so that the colors correspond to 
#treatments, not temperature
expcolors <- c("#40A339", "#3166A9", "#DE0019", "#82388E")

TDNchemplot <- ggplot(expchem_changesummary, aes(x=temperature, y=meanchangeTDN_uM, fill=treatment)) + theme_bw()+
   geom_bar(color="black",stat="identity", position="dodge") +
  scale_fill_manual(values=expcolors,
                    name="Nutrient\nTreatment",
                    labels=c("Control", "Nitrogen", "Phosphorus", "Both (N & P)"))+
  geom_errorbar(aes(ymin=meanchangeTDN_uM-sdchangeTDN_uM, ymax=meanchangeTDN_uM+sdchangeTDN_uM),
                width=.2,
                position=position_dodge(0.9))+
  coord_cartesian(ylim=c(0,70))+
  scale_y_continuous(breaks=seq(0,70,10))+
  theme(axis.text.x = element_text(colour = "black", size = 20), axis.title.y=element_text(size=22, vjust=1.2, face="bold"),
        axis.text.y = element_text(colour = "black", size = 20), axis.title.x=element_text(size=22, vjust=-0.1, face="bold"),
        legend.text = element_text(colour="black", size = 20), legend.title=element_text(size=22, face="bold"),
        panel.border = element_rect(size=1.0, fill=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  geom_hline(yintercept=66) +
  annotate("text",1,66,vjust=-1,label="  Starting [TDN] in N & N+P treatments")+
  geom_hline(yintercept=18, color="black", linetype="dashed")+
  annotate("text",0.8,18,vjust=-1,label="Background [TDN]")+
  labs(x="Temperature Treatment",y="Nitrogen uptake (uM)")
TDNchemplot


ggsave("TDNchemplot.png", width=30, height=16,units="cm")


# DOC uptake/release ------------------------------------------------------


DOCchemplot <- ggplot(expchem_changesummary, aes(x=temperature, y=meanchangeDOC_mgL, fill=treatment)) + theme_bw()+
  geom_bar(color="black",stat="identity", position="dodge") +
  scale_fill_manual(values=expcolors,
                    name="Nutrient\nTreatment",
                    labels=c("Control", "Nitrogen", "Phosphorus", "Both (N & P)"))+
  geom_errorbar(aes(ymin=meanchangeDOC_mgL-sdchangeDOC_mgL, ymax=meanchangeDOC_mgL+sdchangeDOC_mgL),
                width=.2,
                position=position_dodge(0.9))+
  theme(axis.text.x = element_text(colour = "black", size = 20), axis.title.y=element_text(size=22, vjust=1.2, face="bold"),
        axis.text.y = element_text(colour = "black", size = 20), axis.title.x=element_text(size=22, vjust=-0.1, face="bold"),
        legend.text = element_text(colour="black", size = 20), legend.title=element_text(size=22, face="bold"),
        panel.border = element_rect(size=1.0, fill=NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())+
  labs(x="Temperature Treatment", y="DOC exudation (mg/L)")
DOCchemplot
ggsave("DOCchemplot.png", width=20, height=20,units="cm")


# PO4 uptake/release ------------------------------------------------------


PO4chemplot <- ggplot(expchem_changesummary, aes(x=temperature, y=meanchangePO4_uM, fill=treatment)) + theme_bw()+
  geom_bar(color="black", stat="identity", position="dodge") +
  scale_fill_manual(values=expcolors,
                    name="Nutrient\nTreatment",
                    labels=c("Control", "Nitrogen", "Phosphorus", "Both (N & P)"))+
  geom_errorbar(aes(ymin=meanchangePO4_uM-sdchangePO4_uM, ymax=meanchangePO4_uM+sdchangePO4_uM),
                width=.2,
                position=position_dodge(0.9))+
theme(axis.text.x = element_text(colour = "black", size = 20), axis.title.y=element_text(size=22, vjust=1.2, face="bold"),
      axis.text.y = element_text(colour = "black", size = 20), axis.title.x=element_text(size=22, vjust=-0.1, face="bold"),
      legend.text = element_text(colour="black", size = 20), legend.title=element_text(size=22, face="bold"),
      panel.border = element_rect(size=1.0, fill=NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank())+
  geom_hline(yintercept=8.5) +
  annotate("text",1,8.5,vjust=-1,label="Starting [PO4] in P & N+P treatments")+
  geom_hline(yintercept=0.8, color="black", linetype="dashed")+
  annotate("text",0.8,0.8,vjust=-1,label="Background [PO4]")+
  labs(x="Temperature Treatment", y="PO4 uptake (+) or exudation (-) (uM)")
PO4chemplot
ggsave("PO4chemplot.png", width=20, height=20,units="cm")


# NH4 uptake/release ------------------------------------------------------


NH4chemplot <- ggplot(expchem_changesummary, aes(x=temperature, y=meanchangeNH4_mgL, fill=treatment)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=meanchangeNH4_mgL-sdchangeNH4_mgL, ymax=meanchangeNH4_mgL+sdchangeNH4_mgL),
                width=.2,
                position=position_dodge(0.9))
NH4chemplot
ggsave("NH4chemplot.png", width=20, height=20,units="cm")


# phyto biomass -----------------------------------------------------------


phytochlaplot <- ggplot(expchem_changesummary, aes(x=temperature, y=meanphytochla, fill=treatment)) + theme_bw()+
  geom_bar(color="black",stat="identity", position="dodge") +
  scale_fill_manual(values=expcolors,
                    name="Nutrient\nTreatment",
                    labels=c("Control", "Nitrogen", "Phosphorus", "Both (N & P)"))+
  geom_errorbar(aes(ymin=meanphytochla-sdphytochla, ymax=meanphytochla+sdphytochla),
                width=.2,
                position=position_dodge(0.9))+
   theme(axis.text.x = element_text(colour = "black", size = 20), axis.title.y=element_text(size=22, vjust=1.2, face="bold"),
         axis.text.y = element_text(colour = "black", size = 20), axis.title.x=element_text(size=22, vjust=-0.1, face="bold"),
         legend.text = element_text(colour="black", size = 20), legend.title=element_text(size=22, face="bold"),
         panel.border = element_rect(color="black", size=1.0, fill=NA),
         panel.background = element_rect(color="white"),
         panel.grid.minor = element_blank(),
         panel.grid.major = element_blank())+
  labs(x="Temperature Treatment",y="Algal biomass (chlorophyll, ug/L)")
phytochlaplot
ggsave("phytochlaplot.png", width=20, height=20,units="cm")


#Print all 4 onto one page

ggarrange(TDNchemplot, DOCchemplot, PO4chemplot,NH4chemplot, 
          labels = c("Total Dissolved Nitrogen", "Dissolved Organic Carbon", "Phosphate", "Ammonium"),
          heights = c(3,3),
          ncol = 2, nrow = 2, align="hv")
ggsave("change_waterchem_all.png", width=48, height=48,units="cm")



# CORRELATIONS? -----------------------------------------------------------
head(expchem)
#Plot change in TDN versus phytoplankton chlorophyll a 
ggplot(expchem, aes(x=chl_a_phyto, y=changeTDN_uM, color=treatment)) +
  geom_point()+
  expand_limits(x=0,y=0)+
  theme_bw()


correlations<-cor(expchem_changesummary[,4:14], use="pairwise.complete.obs")
correlations
glimpse(expchem)
cor(expchem$chl_a_phyto,expchem$initial_TDN_uM)
# [1] 0.575856
cor(expchem$chl_a_phyto,expchem$initial_PO4_uM)
# [1] 0.4841351

# BOX PLOTS - CHANGE IN TDN BY TRT AT EACH TEMP ---------------------------------------
####CHANGE IN TDN BY TREATMENT AT EACH TEMPERATURE
#####Box plots
glimpse(expchem)
expchem$temperature <- as.factor(expchem$temperature)


#Control Change in TDN by temperature
controlresp <- expchem %>%
  filter(treatment=="C")
ggplot(controlresp, aes(x=temperature, y=changeTDN_uM))+
  geom_boxplot() +
  ggtitle("CONTROL\nChange in TDN by Temperature") +
  expand_limits(y=0)

#+Nitrogen Change in TDN by temperature
nitrogenresp <- expchem %>%
  filter(treatment=="N")
ggplot(nitrogenresp, aes(x=temperature, y=changeTDN_uM))+
  geom_boxplot() +
  ggtitle("+NITROGEN\nChange in TDN by Temperature")+
  expand_limits(y=0)

#+Phosphorus Change in TDN by temperature
phosphorusresp <- expchem %>%
  filter(treatment=="P")
ggplot(phosphorusresp, aes(x=temperature, y=changeTDN_uM))+
  geom_boxplot() +
  ggtitle("+PHOSPHORUS\nChange in TDN by Temperature")+
  expand_limits(y=0) 

#+N&P Change in TDN by temperature
NPresp <- expchem %>%
  filter(treatment=="NP")
ggplot(NPresp, aes(x=temperature, y=changeTDN_uM))+
  geom_boxplot() +
  ggtitle("+N&P\nChange in TDN by Temperature")+
  expand_limits(y=0)


# BOX PLOTS - CHANGE IN NH4 BY TRT AT EACH TEMP ---------------------------------------

expchem$temperature <- as.factor(expchem$temperature)

#Control Change in NH4 by temperature
controlresp <- expchem %>%
  filter(treatment=="C")
ggplot(controlresp, aes(x=temperature, y=changeNH4_mgL))+
  geom_boxplot() +
  ggtitle("CONTROL\nChange in NH4 by Temperature")+
  expand_limits(y=0)
#+Nitrogen Change in NH4 by temperature
nitrogenresp <- expchem %>%
  filter(treatment=="N")
ggplot(nitrogenresp, aes(x=temperature, y=changeNH4_mgL))+
  geom_boxplot() +
  ggtitle("+NITROGEN\nChange in NH4 by Temperature")+ 
  expand_limits(y=0)
#+Phosphorus Change in NH4 by temperature
phosphorusresp <- expchem %>%
  filter(treatment=="P")
ggplot(phosphorusresp, aes(x=temperature, y=changeNH4_mgL))+
  geom_boxplot() +
  ggtitle("+PHOSPHORUS\nChange in NH4 by Temperature")+
  expand_limits(y=0)
#+N&P Change in NH4 by temperature
NPresp <- expchem %>%
  filter(treatment=="NP")
ggplot(NPresp, aes(x=temperature, y=changeNH4_mgL))+
  geom_boxplot() +
  ggtitle("+N&P\nChange in NH4 by Temperature")+
  expand_limits(y=0)

# BOX PLOTS - CHANGE IN PO4 BY TRT AT EACH TEMP ---------------------------------------

expchem$temperature <- as.factor(expchem$temperature)

#Control Change in PO4 by temperature
controlresp <- expchem %>%
  filter(treatment=="Control")
ggplot(controlresp, aes(x=temperature, y=changePO4_uM))+
  geom_boxplot() +
  ggtitle("CONTROL\nChange in PO4 by Temperature")+
  expand_limits(y=0)

#+Nitrogen Change in PO4 by temperature
nitrogenresp <- expchem %>%
  filter(treatment=="Nitrogen")
ggplot(nitrogenresp, aes(x=temperature, y=changePO4_uM))+
  geom_boxplot() +
  ggtitle("+NITROGEN\nChange in PO4 by Temperature")+ 
  expand_limits(y=0)

#+Phosphorus Change in PO4 by temperature
phosphorusresp <- expchem %>%
  filter(treatment=="Phosphorus")
ggplot(phosphorusresp, aes(x=temperature, y=changePO4_uM))+
  geom_boxplot() +
  ggtitle("+PHOSPHORUS\nChange in PO4 by Temperature")+
  expand_limits(y=0)

#+N&P Change in PO4 by temperature
NPresp <- expchem %>%
  filter(treatment=="N+P")
ggplot(NPresp, aes(x=temperature, y=changePO4_uM))+
  geom_boxplot() +
  ggtitle("+N&P\nChange in PO4 by Temperature")+
  expand_limits(y=0)




# BAR PLOTS OF INCUBATION DATA --------------------------------------------
incubation <- read.csv("incubationdata_test_171206.csv")



incubation <- as.tbl(incubation)
glimpse(incubation)

incubation_summary <- incubation %>%
  group_by(treatment, temperature) %>%
  summarize(meanNEP_ugL=mean(NEP_mgL)*1000,
            sdNEP_ugL=sd(NEP_mgL)*1000,
            meanER_ugL=mean(ER_mgL)*1000,
            sdcER_ugL=sd(ER_mgL)*1000,
            meanGPP_ugL=mean(GPP_mgL)*1000,
            sdGPP_ugL=sd(GPP_mgL)*1000,
            n()) %>%
  arrange(temperature)
incubation_summary

#Specify temperature as factor
incubation_summary$temperature <- as.factor(incubation_summary$temperature)


#Bar Plots 
NEP <- ggplot(incubation_summary, aes(x=temperature, y=meanNEP_ugL, fill=treatment)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=meanNEP_ugL-sdNEP_ugL, ymax=meanNEP_ugL+sdNEP_ugL),
                width=.2,
                position=position_dodge(0.9))
NEP
ggsave("NEPplot.png", width=20, height=20,units="cm")

ER <- ggplot(incubation_summary, aes(x=temperature, y=meanER_ugL, fill=treatment)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=meanER_ugL-sdcER_ugL, ymax=meanER_ugL+sdcER_ugL),
                width=.2,
                position=position_dodge(0.9))
ER
ggsave("ERplot.png", width=20, height=20,units="cm")

GPP <- ggplot(incubation_summary, aes(x=temperature, y=meanGPP_ugL, fill=treatment)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=meanGPP_ugL-sdGPP_ugL, ymax=meanGPP_ugL+sdGPP_ugL),
                width=.2,
                position=position_dodge(0.9))
GPP
ggsave("GPPplot.png", width=20, height=20,units="cm")

