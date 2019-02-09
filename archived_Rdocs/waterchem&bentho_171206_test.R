library(dplyr)
library(ggplot2)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)
#To print all 4 onto one page 

setwd("/Users/isabellaoleksy/Desktop/Bella's Folder/Colorado State University/Research/Data/2017 data/Experiments/Warming X Nutrient Experiment/R")
benthic.data <- read.csv("waterchem&bentho_171206_test.csv")
benthic.data <- as.tbl(benthic.data)

benthic.data <- benthic.data %>%
  mutate(         pre_perc_green = (pre_BG/pre_chla_bentho)*100,
                  pre_perc_diatom = (pre_BD/pre_chla_bentho)*100,
                  pre_perc_cyano = (pre_BC/pre_chla_bentho)*100,
                  post_perc_green = (post_BG/post_chla_bentho)*100,
                  post_perc_diatom = (post_BD/post_chla_bentho)*100,
                  post_perc_cyano = (post_BC/post_chla_bentho)*100,
                  diff_perc_green = post_BG - pre_BG,
                  diff_perc_diatom = post_BD - pre_BD,
                  diff_perc_cyano = post_BC - pre_BC
                  )

benthictest <- benthic.data %>%
  mutate(changeDOC_mgL=initial_DOC_mgL-DOC_mgL,
         changeTDN_uM=initial_TDN_uM-TDN_uM,
         changePO4_uM=initial_PO4_uM-PO4_uM,
         changeNH4_mgL=initial_NH4_mgL-NH4_mgL,
         perc_green = (green_bentho/chla_bentho)*100,
         perc_diatom = (diatom_bentho/chla_bentho)*100,
         perc_cyano = (cyano_bentho/chla_bentho)*100) %>% 
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
            meanbenthicchla=mean(chla_bentho),
            sdbenthicchla=sd(chla_bentho),
            meangreen=mean(green_bentho),
            sdgreen=sd(green_bentho),
            meandiatom=mean(diatom_bentho),
            sddiatom=sd(diatom_bentho),
            meancyano=mean(cyano_bentho),
            sdcyano=sd(cyano_bentho),
            meanpercgreen=mean(perc_green),
            sdpercgreen=sd(perc_green),
            meanpercdiatom=mean(perc_diatom),
            sdpercdiatom=sd(perc_diatom),
            meanperccyano=mean(perc_cyano),
            sdperccyano=sd(perc_cyano),
            n()) %>%
  arrange(temperature)
str(benthictest)
#Specify temperature as factor
benthictest$temperature <- as.factor(benthictest$temperature)

na.omit(benthictest)
glimpse(benthictest)


##BAR PLOTS
#Order the factors for graphing
benthictest$treatment = factor(benthictest$treatment, 
                                       levels=c('Control','Nitrogen','Phosphorus',
                                                'N+P'))


# #Showing changes in Total Benthic Benthotorch Chlorophyll  --------------

#All over the place
benthochlorophyll <- ggplot(benthictest, aes(x=temperature, y=meanbenthicchla, fill=treatment)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=meanbenthicchla-sdbenthicchla, ymax=meanbenthicchla+sdbenthicchla),
                width=.2,
                position=position_dodge(0.9))
benthochlorophyll



#Showing changes in Benthotorch Greens 
benthogreen <- ggplot(benthictest, aes(x=temperature, y=meangreen, fill=treatment)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=meangreen-sdgreen, ymax=meangreen+sdgreen),
                width=.2,
                position=position_dodge(0.9))
benthogreen

#Showing changes in Benthotorch Diatoms 
benthodiatoms <- ggplot(benthictest, aes(x=temperature, y=meandiatom, fill=treatment)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=meandiatom-sddiatom, ymax=meandiatom+sddiatom),
                width=.2,
                position=position_dodge(0.9))
benthodiatoms

#Showing changes in Benthotorch Diatoms 
benthocyano <- ggplot(benthictest, aes(x=temperature, y=meancyano, fill=treatment)) +
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=meancyano-sdcyano, ymax=meancyano+sdcyano),
                width=.2,
                position=position_dodge(0.9))
benthocyano


# Benthotorch Community Composition ---------------------------------------

trtcolors <- c("#38305c", "#9b3c73", "#ec5956", "#ffa600") #treatment colors

post_perc_green_box <- ggplot(benthic.data, aes(x=temperature, y=post_perc_green, fill=treatment)) +
  geom_boxplot()+
  stat_boxplot(geom ='errorbar') +
  scale_fill_manual(values=trtcolors,
                    name="",
                    labels=c("Control", "Nitrogen", "Phosphorus", "N&P"))+
  ggtitle("Post % Green Algae (Benthotorch)")
post_perc_green_box

pre_perc_green_box <- ggplot(benthic.data, aes(x=temperature, y=pre_perc_green, fill=treatment)) +
  geom_boxplot()+
  stat_boxplot(geom ='errorbar') +
  scale_fill_manual(values=trtcolors,
                    name="",
                    labels=c("Control", "Nitrogen", "Phosphorus", "N&P"))+
  ggtitle("Pre % Green Algae (Benthotorch)")
pre_perc_green_box


