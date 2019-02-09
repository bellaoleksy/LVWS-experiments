
# Data Wrangling ----------------------------------------------------------
setwd("/Users/isabellaoleksy/Desktop/Bella's Folder/Colorado State University/Research/Data/2017 data/Experiments/Warming X Nutrient Experiment/R")
TxN.raw <- read.csv("TxN_withdilutions.csv")

library(plyr)
library(dplyr)
library(tidyr)
tbl_df(TxN.raw)



# TxN.totals <- TxN.raw %>%
#   mutate(ID = paste(run_ID, substr(pigment_num, 1, 5))) %>%
#   group_by(ID) %>%
#   summarize(mass_ug = sum(mass_ug))
# glimpse(TxN.totals) #creates a dataframe with col = "run_ID pigment_num" and sum of mass_ug
# # of each pigment group. 1= fuco, 2=aphan, 3=myxo and primes, 4= chlb and primes, 5=chla, primes, and pheo


# #This separates the col ="run_ID pigment_num" into two separate columns
# NDS.totals <- separate_(NDS.totals,
#           col = "ID",
#           into = c("run_ID", "pigment_num"),
#           sep = " ")
# 
# write.csv(NDS.totals, file="NDS_totals_180420.csv")
# #exporting the file and adding a column "pigment_ID" and removing "pigment_num"
# #couldnt find a way to do this efficeintly in R. frustrating. It's prob super easy. 
# 
# NDS.totals.new <- read.csv("/Users/isabellaoleksy/Desktop/Bella's Folder/Colorado State University/Research/Data/2017 data/Experiments/NDS/R/NDS_totals_180420.csv")
# NDS.totals.new <- NDS.totals.new %>%
#   select(run_ID, new_pigment_ID, mass_ug)
# 
# NDS.data <- bind_rows(NDS.raw, NDS.totals.new)
# #What happens here is there is a new column called "new_pigment_ID"
# #which contains the totals. In Excel I'm going to merge the 2 column IDs
# #and preseve the mass_ug calculations
# write.csv(NDS.data, file="NDS_IntegrationsDataSheet_Bella_withtotals_180421.csv")


# Data Exploration --------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(ggpubr)
#Example: theme_few() + scale_color_few()

TxN.data.trim <- TxN.raw %>%
  filter(!run_ID %in% c("6C", "7C", "8C","5C_rerun")) %>%
  filter(temperature %in% c("12","16"))%>%
  select(run_ID, temperature, sample_rep, treatment, pigment_ID, mass_ug_cm2) %>%
  filter(pigment_ID %in% c("chla","chlb")) #Since fuco, myxo, aphan had n of 1, only select chlb and chla

#Add in percent green algae of sample
library(reshape2)
#Data in long format. Need to go to wide format.
TxN.data.trim.wide <- dcast(TxN.data.trim, run_ID + temperature + treatment + sample_rep ~ pigment_ID, value.var="mass_ug_cm2") %>%
  arrange(temperature,treatment,run_ID) %>%
  mutate(perc_green = (chlb/chla)*100)
#Convert back to long format.
TxN.data.full = melt(TxN.data.trim.wide, id.vars = c("run_ID", "temperature", "treatment", "sample_rep"),
                     measure.vars = c("chla", "chlb",
                                      "perc_green")) %>%
  arrange(run_ID,treatment, temperature) %>%
  rename(pigment_ID = variable, #rename columns 
         mass_ug_cm2 = value)


TxN.data.summary <- TxN.data.full %>%
  group_by(pigment_ID, temperature, treatment) %>%
  summarize(mean_ug_cm2 = mean(mass_ug_cm2),
            sd_mass_ug_cm2=sd(mass_ug_cm2),
            n = n())

#Order the factors for graphing
TxN.data.summary$treatment = factor(TxN.data.summary$treatment, 
                                      levels=c('C','N','P',
                                               'NP'))
TxN.data.full$treatment = factor(TxN.data.full$treatment, 
                                         levels=c('C','N','P',
                                                  'NP'))


#Make a bar graph with a bar for each treatment and a box for each pigment type 
limits <- aes(ymax = TxN.data.summary$mean_ug_cm2 + TxN.data.summary$sd_mass_ug_cm2,
              ymin = TxN.data.summary$mean_ug_cm2 - TxN.data.summary$sd_mass_ug_cm2)
trtcolors <- c("#38305c", "#9b3c73", "#ec5956", "#ffa600") #treatment colors

visual1 <- ggplot(data = TxN.data.summary, aes(x = factor(treatment), y = mean_ug_cm2, fill=treatment))

visual1 + facet_wrap(pigment_ID ~ temperature, ncol=2, scales="free") +
  theme_few()+
  geom_bar(stat = "identity",
           position = position_dodge(0.9)) +
  scale_fill_manual(values=trtcolors,
                    name="",
                    labels=c("Control", "Nitrogen", "Phosphorus", "N&P"))+
  geom_errorbar(limits, position = position_dodge(0.9),
                width = 0.25)


#But bar plots are shit and misleading so let's look at boxplots instead

visual2 <- ggplot(data = TxN.data.full, aes(x = factor(treatment), y = mass_ug_cm2, fill=treatment))+
  geom_boxplot()+
  stat_boxplot(geom ='errorbar') +
  scale_fill_manual(values=trtcolors,
                    name="",
                    labels=c("Control", "Nitrogen", "Phosphorus", "N&P"))+
  facet_wrap(pigment_ID ~ temperature, ncol=2, scales="free")+
  theme_classic2(base_size=16) 
visual2  


#Univariate scatterplot
visual3 <- ggplot(data = TxN.data.full, aes(x = factor(treatment), y = mass_ug_cm2, color=treatment))+
  geom_point()+
  scale_color_manual(values=trtcolors,
                    name="",
                    labels=c("Control", "Nitrogen", "Phosphorus", "N&P"))+
  facet_wrap(pigment_ID ~ temperature, ncol=2, scales="free")+
  theme_classic2(base_size=16) 
visual3

