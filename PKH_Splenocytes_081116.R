#########This is a guideline for importing and formating the data as well as the code to generate all the graphs/figures

#To Clear working environment
rm(list=ls())
graphics.off()

# Set the correct root folder for Mac
setwd("~/Desktop/R_Folder")

#Set the correct root folder for PC
setwd("~/")

#Be sure to load packages by going to the packages tab in the bottom right pane and click on the packages tab
#Once in the Packages tab, press the install button and search for the following packages and install them
# ggplot2,plyr,dplyr,RColorBrewer,reshape2

#Load tidyverse and misc packages
library(tidyverse)
library(RColorBrewer)
library(ggvis)

#Import PKH_splenocytes.csv
data <- "PKH_Splenocytes_081116.csv"
raw_data <- read.csv(data, sep = ",", header = TRUE)

#Tell R to ignore missing data, it will ignore the control tubes (FMO,Single stain, IgG Cocktai,Auto, etc....)
sp <- na.omit(raw_data) %>%
  select(Replicate,Animal,Condition,Parameter,Gate,Number,Total,Gated) %>% #View only these specific columns
  arrange(Replicate,Animal,Condition,Parameter) #Arrange rows in ascending order

#Make a default theme for optimizeing graph aesthetics
publication_style <- theme(axis.line.x=element_line(color="black",size=1.0), #Make X axis size 1.0 and black
                           axis.line.y=element_line(color="black",size=1.0), #Make Y axis size 1.0 and black
                           panel.background=element_rect(fill=NA,size=rel(20)), #Remove grey background
                           panel.grid.minor=element_line(colour=NA), #Remove grid lines
                           axis.text=element_text(size=18,colour="black"), #Make axis text black and size 18
                           axis.title=element_text(size=20,face="bold"),legend.text=element_text(size=20), # Make Title axis bold and size 20
                           plot.title = element_text(face="bold", size=30)) #Make plot title bold and size 30


################ GUIDE TO MAKING A PLOT WITH FLOW DATA ########################
# 1) Filter on the parameter that you want
# 2) Calculate the mean of your technical replicates
# 3) Rename the "mean" column to average so you can calculate biological replicates if any
# 4) Calculate the mean of your biological replicates
# 5) Graph your data
# 6) Adjust your plot to your liking



######## MAKING A PLOT FOR CELLS THAT ARE PKH POSITIVE##########################

PKH <- sp %>%
  filter(Gate == "PKH_Pos")%>%              #Filter for cells that are PKH_Pos
  group_by(Animal,Condition,Parameter)%>%   #Organize the data by Replicate, Animal, Condition,Parameter
  mutate(average = mean(Gated))%>%          #Create a new column to average the technical replicates (2)
  group_by(Condition,Parameter)%>%          #Organize the data by looking at the Condition and Parameter columns
  summarise(   N = length(average),         #Summarize the data by averaging the three biological replicates and calculate the standard deviation
               mean = mean(average),           #and standard error
               sd = sd(average),
               se = sd / sqrt(N))

#Generate a graph
PKH_plot <- ggplot(PKH, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, #Size of the error bars
                width=.25, # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") + # X axis label
  ylab(" % of Cells PKH67+") + # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("") + scale_y_continuous(expand=c(0,0),limits = c(0, 70)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
PKH_final <- PKH_plot + publication_style

#Test to see if it works
PKH_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/PKH_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
PKH_final
dev.off()  


######## MAKING A PLOT FOR CELLS THAT ARE CD11b POSITIVE##########################  
CD11b <- sp %>%
  filter(Gate == "CD11b_Pos")%>%            #Filter for cells that are CD11b_Pos
  group_by(Animal,Condition,Parameter)%>%   #Organize the data by Replicate, Animal, Condition,Parameter
  mutate(average = mean(Gated))%>%          #Create a new column to average the technical replicates (2)
  group_by(Condition,Parameter)%>%          #Organize the data by looking at the Condition and Parameter columns
  summarise(   N = length(average),         #Summarize the data by averaging the three biological replicates and calculate the standard deviation
               mean = mean(average),        #and standard error
               sd = sd(average),
               se = sd / sqrt(N))

#Generate a graph
CD11b_plot <- ggplot(CD11b, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, #Size of the error bars
                width=.25, # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") + # X axis label
  ylab(" % of Cells CD11b+") + # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("") + scale_y_continuous(expand=c(0,0),limits = c(0, 70)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
CD11b_final <- CD11b_plot + publication_style

#Test to see if it works
CD11b_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/CD11b_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
CD11b_final
dev.off()

######## MAKING A PLOT FOR CELLS THAT ARE CD11c POSITIVE##########################
CD11c <- sp %>%
  filter(Gate == "CD11c_Pos")%>%               #Filter for cells that are CD11c_Pos
  group_by(Animal,Condition,Parameter)%>%   #Organize the data by Replicate, Animal, Condition,Parameter
  mutate(average = mean(Gated))%>%          #Create a new column to average the technical replicates (2)
  group_by(Condition,Parameter)%>%          #Organize the data by looking at the Condition and Parameter columns
  summarise(   N = length(average),         #Summarize the data by averaging the three biological replicates and calculate the standard deviation
               mean = mean(average),        #and standard error
               sd = sd(average),
               se = sd / sqrt(N))

#Generate a graph
CD11c_plot <- ggplot(CD11c, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, #Size of the error bars
                width=.25, # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") + # X axis label
  ylab(" % of Cells CD11c+") + # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("") + scale_y_continuous(expand=c(0,0),limits = c(0, 70)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
CD11c_final <- CD11c_plot + publication_style

#Test to see if it works
CD11c_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/CD11c_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
CD11c_final
dev.off()

######## MAKING A PLOT FOR CELLS THAT ARE MHCII POSITIVE##########################

MHCII <- sp %>%
  filter(Gate == "MHCII_Pos")%>%            #Filter for cells that are MHCII_Pos
  group_by(Animal,Condition,Parameter)%>%   #Organize the data by Replicate, Animal, Condition,Parameter
  mutate(average = mean(Gated))%>%          #Create a new column to average the technical replicates (2)
  group_by(Condition,Parameter)%>%          #Organize the data by looking at the Condition and Parameter columns
  summarise(   N = length(average),         #Summarize the data by averaging the three biological replicates and calculate the standard deviation
               mean = mean(average),        #and standard error
               sd = sd(average),
               se = sd / sqrt(N))

#Generate a graph
MHCII_plot <- ggplot(MHCII, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, #Size of the error bars
                width=.25, # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") + # X axis label
  ylab(" % of Cells MHCII+") + # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("") + scale_y_continuous(expand=c(0,0),limits = c(0, 70)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
MHCII_final <- MHCII_plot + publication_style

#Test to see if it works
MHCII_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/MHCII_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
MHCII_final
dev.off()


######## MAKING A PLOT FOR CELLS THAT ARE MHCII POSITIVE##########################
F4.80 <- sp %>%
  filter(Gate == "F4.80_Pos")%>%            #Filter for cells that are F4.80_Pos
  group_by(Animal,Condition,Parameter)%>%   #Organize the data by Replicate, Animal, Condition,Parameter
  mutate(average = mean(Gated))%>%          #Create a new column to average the technical replicates (2)
  group_by(Condition,Parameter)%>%          #Organize the data by looking at the Condition and Parameter columns
  summarise(   N = length(average),         #Summarize the data by averaging the three biological replicates and calculate the standard deviation
               mean = mean(average),        #and standard error
               sd = sd(average),
               se = sd / sqrt(N))

#Generate a graph
F4.80_plot <- ggplot(F4.80, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, #Size of the error bars
                width=.25, # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") + # X axis label
  ylab(" % of Cells F4.80+") + # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("") + scale_y_continuous(expand=c(0,0),limits = c(0, 70)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
F4.80_final <- F4.80_plot + publication_style

#Test to see if it works
F4.80_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/F4.80_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
F4.80_final
dev.off()


#######################################################################################################################################
#######################################################################################################################################
################### LOOKING AT SPECIFIC POPULATIONS OF CELLS WITH PKH #################################################################
#######################################################################################################################################
#######################################################################################################################################

#Gate on MHCII and look for PKH Pos cells
MHCII_cells <- sp%>%                                    #Create new dataframe looking at MHCII positive cells
  filter(Gate == "PKH-MHCII+"| Gate == "PKH+MHCII+")%>% #Look within the Gate column to select for MHCII+ cells (PKH-MHCII+ and PKH+ MHCII+)
  group_by(Replicate,Animal,Condition,Parameter)%>%     #Organize the data by Replicate, Animal, Condition,Parameter
  mutate(MHCII = sum(Number))                        #Add up the PKH-MHCII+ and PKH+MHCII+ cells

PKH.MHCII <- sp%>%                                      #Create another dataframe with just the MHCII cells that are PKH+ (PKH+MHCII+)
  group_by(Replicate,Animal,Condition,Parameter)%>%     #Organize data 
  select(-Total,-Gated)%>%                              #Remove the total and gated columns as they aren't needed
  filter(Gate == "PKH+MHCII+")                          #Look only for PKH+MHCII+ cells within the Gate column

Gated_MHCII_PKH <-inner_join(PKH.MHCII,MHCII_cells)%>%           #Join the two data frames
  mutate(Ratio = (Number/MHCII) * 100 )%>%  #Create a new column named 'Ratio' = PKH+ cells/all MHCII+ cells (Calculates how many MHCII are PKH pos)
  group_by(Animal,Condition,Parameter)%>%   #Organize the data by looking only at the Animal, Condition and Parameter columns
  mutate(average = mean(Ratio))%>%          #Create a new column to average the technical replicates (2)
  group_by(Condition,Parameter)%>%          #Organize the data by looking at the Condition and Parameter columns
  summarise(   N = length(average),         #Summarize the data by averaging the three biological replicates and calculate the standard deviation
               mean = mean(average),           #and standard error
               sd = sd(average),
               se = sd / sqrt(N))

#Generate a graph
Ratio_MHCII_PKHpos_plot <- ggplot(Gated_MHCII_PKH, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, #Size of the error bars
                width=.25, # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") + # X axis label
  ylab(" % of Cells PKH+") + # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("MHCII+ Splenocytes") + scale_y_continuous(expand=c(0,0),limits = c(0, 70)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
Ratio_MHCII_PKHpos_plot_final <- Ratio_MHCII_PKHpos_plot + publication_style

#Test to see if it works
Ratio_MHCII_PKHpos_plot_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/Ratio_MHCII_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
Ratio_MHCII_PKHpos_plot_final
dev.off()



################### LOOKING AT CD11b+ POPULATIONS OF CELLS WITH PKH #################################################################

#Gate on CD11b and look for PKH Pos cells
CD11b_cells <- sp%>%                                    
  filter(Gate == "PKH-CD11b+"| Gate == "PKH+CD11b+")%>% 
  group_by(Replicate,Animal,Condition,Parameter)%>%
  mutate(CD11b = sum(Number))                        

PKH.CD11b <- sp%>%                                      
  group_by(Replicate,Animal,Condition,Parameter)%>%               
  select(-Total,-Gated)%>%                              
  filter(Gate == "PKH+CD11b+")                          

Gated_CD11b_PKH <-inner_join(PKH.CD11b,CD11b_cells)%>%           
  mutate(Ratio = (Number/CD11b *100))%>%  
  group_by(Animal,Condition,Parameter)%>%   
  mutate(average = mean(Ratio))%>%          
  group_by(Condition,Parameter)%>%          
  summarise(   N = length(average),         
               mean = mean(average),          
               sd = sd(average),
               se = sd / sqrt(N))

Ratio_CD11b_PKHpos_plot <- ggplot(Gated_CD11b_PKH, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, 
                width=.25, 
                position=position_dodge(.9)) +
  xlab("Treatment Condition") + 
  ylab(" % of Cells PKH67+") +
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("CD11b+ Splenocytes") + scale_y_continuous(expand=c(0,0),limits = c(0, 80)) 

Ratio_CD11b_PKHpos_plot_final <- Ratio_CD11b_PKHpos_plot + publication_style

Ratio_CD11b_PKHpos_plot_final 

png("~/R_plots/Ratio_CD11b_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
Ratio_CD11b_PKHpos_plot_final
dev.off()


################### LOOKING AT CD11c+ POPULATIONS OF CELLS WITH PKH #################################################################

#Gate on CD11b and look for PKH Pos cells
CD11c_cells <- sp%>%                                    
  filter(Gate == "PKH-CD11c+"| Gate == "PKH+CD11c+")%>% 
  group_by(Replicate,Animal,Condition,Parameter)%>%
  mutate(CD11c = sum(Number))                        

PKH.CD11c <- sp%>%                                      
  group_by(Replicate,Animal,Condition,Parameter)%>%               
  select(-Total,-Gated)%>%                              
  filter(Gate == "PKH+CD11c+")                          

Gated_CD11c_PKH <-inner_join(PKH.CD11c,CD11c_cells)%>%           
  mutate(Ratio = (Number/CD11c *100))%>%  
  group_by(Animal,Condition,Parameter)%>%   
  mutate(average = mean(Ratio))%>%          
  group_by(Condition,Parameter)%>%          
  summarise(   N = length(average),         
               mean = mean(average),          
               sd = sd(average),
               se = sd / sqrt(N))

Ratio_CD11c_PKHpos_plot <- ggplot(Gated_CD11c_PKH, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, 
                width=.25, 
                position=position_dodge(.9)) +
  xlab("Treatment Condition") + 
  ylab(" % of Cells PKH67+") +
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("CD11c+ Splenocytes") + scale_y_continuous(expand=c(0,0),limits = c(0, 80)) 

Ratio_CD11c_PKHpos_plot_final <- Ratio_CD11c_PKHpos_plot + publication_style

Ratio_CD11c_PKHpos_plot_final 

png("~/R_plots/Ratio_CD11c_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
Ratio_CD11c_PKHpos_plot_final
dev.off()



################### LOOKING AT F4.80 POPULATIONS OF CELLS WITH PKH #################################################################

#Gate on CD11b and look for PKH Pos cells
F4.80_cells <- sp%>%                                    
  filter(Gate == "PKH-F4.80+"| Gate == "PKH+F4.80+")%>% 
  group_by(Replicate,Animal,Condition,Parameter)%>%
  mutate(F4.80 = sum(Number))                        

PKH.F4.80 <- sp%>%                                      
  group_by(Replicate,Animal,Condition,Parameter)%>%               
  select(-Total,-Gated)%>%                              
  filter(Gate == "PKH+F4.80+")                          

Gated_F4.80_PKH <-inner_join(PKH.F4.80,F4.80_cells)%>%           
  mutate(Ratio = (Number/F4.80 *100))%>%  
  group_by(Animal,Condition,Parameter)%>%   
  mutate(average = mean(Ratio))%>%          
  group_by(Condition,Parameter)%>%          
  summarise(   N = length(average),         
               mean = mean(average),          
               sd = sd(average),
               se = sd / sqrt(N))

Ratio_F4.80_PKHpos_plot <- ggplot(Gated_F4.80_PKH, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, 
                width=.25, 
                position=position_dodge(.9)) +
  xlab("Treatment Condition") + 
  ylab(" % of Cells PKH67+") +
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("F4.80+ Splenocytes") + scale_y_continuous(expand=c(0,0),limits = c(0, 80)) 

Ratio_F4.80_PKHpos_plot_final <- Ratio_F4.80_PKHpos_plot + publication_style

Ratio_F4.80_PKHpos_plot_final 

png("~/R_plots/Ratio_F4.80_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
Ratio_F4.80_PKHpos_plot_final
dev.off()
