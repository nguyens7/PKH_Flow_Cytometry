#########This is a guideline for importing and formating the data as well as the code to generate all the graphs/figures

#To Clear working environment
rm(list=ls())

# Set the correct root folder for Mac
setwd("~/Desktop/R_Folder")

#Set the correct root folder for PC
setwd("~/")

#Be sure to load packages by going to the packages tab in the bottom right pane and click on the packages tab
#Once in the Packages tab, press the install button and search for the following packages and install them
# ggplot2,plyr,dplyr,RColorBrewer,reshape2

#Load ggplot2,plyr and dplyr packages
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(RColorBrewer)
library(reshape2)
library(ggvis)

#Import PKH_splenocytes.csv
data <- "PKH_Splenocytes_082616.csv"
raw_data <- read.csv(data, sep = ",", header = TRUE)

#Tell R to ignore missing data, it will ignore the control tubes (FMO,Single stain, IgG Cocktai,Auto, etc....)

my_data <- raw_data %>%
  select(Data.Set,Input.Gate,X.Parameter,Y.Parameter,Gate,Number,X.Total,X.Gated)%>% #Select for which columns that are of interest
  separate(Data.Set, c("Tube","Parameter","Animal","Condition"), sep= "_")#Separate Data.Set column into different columns

sp <- my_data%>% 
  select(-Tube)%>%
  filter(Animal == "1"|Animal == "2"|Animal == "3")%>%
  arrange(Animal,Condition,Parameter,Input.Gate)#Arrange rows in ascending order

sp$X.Gated <- as.numeric(as.character(sp$X.Gated)) # Ensure that the Gated column is read properly
sp$Parameter <- as.factor(as.character(sp$Parameter))
sp$Animal <- as.factor(as.character(sp$Animal))
sp$Condition <- as.factor(as.character(sp$Condition))

levels(sp$Parameter) <- c("Dye","1ug","5ug","10ug") #Set the correct order 
levels(sp$Condition) <- c("4","37") #Set the correct order

str(sp)

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


#Identifying Monocytes

#Gate on CD11b+ and F4.80- cells

Monocytes <- sp %>%
  filter(Gate == "CD11b+F4.80-")%>%              #Filter for cells that are CD11b+ and F4.80+
  group_by(Animal,Condition,Parameter)%>%   #Organize the data by Replicate, Animal, Condition,Parameter
  mutate(average = mean(X.Gated))%>%          #Create a new column to average the technical replicates (2)
  group_by(Condition,Parameter)%>%          #Organize the data by looking at the Condition and Parameter columns
  summarise(   N = length(average),         #Summarize the data by averaging the three biological replicates and calculate the standard deviation
               mean = mean(average),           #and standard error
               sd = sd(average),
               se = sd / sqrt(N))

Monocytes

#Generate a graph
Monocytes_plot <- ggplot(Monocytes, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, #Size of the error bars
                width=.25, # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") + # X axis label
  ylab("% CD11b+ F4/80- Cells") + # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("Monocytes") + scale_y_continuous(expand=c(0,0),limits = c(0, 60)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
Monocytes_final <- Monocytes_plot + publication_style

#Test to see if it works
Monocytes_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/Monocytes_Dose_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
Monocytes_final
dev.off()  



#Identifying Macrophages
#Gate on CD11b- F4.80+ Cells

Macrophages <- sp %>%
  filter(Gate == "CD11b-F4.80+")%>%              #Filter for cells that are CD11b+ and F4.80+
  group_by(Animal,Condition,Parameter)%>%   #Organize the data by Replicate, Animal, Condition,Parameter
  mutate(average = mean(X.Gated))%>%          #Create a new column to average the technical replicates (2)
  group_by(Condition,Parameter)%>%          #Organize the data by looking at the Condition and Parameter columns
  summarise(   N = length(average),         #Summarize the data by averaging the three biological replicates and calculate the standard deviation
               mean = mean(average),           #and standard error
               sd = sd(average),
               se = sd / sqrt(N))

#Generate a graph
Macrophages_plot <- ggplot(Macrophages, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, #Size of the error bars
                width=.25, # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") + # X axis label
  ylab("% CD11b- F4/80+ Cells") + # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("Macrophages") + scale_y_continuous(expand=c(0,0),limits = c(0, 60)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
Macrophages_final <- Macrophages_plot + publication_style

#Test to see if it works
Macrophages_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/Macrophages_Dose_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
Macrophages_final
dev.off()  


#Identify Dendritic Cell Population

Dendritic_Cells <- sp %>%
  filter(Gate == "CD11c+MHCII+")%>%              #Filter for cells that are CD11c+ and MHCII+
  group_by(Animal,Condition,Parameter)%>%   #Organize the data by Replicate, Animal, Condition,Parameter
  mutate(average = mean(X.Gated))%>%          #Create a new column to average the technical replicates (2)
  group_by(Condition,Parameter)%>%          #Organize the data by looking at the Condition and Parameter columns
  summarise(   N = length(average),         #Summarize the data by averaging the three biological replicates and calculate the standard deviation
               mean = mean(average),           #and standard error
               sd = sd(average),
               se = sd / sqrt(N))

#Generate a graph
DC_plot <- ggplot(Dendritic_Cells, aes(x=Condition, y=mean, fill=Parameter)) +
geom_bar(position=position_dodge(), stat="identity",colour="black") +
#scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
              size=0.5, #Size of the error bars
              width=.25, # Width of the error bars
              position=position_dodge(.9)) + #Where to put the errorbars
xlab("Treatment Condition") + # X axis label
ylab("% CD11c+ MHCII+ Cells") + # Y axis label
## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
ggtitle("Dendritic Cells") + scale_y_continuous(expand=c(0,0),limits = c(0, 60)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
DC_final <- DC_plot + publication_style

#Test to see if it works
DC_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/Dendritic_Cells_Dose_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
DC_final
dev.off()  



############################################################################################################################


#Monocyte and PKH67 Labeled exosome uptake comparison
#Look at Monocyte population then compare between Dye and 5ug conditions

Monocyte_cells<-  sp %>% 
  filter(Input.Gate == "[CD11b+F4.80-]",Gate == "PKH+CD11b+"|Gate == "PKH-CD11b+")%>%
  group_by(Animal,Condition,Parameter)%>%
  mutate(Total_Monocytes = sum(Number))
  
PKH.Monocytes <- sp %>%                                      
  group_by(Animal,Condition,Parameter)%>%    
  select(-X.Total,-X.Gated)%>%                              
  filter(Gate == "PKH+CD11b+")

Gated_Monocytes_PKH <-inner_join(PKH.Monocytes,Monocyte_cells)%>%           
  mutate(Ratio = (Number/Total_Monocytes) * 100 )%>%  
  group_by(Animal,Condition,Parameter)%>%   
  mutate(average = mean(Ratio))%>%          
  group_by(Condition,Parameter)%>%          
  summarise(   N = length(average),         
               mean = mean(average),           
               sd = sd(average),
               se = sd / sqrt(N))

#Generate a graph
Ratio_Monocytes_PKHpos_plot <- ggplot(Gated_Monocytes_PKH, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, #Size of the error bars
                width=.25, # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") + # X axis label
  ylab(" % of Cells PKH+") + # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("CD11b+ F4/80- Monocytes") + scale_y_continuous(expand=c(0,0),limits = c(0, 100)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
Ratio_Monocytes_PKHpos_plot_final <- Ratio_Monocytes_PKHpos_plot + publication_style

#Test to see if it works
Ratio_Monocytes_PKHpos_plot_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/Ratio_Monocytes_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
Ratio_Monocytes_PKHpos_plot_final
dev.off()


#Macrophage and PKH67 Labeled exosome uptake comparison
#Look at Macrophage population then compare between Dye and 5ug conditions

Macrophage_cells<- sp2 %>%
  filter(Input_Gate == "CD11b-F4.80+", X_Parameter == "APC-Cy7-A", Y_Parameter == "FITC-A",Gate == "PKH+M0+F4.80+"|Gate == "PKH-M0+F4.80+")%>%
  group_by(Replicate,Animal,Condition,Parameter)%>%
  mutate(Total_Macrophages = sum(Number))

PKH.Macrophages <- sp2 %>%                                      
  group_by(Replicate,Animal,Condition,Parameter)%>%    
  select(-Total,-Gated)%>%                              
  filter(Gate == "PKH+M0+F4.80+")

Gated_Macrophages_PKH <-inner_join(PKH.Macrophages,Macrophage_cells)%>%           
  mutate(Ratio = (Number/Total_Macrophages) * 100 )%>%  
  group_by(Animal,Condition,Parameter)%>%   
  mutate(average = mean(Ratio))%>%          
  group_by(Condition,Parameter)%>%          
  summarise(   N = length(average),         
               mean = mean(average),           
               sd = sd(average),
               se = sd / sqrt(N))

#Generate a graph
Ratio_Macrophages_PKHpos_plot <- ggplot(Gated_Macrophages_PKH, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, #Size of the error bars
                width=.25, # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") + # X axis label
  ylab(" % of Cells PKH+") + # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("CD11b- F4/80+ Macrophages") + scale_y_continuous(expand=c(0,0),limits = c(0, 100)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
Ratio_Macrophages_PKHpos_plot_final <- Ratio_Macrophages_PKHpos_plot + publication_style

#Test to see if it works
Ratio_Macrophages_PKHpos_plot_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/Ratio_Macrophages_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
Ratio_Macrophages_PKHpos_plot_final
dev.off()




#Macrophage and PKH67 Labeled exosome uptake comparison
#Look at Macrophage population then compare between Dye and 5ug conditions

Macrophage_cells<- sp2 %>%
  filter(Input_Gate == "CD11b-F4.80+", X_Parameter == "APC-Cy7-A", Y_Parameter == "FITC-A",Gate == "PKH+M0+F4.80+"|Gate == "PKH-M0+F4.80+")%>%
  group_by(Replicate,Animal,Condition,Parameter)%>%
  mutate(Total_Macrophages = sum(Number))

PKH.Macrophages <- sp2 %>%                                      
  group_by(Replicate,Animal,Condition,Parameter)%>%    
  select(-Total,-Gated)%>%                              
  filter(Gate == "PKH+M0+F4.80+")

Gated_Macrophages_PKH <-inner_join(PKH.Macrophages,Macrophage_cells)%>%           
  mutate(Ratio = (Number/Total_Macrophages) * 100 )%>%  
  group_by(Animal,Condition,Parameter)%>%   
  mutate(average = mean(Ratio))%>%          
  group_by(Condition,Parameter)%>%          
  summarise(   N = length(average),         
               mean = mean(average),           
               sd = sd(average),
               se = sd / sqrt(N))

#Generate a graph
Ratio_Macrophages_PKHpos_plot <- ggplot(Gated_Macrophages_PKH, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, #Size of the error bars
                width=.25, # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") + # X axis label
  ylab(" % of Cells PKH+") + # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("CD11b- F4/80+ Macrophages") + scale_y_continuous(expand=c(0,0),limits = c(0, 100)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
Ratio_Macrophages_PKHpos_plot_final <- Ratio_Macrophages_PKHpos_plot + publication_style

#Test to see if it works
Ratio_Macrophages_PKHpos_plot_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/Ratio_Macrophages_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
Ratio_Macrophages_PKHpos_plot_final
dev.off()

#Dendritic Cell and PKH67 Labeled exosome uptake comparison
#Look at Dendritic Cell population then compare between Dye and 5ug conditions

Dendritic_cells<- sp2 %>%
  filter(Input_Gate == "CD11c+MHCII+")%>%
  group_by(Replicate,Animal,Condition,Parameter)%>%
  mutate(Total_DCs = sum(Number))

PKH.DCs <- sp2 %>%                                      
  group_by(Replicate,Animal,Condition,Parameter)%>%    
  select(-Total,-Gated)%>%                              
  filter(Gate == "PKH+DC+")

Gated_DCs_PKH <-inner_join(PKH.DCs,Dendritic_cells)%>%           
  mutate(Ratio = (Number/Total_DCs) * 100 )%>%  
  group_by(Animal,Condition,Parameter)%>%   
  mutate(average = mean(Ratio))%>%          
  group_by(Condition,Parameter)%>%          
  summarise(   N = length(average),         
               mean = mean(average),           
               sd = sd(average),
               se = sd / sqrt(N))

#Generate a graph
Ratio_DCs_PKHpos_plot <- ggplot(Gated_DCs_PKH, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, #Size of the error bars
                width=.25, # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") + # X axis label
  ylab(" % of Cells PKH+") + # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("CD11c+ MHCII+ Macrophages") + scale_y_continuous(expand=c(0,0),limits = c(0, 100)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
Ratio_DCs_PKHpos_plot_final <- Ratio_DCs_PKHpos_plot + publication_style

#Test to see if it works
Ratio_DCs_PKHpos_plot_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/Ratio_DCs_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
Ratio_DCs_PKHpos_plot_final
dev.off()



#Looking at MHCII+ population and calculating the ratio of cells that are PKH+
MHCII_cells <- sp%>%
  filter(Input_Gate == "APCs",Gate == "PKH-MHCII+" | Gate == "PKH+MHCII+") %>%
  group_by(Replicate,Animal,Condition,Parameter)%>%
  mutate(Total_MHCII = sum(Number))

PKH.MHCII <- sp2 %>%                                      
  group_by(Replicate,Animal,Condition,Parameter)%>%    
  select(-Total,-Gated)%>%                              
  filter(Gate == "PKH+MHCII+")

Gated_MHCII_PKH <-inner_join(PKH.MHCII,MHCII_cells)%>%           
  mutate(Ratio = (Number/Total_MHCII) * 100 )%>%  
  group_by(Animal,Condition,Parameter)%>%   
  mutate(average = mean(Ratio))%>%          
  group_by(Condition,Parameter)%>%          
  summarise(   N = length(average),         
               mean = mean(average),           
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
  ggtitle("MHCII+ APC Splenocytes") + scale_y_continuous(expand=c(0,0),limits = c(0, 100)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
Ratio_MHCII_PKHpos_plot_final <- Ratio_MHCII_PKHpos_plot + publication_style

#Test to see if it works
Ratio_MHCII_PKHpos_plot_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/Ratio_MHCII_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
Ratio_MHCII_PKHpos_plot_final
dev.off()




#Looking at CD11c+ population and calculating the ratio of cells that are PKH+
CD11c_cells <- sp%>%
  filter(Input_Gate == "APCs",Gate == "PKH-CD11c+" | Gate == "PKH+CD11c+") %>%
  group_by(Replicate,Animal,Condition,Parameter)%>%
  mutate(Total_CD11c = sum(Number))

PKH.CD11c <- sp2 %>%                                      
  group_by(Replicate,Animal,Condition,Parameter)%>%    
  select(-Total,-Gated)%>%                              
  filter(Gate == "PKH+CD11c+")

Gated_CD11c_PKH <-inner_join(PKH.CD11c,CD11c_cells)%>%           
  mutate(Ratio = (Number/Total_CD11c) * 100 )%>%  
  group_by(Animal,Condition,Parameter)%>%   
  mutate(average = mean(Ratio))%>%          
  group_by(Condition,Parameter)%>%          
  summarise(   N = length(average),         
               mean = mean(average),           
               sd = sd(average),
               se = sd / sqrt(N))


#Generate a graph
Ratio_CD11c_PKHpos_plot <- ggplot(Gated_CD11c_PKH, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, #Size of the error bars
                width=.25, # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") + # X axis label
  ylab(" % of Cells PKH+") + # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("CD11c+ APC Splenocytes") + scale_y_continuous(expand=c(0,0),limits = c(0, 100)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
Ratio_CD11c_PKHpos_plot_final <- Ratio_CD11c_PKHpos_plot + publication_style

#Test to see if it works
Ratio_CD11c_PKHpos_plot_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/Ratio_CD11c_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
Ratio_CD11c_PKHpos_plot_final
dev.off()



#Looking at CD11b+ population and calculating the ratio of cells that are PKH+
CD11b_cells <- sp%>%
  filter(Input_Gate == "APCs",Gate == "PKH-CD11b+" | Gate == "PKH+CD11b+") %>%
  group_by(Replicate,Animal,Condition,Parameter)%>%
  mutate(Total_CD11b = sum(Number))

PKH.CD11b <- sp2 %>%                                      
  group_by(Replicate,Animal,Condition,Parameter)%>%    
  select(-Total,-Gated)%>%                              
  filter(Gate == "PKH+CD11b+")

Gated_CD11b_PKH <-inner_join(PKH.CD11b,CD11b_cells)%>%           
  mutate(Ratio = (Number/Total_CD11b) * 100 )%>%  
  group_by(Animal,Condition,Parameter)%>%   
  mutate(average = mean(Ratio))%>%          
  group_by(Condition,Parameter)%>%          
  summarise(   N = length(average),         
               mean = mean(average),           
               sd = sd(average),
               se = sd / sqrt(N))


#Generate a graph
Ratio_CD11b_PKHpos_plot <- ggplot(Gated_CD11b_PKH, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, #Size of the error bars
                width=.25, # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") + # X axis label
  ylab(" % of Cells PKH+") + # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("CD11b+ APC Splenocytes") + scale_y_continuous(expand=c(0,0),limits = c(0, 100)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
Ratio_CD11b_PKHpos_plot_final <- Ratio_CD11b_PKHpos_plot + publication_style

#Test to see if it works
Ratio_CD11b_PKHpos_plot_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/Ratio_CD11b_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
Ratio_CD11b_PKHpos_plot_final
dev.off()


#Looking at F4.80+ population and calculating the ratio of cells that are PKH+
F4.80_cells <- sp%>%
  filter(Input_Gate == "APCs",Gate == "PKH-F4.80+" | Gate == "PKH+F4.80+") %>%
  group_by(Replicate,Animal,Condition,Parameter)%>%
  mutate(Total_F4.80 = sum(Number))

PKH.F4.80 <- sp2 %>%                                      
  group_by(Replicate,Animal,Condition,Parameter)%>%    
  select(-Total,-Gated)%>%                              
  filter(Gate == "PKH+F4.80+")

Gated_F4.80_PKH <-inner_join(PKH.F4.80,F4.80_cells)%>%           
  mutate(Ratio = (Number/Total_F4.80) * 100 )%>%  
  group_by(Animal,Condition,Parameter)%>%   
  mutate(average = mean(Ratio))%>%          
  group_by(Condition,Parameter)%>%          
  summarise(   N = length(average),         
               mean = mean(average),           
               sd = sd(average),
               se = sd / sqrt(N))


#Generate a graph
Ratio_F4.80_PKHpos_plot <- ggplot(Gated_F4.80_PKH, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  #scale_fill_manual(values=c("black", "grey"))+ #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                size=0.5, #Size of the error bars
                width=.25, # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") + # X axis label
  ylab(" % of Cells PKH+") + # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) +
  ggtitle("F4.80+ APC Splenocytes") + scale_y_continuous(expand=c(0,0),limits = c(0, 100)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
Ratio_F4.80_PKHpos_plot_final <- Ratio_F4.80_PKHpos_plot + publication_style

#Test to see if it works
Ratio_F4.80_PKHpos_plot_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/Ratio_F4.80_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
Ratio_F4.80_PKHpos_plot_final
dev.off()





