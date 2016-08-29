#########This is a guideline for importing and formating the data as well as the code to generate all the graphs/figures

#To Clear working environment
rm(list=ls())

# Set the correct root folder for Mac
setwd("~/Desktop/R_Folder")

#Set the correct root folder for PC
setwd("~/")

#Be sure to load packages by going to the packages tab in the bottom right pane and click on the packages tab
#Once in the Packages tab, press the install button and search for the following packages and install them
#ggplot2,plyr,dplyr,RColorBrewer,reshape2

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

sp$Parameter <- factor(sp$Parameter, levels = c("Dye","1ug","5ug","10ug")) #Set the correct order 
sp$Condition <- factor(sp$Condition, levels = c("4","37")) #Set the correct order

str(sp) #Check if everything is recognized correctly

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
# 2) Calculate the mean of your biological replicates
# 3) Summarise the data (mean,sd,se)
# 4) Graph your data
# 5) Adjust your plot to your liking


#Identifying Monocytes

#Gate on CD11b+ and F4.80- cells

Monocytes <- sp %>%
  filter(Gate == "CD11b+F4.80-")%>%         #Filter for cells that are CD11b+ and F4.80+
  group_by(Animal,Condition,Parameter)%>%   #Organize the data by Replicate, Animal, Condition,Parameter
  mutate(average = mean(X.Gated))%>%        #Create a new column to average the technical replicates (2)
  group_by(Condition,Parameter)%>%          #Organize the data by looking at the Condition and Parameter columns
  summarise(   N = length(average),         #Summarize the data by averaging the three biological replicates and calculate the standard deviation
               mean = mean(average),           #and standard error
               sd = sd(average),
               se = sd / sqrt(N))

Monocytes

#Generate a graph
Monocytes_plot <- ggplot(Monocytes, aes(x=Condition, y=mean, fill=Parameter)) + #Make a plot, X-Axis=Condition Y-Axis=Mean, Parameter=bars
  geom_bar(position=position_dodge(), stat="identity",colour="black") +         #Make the plot a bar graph with the bars outlined in black
  #scale_fill_manual(values=c("black", "grey"))+                      OPTIONAL  #Set colors to black and white
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), #Make error bars equal to the standard error
                size=0.5,  #Size of the error bars
                width=.25, #Width of the error bars
                position=position_dodge(.9)) +  #Where to put the errorbars
  xlab("Treatment Condition (°C)") + # X axis label
  ylab("% CD11b+ F4/80- Cells") + # Y axis label
 #scale_fill_hue(name="", breaks=c(""), labels=c("")) +   (use this to get rid of legend)
  ggtitle("Monocytes") + scale_y_continuous(expand=c(0,0),limits = c(0, 60)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
Monocytes_final <- Monocytes_plot + publication_style #Standardize the theme with publication style function

#Test to see if it works
Monocytes_final 

#Save as very high quality PNG @ 1000dpi
#good for publications
png("~/R_plots/Monocytes_Dose_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Monocytes_final
dev.off()  

#for Mac ~/Desktop/R_Folder
png("~/Desktop/R_Folder/R_plots/Monocytes_Dose_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Monocytes_final
dev.off()  


#Identifying Macrophages
#Gate on CD11b- F4.80+ Cells
Macrophages <- sp %>%
  filter(Gate == "CD11b-F4.80+")%>%         #Filter for cells that are CD11b+ and F4.80+
  group_by(Animal,Condition,Parameter)%>%   #Organize the data by Animal, Condition,Parameter
  mutate(average = mean(X.Gated))%>%        #Create a new column to average the biologicalreplicates (3)
  group_by(Condition,Parameter)%>%          #Organize the data by looking at the Condition and Parameter columns
  summarise(   N = length(average),         #Summarize the data by averaging the three biological replicates and calculate the standard deviation
               mean = mean(average),           #and standard error
               sd = sd(average),
               se = sd / sqrt(N))

#Generate a graph
Macrophages_plot <- ggplot(Macrophages, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=0.5, width=.25, position=position_dodge(.9)) + 
  xlab("Treatment Condition (°C)") +
  ylab("% CD11b- F4/80+ Cells") + 
  ggtitle("Macrophages") + scale_y_continuous(expand=c(0,0),limits = c(0, 60)) 

#Finalize plot
Macrophages_final <- Macrophages_plot + publication_style

#Test to see if it works
Macrophages_final 

#Save as very high quality PNG @ 1000dpi
png("~/R_plots/Macrophages_Dose_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Macrophages_final
dev.off()  


#For mac
png("~/Desktop/R_Folder/R_plots/Macrophages_Dose_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Macrophages_final
dev.off()  



#Identify Dendritic Cell Population
Dendritic_Cells <- sp %>%
  filter(Gate == "CD11c+MHCII+")%>%         #Filter for cells that are CD11c+ and MHCII+
  group_by(Animal,Condition,Parameter)%>%   #Organize the data by Replicate, Animal, Condition,Parameter
  mutate(average = mean(X.Gated))%>%        #Create a new column to average the biological replicates (3)
  group_by(Condition,Parameter)%>%          #Organize the data by looking at the Condition and Parameter columns
  summarise(   N = length(average),         #Summarize the data by averaging the three biological replicates and calculate the standard deviation
               mean = mean(average),           #and standard error
               sd = sd(average),
               se = sd / sqrt(N))

#Generate a graph
DC_plot <- ggplot(Dendritic_Cells, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity",colour="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=0.5, width=.25, position=position_dodge(.9)) +
  xlab("Treatment Condition (°C)") + 
  ylab("% CD11c+ MHCII+ Cells") +
  ggtitle("Dendritic Cells") + scale_y_continuous(expand=c(0,0),limits = c(0, 60))

#Finalize plot
DC_final <- DC_plot + publication_style 

#Test to see if it works
DC_final 

#Save as very high quality PNG @ 1000dpi
png("~/R_plots/Dendritic_Cells_Dose_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
DC_final
dev.off()  

#For mac
png("~/Desktop/R_Folder/R_plots/Dendritic_Cells_Dose_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
DC_final
dev.off()  

############################################################################################################################


#Monocyte and PKH67 Labeled exosome uptake comparison
#Look at Monocyte population  and look at the dose response then compare between treatment conditions

Monocyte_cells <-  sp %>%                #Create a data frame where you only look for CD11b+F4.80- cells
  filter(Input.Gate == "[CD11b+F4.80-]",Gate == "PKH+CD11b+"|Gate == "PKH-CD11b+")%>%   #Then look for CD11b+ cells that are PKH- and PKH+
  group_by(Animal,Condition,Parameter)%>% # select based off of Animal,Condition and Parameter
  mutate(Total_Monocytes = sum(Number))   #Create a new columnn named "Total_Monocytes" that sums up the PKH- CD11b+ cells and PKH+ CD11b+ cells 

PKH.Monocytes <- sp %>%                                      
  group_by(Animal,Condition,Parameter)%>%    #Create a data frame that looks at CD11b+ cells that are only PKH+
  select(-X.Total,-X.Gated)%>%                              
  filter(Gate == "PKH+CD11b+")

Gated_Monocytes_PKH <-inner_join(PKH.Monocytes,Monocyte_cells)%>%  #Make a data table where you determine the ratio of CD11b+ cells that are 
  mutate(Ratio = (Number/Total_Monocytes) * 100 )%>%               #PKH+ and then summarise this for each biological replicate and condition
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
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=0.5, width=.25,position=position_dodge(.9)) + 
  xlab("Treatment Condition (°C)") +
  ylab(" % of Cells PKH+") + 
  ggtitle("CD11b+ F4/80- Monocytes") + scale_y_continuous(expand=c(0,0),limits = c(0, 100)) 

#Finalize plot
Ratio_Monocytes_PKHpos_plot_final <- Ratio_Monocytes_PKHpos_plot + publication_style + scale_fill_brewer(palette="Greens")

#Test to see if it works
Ratio_Monocytes_PKHpos_plot_final

#Save as very high quality PNG @ 1000dpi
#good for publications
png("~/R_plots/Ratio_Monocytes_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Ratio_Monocytes_PKHpos_plot_final
dev.off()

#For mac
png("~/Desktop/R_Folder/R_plots/Ratio_Monocytes_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Ratio_Monocytes_PKHpos_plot_final
dev.off()

#Macrophage and PKH67 Labeled exosome uptake comparison
#Look at Macrophage population then compare between Dye and 5ug conditions

Macrophage_cells<- sp %>%
  filter(Input.Gate == "[CD11b-F4.80+]",Gate == "PKH+F4.80+"|Gate == "PKH-F4.80+")%>%
  group_by(Animal,Condition,Parameter)%>%
  mutate(Total_Macrophages = sum(Number))

PKH.Macrophages <- sp %>%                                      
  group_by(Animal,Condition,Parameter)%>%    
  select(-X.Total,-X.Gated)%>%                              
  filter(Gate == "PKH+F4.80+")

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
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=0.5, width=.25, position=position_dodge(.9)) +
  xlab("Treatment Condition (°C)") +
  ylab(" % of Cells PKH+") +
  ggtitle("CD11b- F4/80+ Macrophages") + scale_y_continuous(expand=c(0,0),limits = c(0, 100))

#Finalize plot
Ratio_Macrophages_PKHpos_plot_final <- Ratio_Macrophages_PKHpos_plot + publication_style + scale_fill_brewer(palette="Greens")

#Test to see if it works
Ratio_Macrophages_PKHpos_plot_final 

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/Ratio_Macrophages_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Ratio_Macrophages_PKHpos_plot_final
dev.off()

#For mac
png("~/Desktop/R_Folder/R_plots/Ratio_Macrophages_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Ratio_Macrophages_PKHpos_plot_final
dev.off()

#Dendritic Cell and PKH67 Labeled exosome uptake comparison
#Look at Dendritic Cell population then compare between Dye and 5ug conditions

Dendritic_cells<- sp %>%
  filter(Input.Gate == "[CD11c+MHCII+]")%>%
  group_by(Animal,Condition,Parameter)%>%
  mutate(Total_DCs = sum(Number))

PKH.DCs <- sp %>%                                      
  group_by(Animal,Condition,Parameter)%>%    
  select(-X.Total,-X.Gated)%>%                              
  filter(Gate == "PKH+.DC.MHCII+")

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
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=0.5, width=.25, position=position_dodge(.9)) +
  xlab("Treatment Condition (°C)") + 
  ylab(" % of Cells PKH+") +
  ggtitle("CD11c+ MHCII+ Dendritic Cells") + scale_y_continuous(expand=c(0,0),limits = c(0, 100)) 

#Finalize plot
Ratio_DCs_PKHpos_plot_final <- Ratio_DCs_PKHpos_plot + publication_style + scale_fill_brewer(palette="Greens")

#Test to see if it works
Ratio_DCs_PKHpos_plot_final 

#Save as very high quality PNG @ 1000dpi
png("~/R_plots/Ratio_DCs_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Ratio_DCs_PKHpos_plot_final
dev.off()

#For mac
png("~/Desktop/R_Folder/R_plots/Ratio_DCs_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Ratio_DCs_PKHpos_plot_final
dev.off()


#Looking at MHCII+ population and calculating the ratio of cells that are PKH+
MHCII_cells <- sp%>%
  filter(Gate == "PKH-MHCII+" | Gate == "PKH+MHCII+") %>%
  group_by(Animal,Condition,Parameter)%>%
  mutate(Total_MHCII = sum(Number))

PKH.MHCII <- sp %>%                                      
  group_by(Animal,Condition,Parameter)%>%    
  select(-X.Total,-X.Gated)%>%                              
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
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),size=0.5, width=.25,position=position_dodge(.9)) +
  xlab("Treatment Condition (°C)") + 
  ylab(" % of Cells PKH+") + 
  ggtitle("MHCII+ APC Splenocytes") + scale_y_continuous(expand=c(0,0),limits = c(0, 100)) 

#Finalize plot
Ratio_MHCII_PKHpos_plot_final <- Ratio_MHCII_PKHpos_plot + publication_style + scale_fill_brewer(palette="Greens")

#Test to see if it works
Ratio_MHCII_PKHpos_plot_final 

#Save as very high quality PNG @ 1000dpi
png("~/R_plots/Ratio_MHCII_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Ratio_MHCII_PKHpos_plot_final
dev.off()

#For mac
png("~/Desktop/R_Folder/R_plots/Ratio_MHCII_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Ratio_MHCII_PKHpos_plot_final
dev.off()


#Looking at CD11c+ population and calculating the ratio of cells that are PKH+
CD11c_cells <- sp%>%
  filter(Gate == "PKH-CD11c+" | Gate == "PKH+CD11c+") %>%
  group_by(Animal,Condition,Parameter)%>%
  mutate(Total_CD11c = sum(Number))

PKH.CD11c <- sp %>%                                      
  group_by(Animal,Condition,Parameter)%>%    
  select(-X.Total,-X.Gated)%>%                              
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
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),size=0.5,width=.25,position=position_dodge(.9)) + 
  xlab("Treatment Condition (°C)") + 
  ylab(" % of Cells PKH+") + 
  ggtitle("CD11c+ APC Splenocytes") + scale_y_continuous(expand=c(0,0),limits = c(0, 100))

#Finalize plot
Ratio_CD11c_PKHpos_plot_final <- Ratio_CD11c_PKHpos_plot + publication_style + scale_fill_brewer(palette="Greens")

#Test to see if it works
Ratio_CD11c_PKHpos_plot_final 

#Save as very high quality PNG @ 1000dpi
#good for publications
png("~/R_plots/Ratio_CD11c_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Ratio_CD11c_PKHpos_plot_final
dev.off()

#For mac
png("~/Desktop/R_Folder/R_plots/Ratio_CD11c_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Ratio_CD11c_PKHpos_plot_final
dev.off()



#Looking at CD11b+ population and calculating the ratio of cells that are PKH+
CD11b_cells <- sp%>%
  filter(Gate == "PKH-CD11b+" | Gate == "PKH+CD11b+") %>%
  group_by(Animal,Condition,Parameter)%>%
  mutate(Total_CD11b = sum(Number))

PKH.CD11b <- sp %>%                                      
  group_by(Animal,Condition,Parameter)%>%    
  select(-X.Total,-X.Gated)%>%                              
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
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=0.5, width=.25, position=position_dodge(.9)) +
  xlab("Treatment Condition (°C)") +
  ylab(" % of Cells PKH+") +
  ggtitle("CD11b+ APC Splenocytes") + scale_y_continuous(expand=c(0,0),limits = c(0, 100))

#Finalize plot
Ratio_CD11b_PKHpos_plot_final <- Ratio_CD11b_PKHpos_plot + publication_style + publication_style + scale_fill_brewer(palette="Greens")

#Test to see if it works
Ratio_CD11b_PKHpos_plot_final 

#Save as very high quality PNG @ 1000dpi
#good for publications
png("~/R_plots/Ratio_CD11b_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Ratio_CD11b_PKHpos_plot_final
dev.off()

#For mac
png("~/Desktop/R_Folder/R_plots/Ratio_CD11b_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Ratio_CD11b_PKHpos_plot_final
dev.off()


#Looking at F4.80+ population and calculating the ratio of cells that are PKH+
F4.80_cells <- sp%>%
  filter(Gate == "PKH-F4.80+" | Gate == "PKH+F4.80+") %>%
  group_by(Animal,Condition,Parameter)%>%
  mutate(Total_F4.80 = sum(Number))

PKH.F4.80 <- sp %>%                                      
  group_by(Animal,Condition,Parameter)%>%    
  select(-X.Total,-X.Gated)%>%                              
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
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), size=0.5,width=.25, position=position_dodge(.9)) +
  xlab("Treatment Condition (°C)") + 
  ylab(" % of Cells PKH+") +
  ggtitle("F4.80+ APC Splenocytes") + scale_y_continuous(expand=c(0,0),limits = c(0, 100)) 

#Finalize plot
Ratio_F4.80_PKHpos_plot_final <- Ratio_F4.80_PKHpos_plot + publication_style + scale_fill_brewer(palette="Greens")

#Test to see if it works
Ratio_F4.80_PKHpos_plot_final 

#Save as very high quality PNG @ 1000dpi
png("~/R_plots/Ratio_F4.80_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Ratio_F4.80_PKHpos_plot_final
dev.off()

#For mac
png("~/Desktop/R_Folder/R_plots/Ratio_F4.80_PKHpos_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 1000)
Ratio_F4.80_PKHpos_plot_final
dev.off()
