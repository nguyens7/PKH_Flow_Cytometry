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
library(RColorBrewer)
library(reshape2)

#Function to convert a factor into a numeric
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#Import group1.csv
data <- "PKH_Splenocytes_081116.csv"
my_data <- read.csv(data, sep = ",", header = TRUE)

#Remove columns "Data_Set", "X_Parameter", "Y_Parameter" as they aren't necessary for analysis
my_data$Data_Set <- NULL
my_data$X_Parameter <- NULL
my_data$Y_Parameter <- NULL

#Tell R to ignore missing data, it will ignore the control tubes (FMO,Single stain, IgG Cocktai,Auto, etc....)
sp <- na.omit(my_data)

#Make sure you imported the CSV file
summary(sp)

#Look at the headers for the data
head(sp)
tail(sp)

################ GUIDE TO MAKING A PLOT WITH FLOW DATA ########################
#  1) Filter on the parameter that you want
#  2) Calculate the mean of your technical replicates
#  3) Rename the "mean" column to average so you can calculate biological replicates if any
#  4) Calculate the mean of your biological replicates
#  5) Graph your data
#  6) Adjust your plot to your liking



######## MAKING A PLOT FOR CELLS THAT ARE PKH POSITIVE##########################


#Filter for PKH data within sp
PKH <- sp[sp$Gate %in% c("PKH_Pos"),]

#Summarize data (Average the technical replicates)
PKH_data <- ddply(PKH, c("Animal","Condition","Parameter","Gate"), summarise,
                 N    = length(Gated),
                 mean = mean(Gated),
                 sd   = sd(Gated),
                 se   = sd / sqrt(N))

#Rename "mean" column in sp_data to "average" to avoid confusion
PKH_data <- rename(PKH_data, average = mean)

#Calculate the Average value of three animals, N=3
Avg_PKH_data <- ddply(PKH_data, c("Condition","Parameter"), summarise,
                      N    = length(average),
                      mean = mean(average),
                      sd   = sd(average),
                      se   = sd / sqrt(N))

#Plot PKH data
PKH_plot <- ggplot(Avg_PKH_data, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                size=0.5,   #Size of the error bars
                width=.25,  # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") +  # X axis label
  ylab("Percentage of PKH67+ Cells") +  # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) + 
  ggtitle("") + scale_y_continuous(expand=c(0,0),limits = c(0, 60)) # Plot title, make graph sit on x axis, define y axis values

#Make a default theme for optimizeing graph aesthetics
publication_style <- theme(axis.line.x=element_line(color="black",size=1.0),    #Make X axis size 1.0 and black
                           axis.line.y=element_line(color="black",size=1.0),     #Make Y axis size 1.0 and black
                           panel.background=element_rect(fill=NA,size=rel(20)), #Remove grey background
                           panel.grid.minor=element_line(colour=NA),            #Remove grid lines
                           axis.text=element_text(size=18,colour="black"),      #Make axis text black and size 18
                           axis.title=element_text(size=20,face="bold"),legend.text=element_text(size=20), # Make Title axis bold and size 20
                           plot.title = element_text(face="bold", size=30))     #Make plot title bold and size 30

#Test to see if PKH Plot works with publication_style
PKH_plot + publication_style

#Finalize plot
PKH_final <- PKH_plot + publication_style 


#Test to see if it works
PKH_final

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/PKH_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
PKH_final
dev.off()


#######PLOTTING PKH+ MHCII+ Cells################################

#Filter within the Gate Column for PKH_pos
PKH_MHCII <- sp[sp$Gate %in% c("PKH+MHCII+"),]

#Summarize data (Average the technical replicates)
PKH_MHCII_data <- ddply(PKH_MHCII, c("Animal","Condition","Parameter","Gate"), summarise,
                  N    = length(Gated),
                  mean = mean(Gated),
                  sd   = sd(Gated),
                  se   = sd / sqrt(N))

#Rename "mean" column in sp_data to "average" to avoid confusion
PKH_MHCII_data <- rename(PKH_MHCII_data, average = mean)

#Calculate the Average value of three animals, N=3
Avg_PKH_MHCII_data <- ddply(PKH_MHCII_data, c("Condition","Parameter"), summarise,
                      N    = length(average),
                      mean = mean(average),
                      sd   = sd(average),
                      se   = sd / sqrt(N))


#Plot PKH data

PKH_MHCII_plot <- ggplot(Avg_PKH_MHCII_data, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                size=0.5,   #Size of the error bars
                width=.25,  # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") +  # X axis label
  ylab("Percentage of PKH67+ MHC II+ Cells") +  # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) + 
  ggtitle("") + scale_y_continuous(expand=c(0,0),limits = c(0, 60)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
PKH_MHCII_final <- PKH_MHCII_plot + publication_style

#Test to see if it works
PKH_MHCII_final  

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/PKH_MHCII_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
PKH_MHCII_final
dev.off()


####################### Plotting PKH67+ Cells of All MHCII+ Cells
#*****************************************************************************
#*************************************************************************************

#Summarize data for MHCII + cells
sp_MHCII <- sp[sp$Gate %in% c("PKH+MHCII+","PKH-MHCII+"), ]

#Calculate average total number of cells for each technical replicate
sp_MHCII_total <- ddply(sp_MHCII, c("Animal","Condition","Parameter","Gate"), summarise,
      N    = length(Gated),
      mean = mean(Gated),
      sd   = sd(Gated),
      se   = sd / sqrt(N))

sp_MHCII_total <- rename(sp_MHCII_total, average = mean)

MHCII_total <- ddply(sp_MHCII_total, c("Condition","Parameter"), summarise,
                     N    = length(average),
                     mean = mean(average),
                     sd   = sd(average),
                     se   = sd / sqrt(N))


#Filter within the Gate Column for PKH_pos
PKH_MHCII_only <- sp[sp$Gate %in% c("PKH+MHCII+","PKH-MHCII+"), ]


PKH_MHCII_only <- ddply(PKH_MHCII_only, c("Animal","Condition","Parameter","Gate"), summarise,
                        N    = length(Gated),
                        mean = mean(Gated),
                        sd   = sd(Gated),
                        se   = sd / sqrt(N))

#Rename "mean" column in sp_data to "average" to avoid confusion
PKH_MHCII_only <- rename(PKH_MHCII_only, average = mean)

#Calculate the Average value of three animals, N=3
Avg_PKH_MHCII_only <- ddply(PKH_MHCII_only, c("Condition","Parameter","Gate"), summarise,
                            N    = length(average),
                            mean = mean(average),
                            sd   = sd(average),
                            se   = sd / sqrt(N))


#Plot PKH data

PKH_MHCII_only_plot <- ggplot(Avg_PKH_MHCII_only, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                size=0.5,   #Size of the error bars
                width=.25,  # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") +  # X axis label
  ylab("MHC II+ Cells with PKH67 Exo (%)") +  # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) + 
  ggtitle("") + scale_y_continuous(expand=c(0,0),limits = c(0, 60)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
PKH_MHCII_only_final <- PKH_MHCII_only_plot + publication_style

#Test to see if it works
PKH_MHCII_Only_final  

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/PKH_MHCII_only_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
PKH_MHCII_final
dev.off()



########################  Create graph of PKH+ F4.80+ cells####################################


#Filter within the Gate Column for PKH_pos
PKH_F4.80 <- sp[sp$Gate %in% c("PKH+F4.80+"),]

#Summarize data (Average the technical replicates)
PKH_F4.80_data <- ddply(PKH_MHCII, c("Animal","Condition","Parameter","Gate"), summarise,
                        N    = length(Gated),
                        mean = mean(Gated),
                        sd   = sd(Gated),
                        se   = sd / sqrt(N))

#Rename "mean" column in sp_data to "average" to avoid confusion
PKH_F4.80_data <- rename(PKH_F4.80_data, average = mean)

#Calculate the Average value of three animals, N=3
Avg_PKH_F4.80_data <- ddply(PKH_F4.80_data, c("Condition","Parameter"), summarise,
                            N    = length(average),
                            mean = mean(average),
                            sd   = sd(average),
                            se   = sd / sqrt(N))


#Plot PKH data

PKH_F4.80_plot <- ggplot(Avg_PKH_MHCII_data, aes(x=Condition, y=mean, fill=Parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), 
                size=0.5,   #Size of the error bars
                width=.25,  # Width of the error bars
                position=position_dodge(.9)) + #Where to put the errorbars
  xlab("Treatment Condition") +  # X axis label
  ylab("Percentage of PKH67+ F4/80+ Cells") +  # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) + 
  ggtitle("") + scale_y_continuous(expand=c(0,0),limits = c(0, 60)) # Plot title, make graph sit on x axis, define y axis values

#Finalize plot
PKH_F4.80_final <- PKH_F4.80_plot + publication_style

#Test to see if it works
PKH_F4.80_final  

#Save as very high quality PNG @ 600dpi
#good for publications
png("~/R_plots/PKH_F4.80_graph_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
PKH_MHCII_final
dev.off()










# ***********************************FOR BAR GRAPHS***************
# Make day column as factor
# This effectlively tells R to treat your data as it and DON'T alphabetize it
# You tell R which column you want it to read as a factor (leave it in the order that you entered the data)
b_tclc <- tclc
b_tclc$day <- factor(b_tclc$day, levels=c("0","5","10","14","17","19"))  #Adjust the day column and make it in the correct order

## Test to see if parameter needs to be factored uniquely*****
b_tclc$parameter <- factor(tclc$parameter, levels=unique(tclc$parameter))

#Verify the factor is in the correct order
str(b_tclc)


# Make the bar graph data easy to recall by renaming it "b"
b <- b_tclc
#Summarize the data
b_data <- ddply(b, c("day","parameter"), summarise,
                N    = length(value),
                mean = mean(value),
                sd   = sd(value),
                se   = sd / sqrt(N))


#check if data is truly summarized
b_data

#Plot all the data
tc_plot <- ggplot(b_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Day of Gestation") +  # X axis label
  ylab("Mouse parameter (in g or #)") +  # Y axis label
  ## OPTIONAL ##scale_fill_hue(name="", breaks=c(""), labels=c("")) + 
  ggtitle("") + scale_y_continuous(expand=c(0,0)) # Plot title


# Check if plot worked
tc_plot


#Make a default theme for optimizeing graph aesthetics
publication_style <- theme(axis.line.x=element_line(color="black",size=1.0),    #Make X axis size 1.0 and black
                           axis.line.y=element_line(color="black",size=1.0),     #Make Y axis size 1.0 and black
                           panel.background=element_rect(fill=NA,size=rel(20)), #Remove grey background
                           panel.grid.minor=element_line(colour=NA),            #Remove grid lines
                           axis.text=element_text(size=18,colour="black"),      #Make axis text black and size 18
                           axis.title=element_text(size=20,face="bold"),legend.text=element_text(size=20), # Make Title axis bold and size 20
                           plot.title = element_text(face="bold", size=30))     #Make plot title bold and size 30

#fix axes
tc_plot_final <- tc_plot + publication_style

#Check the final plot
tc_plot_final



#Save as very high quality PNG @ 600dpi
#good for publications
png("~/Desktop/R_Folder/tc_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
tc_plot_final
dev.off()

########################################################################

#Filter weight
weight_data <- b_data[b_data$parameter %in% c("weight"),]

summary(b_data)
summary(weight_data)

#plot weight data
weight_plot <- ggplot(weight_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Day of Gestation") +  # X axis label
  ylab("Mean Weight (g)") +  # Y axis label
  scale_fill_hue(name="", breaks=c(""), labels=c("")) + 
  ggtitle("") + scale_y_continuous(expand=c(0,0)) # Plot title

#Check if weight plot worked
weight_plot

#fix axes (using publication style)
tc_weight_final <- weight_plot + publication_style

#Check the final plot
tc_weight_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_Weight_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
tc_weight_final
dev.off()

######################################
#Test for line graph generation ######

l_weight_data <- l_data[l_data$parameter %in% c("weight"),]

weight_plot_line <- ggplot(l_weight_data, aes(x=day, y=mean,  group=1, colour=parameter)) +
  geom_line(size=2, colour="black") + geom_point(size=3, colour="black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.8,  # Width of the error bars
                size=1.5,   # Size of the error bars
                colour="black", #color of the error bars
                position=position_dodge(.9)) +
  theme(legend.position="none") + #remove legend
  xlab("Day of Gestation") +  # X axis label
  ylab("Mean Weight (g)") +  # Y axis label  
  ggtitle("") + scale_y_continuous(expand=c(0,0)) # Plot title


#test is the line graph worked
weight_plot_line

#Format axes correctly using (publication_style)
weight_plot_line + publication_style

#Make finalize line graph 
weight_line_final <- weight_plot_line + publication_style + scale_y_continuous(limits=c(15, 35)) #Adjust y axis

#Check to see if it worked
weight_line_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/line_tc_Weight_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
weight_line_final
dev.off()



###########################################################################


#Filter pups
pup_data <- b_data[b_data$parameter %in% c("pups"),]

summary(b_data)
summary(pup_data)

#plot weight data
pup_plot <- ggplot(pup_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Days of Gestation") +  # X axis label
  ylab("Number of Pups") +  # Y axis label
  scale_fill_hue(name="", breaks=c(""), labels=c("")) + 
  ggtitle("") + scale_y_continuous(expand=c(0,0)) # Plot title

#Check if weight plot worked
pup_plot

#fix axes
tc_pup_final <- pup_plot + publication_style

#Check the final plot
tc_pup_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_pup_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
tc_pup_final
dev.off()


########################################################################################

#Calculation of Pup Implantation Within Uterine Horns
horn_data <- b_data[b_data$parameter %in% c("left.horn","right.horn"),]

#Check if horn data works
summary(horn_data)
head(horn_data)

#plot weight data
horn_plot <- ggplot(horn_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Implantation Sites") +  # X axis label
  ylab("Number of Pups") +  # Y axis label
  #scale_fill_hue(name="Uterine Horn", breaks=c("Left Horn", "Right Horn"), labels=c("Left Horn", "Right Horn")) + 
  ggtitle("") + scale_y_continuous(expand=c(0,0)) # Plot title

#Check if weight plot worked
horn_plot

#fix axes
tc_horn_final <- horn_plot + publication_style

#Check the final plot
tc_horn_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_horn_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
tc_horn_final
dev.off()


##########################################################################

#Calculation of mean placenta weight throughout gestation

#Calculation of Pup Implantation Within Uterine Horns
plac_data <- b_data[b_data$parameter %in% c("placenta.1","placenta.2","placenta.3"),]

#Check if placenta data works
summary(b_data)
head(plac_data)

#plot placenta weight data
plac_plot <- ggplot(plac_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Day of Gestation") +  # X axis label
  ylab("Mean Placenta Weight (g)") +  # Y axis label
  #scale_fill_hue(name="", breaks=c("", ""), labels=c("","")) + 
  ggtitle("") + scale_y_continuous(expand=c(0,0)) # Plot title


#Check if weight plot worked
plac_plot

#fix axes
plac_final <- plac_plot + publication_style

#Check the final plot
plac_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_plac_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
plac_final
dev.off()

##########################################################################################

#Calculation of finalized mean placenta weight throughout gestation

#Calculation of Pup Implantation Within Uterine Horns
mean_plac_data <- b_data[b_data$parameter %in% c("plac.avg"),]

#Check if placenta data works
summary(mean_plac_data)
head(mean_plac_data)

#plot placenta weight data
mean_plac_plot <- ggplot(mean_plac_data, aes(x=day, y=mean, fill=parameter)) +
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.25,                    # Width of the error bars
                position=position_dodge(.9)) +
  xlab("Day of Gestation") +  # X axis label
  ylab("Mean Placenta Weight (g)") +  # Y axis label
  scale_fill_hue(name="", breaks=c("", ""), labels=c("","")) + 
  ggtitle("") + scale_y_continuous(expand=c(0,0)) # Plot title

#Check if weight plot worked
mean_plac_plot

#fix axes
mean_plac_final <- mean_plac_plot + publication_style

#Check the final plot
mean_plac_final

#Save as very high quality PNG @ 300dpi
#good for publications
png("~/Desktop/R_Folder/tc_mean_plac_hi_res.png", width = 7, height = 5, units = 'in', res = 600)
mean_plac_final
dev.off()



###########################################################################################################################


# Experimental section
#annotate
##annotate("text", x = 4, y = 25, label = "Some text")
