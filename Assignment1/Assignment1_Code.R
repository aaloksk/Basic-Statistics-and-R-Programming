#Clearing the variable work space
rm(list=ls()) 

#Setting working directory
setwd("C:\Users\Aalok\Dropbox\Github\Proabability2022\Assignment1\WD")

#Importing necessary libraries
library(corrplot)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(reshape2)
library(knitr)
library(tidyr)
library(corrplot)



################################################################################
##############PART 1 - DATA COMPILATION AND FORMATTING##########################
################################################################################

#Importing all text files
fname <- read.csv('fname.csv')


#Data frame for each variable of interest
Ox_N <- data.frame() #Oxides of Nitrogen
Ozone <- data.frame() #Ozone
WS <- data.frame() #Wind Speed
Res_WS <- data.frame() #Resultant Wind Spped
Res_WD <- data.frame() #Resultant Wind Direction
Temp_F <- data.frame() #Outdoor Temperature
DPT <- data.frame() #Dew Point Temperature
RH <- data.frame() #Relative humidity
SR <- data.frame() #Solar Radiation


#Arranging the data
for (x in 1:nrow(fname)) {
  
  #Importing data for a month
  mon <- read.csv(fname$fname[x])
  
  #Removing text fillers from the data
  if (fname$NOD[x]==28){
    data <- mon[-c(1:3,33:38,67:71,100:104,133:137,166:170,199:203,232:236,265:269),]
  } else if (fname$NOD[x]==30){
    data <- mon[-c(1:3,35:40,71:75,106:110,141:145,176:180,211:215,246:250,281:285),]
  } else if (fname$NOD[x]==31){
    data <- mon[-c(1:3,36:41,73:77,109:113,145:149,181:185,217:221,253:257,289:293),]
  }
  
  #Putting the data into data frame
  df <- data.table::fread(paste(data, collapse = "\n"))
  
  #Changing everything to character in the dataframe
  df <- df %>%
    mutate_all(as.character)
  
  #Converting the wide data into long data with time as new column
  df_long <- 
    df %>% 
    pivot_longer(cols = "00:00":"23:00", 
                 names_to = "Time", 
                 values_to = "Value")
  
  #Removing unnecessary columns
  df_long <- df_long[,-c(2:5)]
  
  #Checking the dataframe format as of now
  head (df_long,5)
  
  #Counting no of rows for each variable (Total 9 variables)
  n_var <- nrow(df_long) / 9
  
  #Extracting the values of each variable into respective dataframe
  
  #Oxide of Nitrogen
  start = 1
  row_var <- n_var
  Ox_N <- rbind(Ox_N, df_long[c(start:row_var),])

  #Ozone
  start = row_var+1
  row_var = row_var+n_var
  Ozone <- rbind(Ozone, df_long[c(start:row_var),])
  
  #Wind Spped
  start = row_var+1
  row_var = row_var+n_var
  WS <- rbind(WS, df_long[c(start:row_var),])
  
  #Resultant Wind Speed
  start = row_var+1
  row_var = row_var+n_var
  Res_WS <- rbind(Res_WS, df_long[c(start:row_var),])
  
  #Resultant Wind Direction
  start = row_var+1
  row_var = row_var+n_var
  Res_WD <- rbind(Res_WD, df_long[c(start:row_var),])
  
  #Outdoor Temperature
  start = row_var+1
  row_var = row_var+n_var
  Temp_F <- rbind(Temp_F, df_long[c(start:row_var),])
  
  #Dew Point Temperature
  start = row_var+1
  row_var = row_var+n_var
  DPT <- rbind(DPT, df_long[c(start:row_var),])
  
  #Relative Humidity
  start = row_var+1
  row_var = row_var+n_var
  RH <- rbind(RH, df_long[c(start:row_var),])
  
  #Solar Radiation
  start = row_var+1
  row_var = row_var+n_var
  SR <- rbind(SR, df_long[c(start:row_var),])
} 


#write.csv(Ox_N,"Oxide_Nitrogen.csv",row.names=FALSE)
#write.csv(Ozone,"Oxide_Nitrogen.csv",row.names=FALSE)
#write.csv(Res_WS,"Oxide_Nitrogen.csv",row.names=FALSE)
#write.csv(Res_WD,"Oxide_Nitrogen.csv",row.names=FALSE)
#write.csv(Temp_F,"Oxide_Nitrogen.csv",row.names=FALSE)
#write.csv(DPT,"Oxide_Nitrogen.csv",row.names=FALSE)
#write.csv(RH,"Oxide_Nitrogen.csv",row.names=FALSE)
#write.csv(SR,"Oxide_Nitrogen.csv",row.names=FALSE)

#Calculating summary statistics for Oxide of Nitrogen
Ox_N$Value <- as.numeric(Ox_N$Value) #Converting the values to numerical format
Ox_N2 <- na.omit(Ox_N) #Crating new df omitting the NAs
df_OxN <- Ox_N2 %>% group_by(Date) %>% summarise(Min=min(Value),Mean = mean(Value), Max=max(Value)) #Using dplyr to calculate summary stats
colnames(df_OxN) <- c("Date", "Ox_N_Min", "Ox_N_Avg", "Ox_N_Max") #Specifying column names

#Calculating summary statistics for Ozone
Ozone$Value <- as.numeric(Ozone$Value) #Converting the values to numerical format
Ozone2 <- na.omit(Ozone) #Crating new df omitting the NAs
df_Ozone <- Ozone2 %>% group_by(Date) %>% summarise(Min=min(Value),Mean = mean(Value), Max=max(Value)) #Using dplyr to calculate summary stats
colnames(df_Ozone) <- c("Date", "Ozone_Min", "Ozone_Avg", "Ozone_Max") #Specifying column names

#Calculating summary statistics for Resultant Wind Speed
Res_WS$Value <- as.numeric(Res_WS$Value) #Converting the values to numerical format
Res_WS2 <- na.omit(Res_WS) #Crating new df omitting the NAs
df_Res_WS <- Res_WS2 %>% group_by(Date) %>% summarise(Min=min(Value),Mean = mean(Value), Max=max(Value)) #Using dplyr to calculate summary stats
colnames(df_Res_WS) <- c("Date", "Res_WS_Min", "Res_WS_Avg", "Res_WS_Max") #Specifying column names

#Calculating summary statistics for Resultant Wind Direction
Res_WD$Value <- as.numeric(Res_WD$Value) #Converting the values to numerical format
Res_WD2 <- na.omit(Res_WD) #Crating new df omitting the NAs
df_Res_WD <- Res_WD2 %>% group_by(Date) %>% summarise(Min=min(Value),Mean = mean(Value), Max=max(Value)) #Using dplyr to calculate summary stats
colnames(df_Res_WD) <- c("Date", "Res_WD_Min", "Res_WD_Avg", "Res_WD_Max") #Specifying column names

#Calculating summary statistics for Outdoor Temperature
Temp_F$Value <- as.numeric(Temp_F$Value) #Converting the values to numerical format
Temp_F2 <- na.omit(Temp_F) #Crating new df omitting the NAs
df_Temp_F <- Temp_F2 %>% group_by(Date) %>% summarise(Min=min(Value),Mean = mean(Value), Max=max(Value)) #Using dplyr to calculate summary stats
colnames(df_Temp_F) <- c("Date", "Temp_F_Min", "Temp_F_Avg", "Temp_F_Max") #Specifying column names

#Calculating summary statistics for Oxide of Nitrogen
DPT$Value <- as.numeric(DPT$Value) #Converting the values to numerical format
DPT2 <- na.omit(DPT) #Crating new df omitting the NAs
df_DPT <- DPT2 %>% group_by(Date) %>% summarise(Min=min(Value),Mean = mean(Value), Max=max(Value)) #Using dplyr to calculate summary stats
colnames(df_DPT) <- c("Date", "DPT_Min", "DPT_Avg", "DPT_Max") #Specifying column names

#Calculating summary statistics for Oxide of Nitrogen
RH$Value <- as.numeric(RH$Value) #Converting the values to numerical format
RH2 <- na.omit(RH) #Crating new df omitting the NAs
df_RH <- RH2 %>% group_by(Date) %>% summarise(Min=min(Value),Mean = mean(Value), Max=max(Value)) #Using dplyr to calculate summary stats
colnames(df_RH) <- c("Date", "RH_Min", "RH_Avg", "RH_Max") #Specifying column names

#Calculating summary statistics for Oxide of Nitrogen
SR$Value <- as.numeric(SR$Value) #Converting the values to numerical format
SR2 <- na.omit(SR) #Crating new df omitting the NAs
df_SR <- SR2 %>% group_by(Date) %>% summarise(Min=min(Value),Mean = mean(Value), Max=max(Value)) #Using dplyr to calculate summary stats
colnames(df_SR) <- c("Date", "SR_Min", "SR_Avg", "SR_Max") #Specifying column names

#Creating daily data
dates <- data.frame(seq(as.Date('2020-10-01'), as.Date('2021-09-30'), by = 'days'))
colnames(dates) <- c("Date")

#Format the date in similar fashion as our earlier dataframes
dates$Date <- format(as.Date(dates$Date), "%m/%d/%Y")

#Merge the dates dataframe with Ox_N dataframe
#zz <- left_join(dates,df_OxN, by="Date")

#Making a list of all datafrtame to merge
df_list = list(dates, df_OxN, df_Ozone, df_Res_WS, df_Res_WD, df_Temp_F, df_DPT, df_RH, df_SR)

#Merge the dates dataframe with all the dataframe with values 
daily_df <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)


################################################################################
###################PART 2 - DATA ANALYSIS AND PROBLEMS##########################
################################################################################


#################################################################
#Problem 1
#################################################################

#Plotting empirical cumulative distribution function
#CDF <- ecdf(daily_df$Ozone_Max)
#plot( CDF )

#Plotting position function taking data and values of a,b
plot_pos <- function(data,a,b){
  n <- length(data)
  Qp <- sort(data)
  Qr <- rank(data)
  fx <- (rank(Qp-a))/(n+b)
  df <- cbind(Qp, fx)
  return(df)
}

#Defining a and bor Beard Plotting Position Formula
a <- 0.31
b <- 0.38

#using the function to get probabilities
F_Oz <- plot_pos(daily_df$Ozone_Max,a,b)

#Plotting ECDF
plot(F_Oz, xlab="Maximum Daily Ozone", ylab="Probability", pch=16)
title("Emperical CDF of Maximum Daily Ozone (Beard PP Formula)")
grid(col='gray', lty =2)

#################################################################
#Problem 2
#################################################################

#Extracting mean daily temperature
x <- na.omit(daily_df$Temp_F_Avg)

#Calculating length, IQR and range of data
n=length(x)
IQR=IQR(x)
range = max(x)-min(x)

#Freedman-Diaconis Binning Formula
bins = (n**(1/3) * range)/(2 * IQR)

#FD binning directly through package
#require("grDevices")
#bins = nclass.FD(x)

#Plotting the histogram
hist(daily_df$Ozone_Max,
     main="Mean daily temperature",
     xlab="Temperature in degrees Fahrenheit",
     col="chocolate",
     border="brown",
     breaks=ceiling(bins))

#################################################################
#Problem 3
#################################################################

#Creating a function to calculate multiple percentiles
percentile_func <- function(data) {
  res = quantile(data, probs = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99), type =2)
  return(res)
}

#Extracting the values for Max Oxides of Nitrogen
x3 = na.omit(daily_df$Ox_N_Max)
#Using the function to calculate percentiles of Maximum Oxide of Nitrogen
percentile_func(x3)


#################################################################
#Problem 4
#################################################################

#Extracting all necessary variables
corr_data <- subset(daily_df, select=c("Ozone_Max", "DPT_Min","SR_Max","Res_WS_Max", "Ox_N_Max"))

#Removing NAs
corr_data2 <- na.omit(corr_data)
colnames(corr_data2) <- c("Max_Ozone", "Min_DPT", "Max_SR", "Max_RS_WindSpeed", "Max_OxideConc")

#Calculating correlation and saving as matrix
M <-cor(corr_data2, method = 'spearman')

corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"), tl.col="Black", tl.srt=35, addCoef.col = "black")


#################################################################
#Problem 5 using hourly data
#################################################################

#Extracting hourly data
df_hourly <- left_join(Ozone, Res_WD, by=c("Date","Time"))
colnames(df_hourly) <- c("Date","Time","Ozone_h", "Res_WD_h")

#Reclassification
df_hourly$Ozone <- cut(df_hourly$Ozone_h, c(-Inf,10,Inf), c("low", "high"))
df_hourly$WD <- cut(df_hourly$Res_WD_h, c(0,90,180,270,360), c("NE", "SE", "SW", "NW"))

#Removing NAs
df_hourly <- na.omit(df_hourly)

data5 <- df_hourly

#Contingency Table
#In termas of count
count_table <- table(data5$Ozone,data5$WD)
count_table

#In terms of probability
prob_table <- prop.table(count_table)
prob_table

#Converting the table of probabilities to df
prob_df <- as.data.frame.matrix(prob_table) 

#Column Sums
cont_table <- rbind(prob_df, colSums(prob_df))
rownames(cont_table) <- c("Less than 10", "Greater or Equal to 10","Total")

#Row Sums
cont_table <- cbind(cont_table, rowSums(cont_table))
colnames(cont_table) <- c("0-90 Quad", "90-180 Quad", "180-270 Quad","270-360 Quad","Total")

#Final Contingency Table
cont_table

#Joint Probability
joint_prob <- prob_df['high','NE']

#Marginal Probability
marg_prob <- sum(prob_df$NE)

#Conditional Probability
cond_prob <- joint_prob / marg_prob
cond_prob