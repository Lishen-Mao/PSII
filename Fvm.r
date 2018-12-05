#load packages
#library(gdata)
#library(data.table)
library(lubridate)
library(tidyr)
library (psych)
library (tidyverse)
library (scales)
library(dplyr)
library(ggplot2)

#Load dataset
#set strings as factors to false
#setwd("C:/Users/Lishen-PC/Desktop/PAM/JuncusPAM/")
#tbl_df(PAM0301_0528)
#tbl_df(EddyPAR)
options(stringsAsFactors = FALSE)
MayPAM <- read.csv("C:/Users/lishe/OneDrive - University of Georgia/Analysis/Fv_Fm/PSII/Data/GB/Juncus0507_0528.csv")
MayBiomet <- read.csv("C:/Users/lishe/OneDrive - University of Georgia/Analysis/Fv_Fm/PSII/Data/GB/GrandBay_biomet_201800507_20180528.csv")

#Working with Flux data
Biomet <- MayBiomet %>%
  select ("datetime", "Date", "Time", "Tair", "PAR_IN", "Tide_level", "tide_relative_soil")
Biomet$datetime <- as.POSIXct(strptime(Biomet$datetime, tz='Etc/GMT+6', format= "%m/%d/%Y %H:%M")) 

#working with sensor head at 50cm.
#Remove zero or empty PAR cell of 50 cm height sensor: CFML0176C blue
FiftyCM_PAM <- MayPAM %>%
  filter(X5.PAR >0 & X5.PAR<9999)
FiftyCM_PAM$datetime <- mdy_hms(paste(FiftyCM_PAM$Date, FiftyCM_PAM$Time))
FiftyCM_PAM$datetime <- round_date(FiftyCM_PAM$datetime, '15 minutes')
FiftyCM_PAM$datetime <- as.POSIXct(strptime(FiftyCM_PAM$datetime, tz='Etc/GMT+6', format = '%Y-%m-%d %H:%M'))
#move datetime column to first in cleaned PAM data
FiftyCM_PAM <- FiftyCM_PAM %>%
  select(datetime, everything())
#Join two dataframes, remove extreme PAM PAR and calculate Fv/Fm
FiftyCM_PAM_Join <- left_join(FiftyCM_PAM,Biomet, by="datetime")%>%
  filter(X5.PAR<PAR_IN*1.3)%>%
  mutate(X5.Fv_Fm = (X5.Fm. - X5.F)/X5.Fm.)
#plot Fv/Fm vs PAR in order to find plant under stree in different environmental conditions
#plot Fv/Fm
FiftyCM_PAM_Join %>%
  ggplot(aes(x = datetime,y = X5.Fv_Fm,group=1)) +
  geom_line(color="blue")+
  labs(title = "Juncas May Fv/Fm at 50cm",
      y = "Maximum PSII Efficiency",
      x = "Date") + theme_bw(base_size = 15) +
  facet_wrap(~ Date.x, ncol=4, scales="free")
#plot Flux PAR and PAM PAR
FiftyCM_PAM_Join %>%
  ggplot(aes(x = datetime,y = PAR_IN,group=1)) +
  geom_line(color="firebrick1")+
  geom_line(aes(x = datetime, y = X5.PAR,group=1), color = "seagreen") +
  labs(title = "Juncas May PAR at 50cm",
       y = "Absorbed PAR",
       x = "Date") + theme_bw(base_size = 15) +
  facet_wrap(~ Date.x, ncol=4, scales="free")


#Align multiple ggplot2 graphs with a common x axis and different y axes, each with different y-axis labels.
plot1 <- FiftyCM_PAM_Join %>%
  ggplot() +
  geom_line(aes(x = datetime, y = X5.Fv_Fm), size = 0.5, alpha = 0.75, color="firebrick1") +
  ylab("Fv/Fm rate") +
  theme_minimal() +
  theme(axis.title.x = element_blank())
plot2 <- FiftyCM_PAM_Join %>%
  ggplot() +
  geom_line(aes(x = datetime, y = PAR_IN), size = 0.5, alpha = 0.75, color = "blue") +
  ylab("PAR") +
  theme_minimal() +
  theme(axis.title.x = element_blank())
grid.newpage()
grid.draw(rbind(ggplotGrob(plot1), ggplotGrob(plot2), size = "last"))

#working with sensor head at 28cm.
#Remove zero or empty PAR cell of 50 cm height sensor: CFML0175C Pink
TwentyeightCM_PAM <- MayPAM %>%
  filter(X4.PAR >0 & X4.PAR<9999)
TwentyeightCM_PAM$X4.F = as.numeric(TwentyeightCM_PAM$X4.F)
TwentyeightCM_PAM$X4.Fm. = as.numeric(TwentyeightCM_PAM$X4.Fm.)
TwentyeightCM_PAM$X4.PAR  = as.numeric(TwentyeightCM_PAM$X4.PAR)
TwentyeightCM_PAM$datetime <- mdy_hms(paste(TwentyeightCM_PAM$Date, TwentyeightCM_PAM$Time))
TwentyeightCM_PAM$datetime <- round_date(TwentyeightCM_PAM$datetime, '15 minutes')
TwentyeightCM_PAM$datetime <- as.POSIXct(strptime(TwentyeightCM_PAM$datetime, tz='Etc/GMT+6', format = '%Y-%m-%d %H:%M'))
#move datetime column to first in cleaned PAM data
TwentyeightCM_PAM <- TwentyeightCM_PAM %>%
  select(datetime, everything())
#Join two dataframes, remove extreme PAM PAR and calculate Fv/Fm
TwentyeightCM_PAM_Join <- left_join(TwentyeightCM_PAM,Biomet, by="datetime")%>%
  filter(X4.PAR<PAR_IN*1.3)%>%
  mutate(X4.Fv_Fm = (X4.Fm. - X4.F)/X4.Fm.)
#plot Fv/Fm vs PAR in order to find plant under stree in different environmental conditions
#plot Fv/Fm
TwentyeightCM_PAM_Join %>%
  ggplot(aes(x = datetime,y = X4.Fv_Fm,group=1)) +
  geom_line(color="blue")+
  labs(title = "Juncas May Fv/Fm at 28cm",
       y = "Maximum PSII Efficiency",
       x = "Date") + theme_bw(base_size = 15) +
  facet_wrap(~ Date.x, ncol=4, scales="free")
#plot Flux PAR and PAM PAR
TwentyeightCM_PAM_Join %>%
  ggplot(aes(x = datetime,y = PAR_IN,group=1)) +
  geom_line(color="firebrick1")+
  geom_line(aes(x = datetime, y = X4.PAR,group=1), color = "seagreen") +
  labs(title = "Juncas May PAR at 28cm",
       y = "Absorbed PAR",
       x = "Date") + theme_bw(base_size = 15) +
  facet_wrap(~ Date.x, ncol=4, scales="free")

#working with sensor head at 10cm.
#Remove zero or empty PAR cell of 10 cm height sensor: CFML0177C Orange
TenCM_PAM <- MayPAM %>%
  filter(X6.PAR >0 & X6.PAR<9999)
TenCM_PAM$datetime <- mdy_hms(paste(TenCM_PAM$Date, TenCM_PAM$Time))
TenCM_PAM$datetime <- round_date(TenCM_PAM$datetime, '15 minutes')
TenCM_PAM$datetime <- as.POSIXct(strptime(TenCM_PAM$datetime, tz='Etc/GMT+6', format = '%Y-%m-%d %H:%M'))
#move datetime column to first in cleaned PAM data
TenCM_PAM <- TenCM_PAM %>%
  select(datetime, everything())
#Join two dataframes, remove extreme PAM PAR and calculate Fv/Fm
TenCM_PAM_Join <- left_join(TenCM_PAM,Biomet, by="datetime")%>%
  filter(X6.PAR<PAR_IN*1.3)%>%
  mutate(X6.Fv_Fm = (X6.Fm. - X6.F)/X6.Fm.)
#plot Fv/Fm vs PAR in order to find plant under stree in different environmental conditions
#plot Fv/Fm
TenCM_PAM_Join %>%
  ggplot(aes(x = datetime,y = X6.Fv_Fm,group=1)) +
  geom_line(color="blue")+
  labs(title = "Juncas May Fv/Fm at 10cm",
       y = "Maximum PSII Efficiency",
       x = "Date") + theme_bw(base_size = 15) +
  facet_wrap(~ Date.x, ncol=4, scales="free")
#plot Flux PAR and PAM PAR
TenCM_PAM_Join %>%
  ggplot(aes(x = datetime,y = PAR_IN,group=1)) +
  geom_line(color="firebrick1")+
  geom_line(aes(x = datetime, y = X6.PAR,group=1), color = "seagreen") +
  labs(title = "Juncas May PAR at 10cm",
       y = "Absorbed PAR",
       x = "Date") + theme_bw(base_size = 15) +
  facet_wrap(~ Date.x, ncol=4, scales="free")

#working with sensor head at 82cm.
#Remove zero or empty PAR cell of 82 cm height sensor: CFML0174C Green
EightyCM_PAM <- MayPAM %>%
  filter(X3.PAR >0 & X3.PAR<9999)
EightyCM_PAM$X3.F = as.numeric(EightyCM_PAM$X3.F)
EightyCM_PAM$X3.Fm. = as.numeric(EightyCM_PAM$X3.Fm.)
EightyCM_PAM$X3.PAR  = as.numeric(EightyCM_PAM$X3.PAR)
EightyCM_PAM$datetime <- mdy_hms(paste(EightyCM_PAM$Date, EightyCM_PAM$Time))
EightyCM_PAM$datetime <- round_date(EightyCM_PAM$datetime, '15 minutes')
EightyCM_PAM$datetime <- as.POSIXct(strptime(EightyCM_PAM$datetime, tz='Etc/GMT+6', format = '%Y-%m-%d %H:%M'))
#move datetime column to first in cleaned PAM data
EightyCM_PAM <- EightyCM_PAM %>%
  select(datetime, everything())
#Join two dataframes, remove extreme PAM PAR and calculate Fv/Fm
EightyCM_PAM_Join <- left_join(EightyCM_PAM,Biomet, by="datetime")%>%
  filter(X3.PAR<PAR_IN*1.3)%>%
  mutate(X3.Fv_Fm = (X3.Fm. - X3.F)/X3.Fm.)
#plot Fv/Fm vs PAR in order to find plant under stree in different environmental conditions
#plot Fv/Fm
EightyCM_PAM_Join %>%
  ggplot(aes(x = datetime,y = X3.Fv_Fm,group=1)) +
  geom_line(color="blue")+
  labs(title = "Juncas May Fv/Fm at 82cm",
       y = "Maximum PSII Efficiency",
       x = "Date") + theme_bw(base_size = 15) +
  facet_wrap(~ Date.x, ncol=4, scales="free")
#plot Flux PAR and PAM PAR
EightyCM_PAM_Join %>%
  ggplot(aes(x = datetime,y = PAR_IN,group=1)) +
  geom_line(color="firebrick1")+
  geom_line(aes(x = datetime, y = X3.PAR,group=1), color = "seagreen") +
  labs(title = "Juncas May PAR at 82cm",
       y = "Absorbed PAR",
       x = "Date") + theme_bw(base_size = 15) +
  facet_wrap(~ Date.x, ncol=4, scales="free")

#Sunnyday 50cm
require(ggcorrplot)
Sunnyday50cm <- subset(FiftyCM_PAM_Join, Date.x == as.Date("5/11/2018"))
Sunnyday50cm$X5.ETR = as.numeric(Sunnyday50cm$X5.ETR)
Sunnyday50cm$X5.Y..II. = as.numeric(Sunnyday50cm$X5.Y..II.)
Sunnyday50cm_variables <- Sunnyday50cm%>%
  select(X5.PAR,X5.Fv_Fm,Tair,X5.ETR)
correlation_matrix <- cor(Sunnyday50cm_variables, method = "spearman")
ggcorrplot(correlation_matrix,
           lab=TRUE, #Include numbers
           type = "lower", 
           insig = "blank")
ggplot(Sunnyday50cm, aes(x=X5.Fv_Fm, y=X5.ETR)) +
  labs(title="50cm height sensor's ETR vs Fv/Fm")+
  geom_point()
#Sunnyday 28cm
Sunnyday28cm <- subset(TwentyeightCM_PAM_Join, Date.x == as.Date("5/11/2018"))
Sunnyday28cm_variables <- Sunnyday28cm%>%
  select(X4.PAR,X4.Fv_Fm,Tair)
correlation_matrix <- cor(Sunnyday28cm_variables, method = "spearman")
ggcorrplot(correlation_matrix,
           lab=TRUE, #Include numbers
           type = "lower", 
           insig = "blank")
Sunnyday28cm$X4.ETR = as.numeric(Sunnyday28cm$X4.ETR)
ggplot(Sunnyday28cm, aes(x=X4.Fv_Fm, y=X4.ETR)) +
  labs(title="28cm height sensor's ETR vs Fv/Fm")+
  geom_point()
#Sunnyday 10cm
Sunnyday10cm <- subset(TenCM_PAM_Join, Date.x == as.Date("5/11/2018"))
Sunnyday10cm_variables <- Sunnyday10cm%>%
  select(X6.PAR,X6.Fv_Fm,Tair)
correlation_matrix <- cor(Sunnyday10cm_variables, method = "spearman")
ggcorrplot(correlation_matrix,
           lab=TRUE, #Include numbers
           type = "lower", 
           insig = "blank")
Sunnyday10cm$X6.ETR = as.numeric(Sunnyday10cm$X6.ETR)
ggplot(Sunnyday10cm, aes(x=X6.Fv_Fm, y=X6.ETR)) +
  labs(title="10cm height sensor's ETR vs Fv/Fm")+
  geom_point()
##Sunnyday 82cm
Sunnyday82cm <-EightyCM_PAM_Join %>%
  filter(Date.x >"5/10/2018" & Date.x <"5/12/2018")
Sunnyday82cm_variables <- Sunnyday82cm%>%
  select(X3.PAR,X3.Fv_Fm,Tair)
correlation_matrix <- cor(Sunnyday82cm_variables, method = "spearman")
ggcorrplot(correlation_matrix,
           lab=TRUE, #Include numbers
           type = "lower", 
           insig = "blank")
Sunnyday82cm$X3.ETR = as.numeric(Sunnyday82cm$X3.ETR)
ggplot(Sunnyday82cm, aes(x=X3.Fv_Fm, y=X3.ETR)) +
  labs(title="82cm height sensor's ETR vs Fv/Fm")+
  geom_point()
#Plot ETR vs height on Sunnyday
ETR<- MayPAM
ETR$X3.ETR = as.numeric(ETR$X3.ETR)
ETR$X4.ETR = as.numeric(ETR$X4.ETR)
ETR$X5.ETR = as.numeric(ETR$X5.ETR)
ETR$X6.ETR = as.numeric(ETR$X6.ETR)
ETR$datetime <- mdy_hms(paste(ETR$Date, ETR$Time))
ETR$datetime <- round_date(ETR$datetime, '15 minutes')
ETR$datetime <- as.POSIXct(strptime(ETR$datetime, tz='Etc/GMT+6', format = '%Y-%m-%d %H:%M'))
ETR<-ETR %>%
  filter(Date >"5/10/2018" & Date <"5/12/2018")%>%
  filter(X5.ETR >0 & X5.ETR<850)
ggplot(ETR, aes(datetime)) + 
  geom_point(aes(y = X6.ETR, colour = "10cm")) + 
  geom_point(aes(y = X4.ETR, colour = "28cm"))+
  geom_point(aes(y = X5.ETR, colour = "50cm"))+
  geom_point(aes(y = X3.ETR, colour = "82cm"))+
  scale_x_datetime(labels = date_format("%H:%M"), 
                   breaks = date_breaks("2 hours"), 
                   expand = c(0, 0))
ggplot()+
  geom_point(data = Sunnyday28cm, aes(x = datetime, y = X6.ETR, colour = "28cm"))+
  geom_point(data = Sunnyday50cm, aes(x= datetime, y =X5.ETR, color = "50cm"))
  







#Working with Head #5
Head5<-PAM0301_0528 %>%
  select ("Date", "Time", "X5.F", "X5.Fm.", "X5.PAR", "X5.Y..II.", "X5.ETR" ) %>%
  filter (X5.PAR > 0 ) %>%
  mutate (datetime = mdy_hms(paste(Date, Time)))
Head5$datetime <- as.POSIXct(strptime(Head5$datetime, tz='Etc/GMT+6', format = '%Y-%m-%d %H:%M'))
Head5$datetime <- round_date(Head5$datetime, '15 minutes')
Head5 <- merge(x = Head5, y = EddyPAR1, by = "datetime", all.x = TRUE)  
Head5 <- Head5 %>%
  filter (PAR_IN > 0) %>%
  mutate (flag =2)
Head5[Head5$X5.PAR >= Head5$PAR_IN * 1.3,]$flag <- 1  
Head5 <- Head5 %>%
  filter (flag == 2) %>%
  mutate (X5.Fv_Fm = (X5.Fm. - X5.F)/X5.Fm.)

#Fv/Fm
ggplot(data = Head5, aes(x = datetime)) +
  geom_point(aes(y = X5.Fv_Fm, colour = "Head50cm"))+
  theme(axis.text.x=element_text(angle=90))+
  xlab("Datetime") +
  scale_colour_manual("", 
                      values = c("Head50cm"="red")) +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="Fv/Fm. time (Method1, Head#5)")
ggsave("Fv_Fm vs datetime_method1.png")

#subset PAR=0 for head #3
#Head3PAR0 <-subset(PAM, X3.PAR!=0)
#subset PAR=0 for head #4
#Head4PAR0 <-subset(Head3PAR0, X4.PAR!=0)
#str(Head4PAR0)
#subset PAR=0 for head #5
subsetHead5 <- subset(PAM, X5.PAR!=0)
str(subsetHead5)
#subset PAR=0 for head #6
subsetHead6 <- subset(subsetHead5, X6.PAR!=0)
str(subsetHead6)
#rename PAM dataset with no 0 and null PAR value to new PAM
newPAM <- subsetHead6

#Create a new datetime column
newPAM$datetime <- mdy_hms(paste(newPAM$Date, newPAM$Time))
str(newPAM)
#round time to nearest 15 minute
newPAM$datetime <- round_date(newPAM$datetime, '15 minutes')
str(newPAM)
#format datetime as POSIXct, set time zone (important)
newPAM$datetime <- as.POSIXct(strptime(newPAM$datetime, tz='Etc/GMT+6', format = '%Y-%m-%d %H:%M'))
# exclude unnecessary variables Datetime, Time..rel.,s., Date, Time, Type, No.
#Deletevars <- names(newPAM) %in% c("Datetime", "Time..rel.ms.", "Type", "No.")
Deletevars <- names(newPAM) %in% c( "Time..rel.ms.", "Type")
newPAM <- newPAM[!Deletevars]
#move datetime column to first in cleaned PAM data
newPAM <- newPAM %>%
  select(datetime, everything())

##This moving column function is also valid for any column and any quantity.
##new_df <- df %>%
##select(col_5, col_8, everything())

#read in Eddy Pro PAR data
EddyPAR <- read.csv("GrandBay_biomet_20180507_20180606.csv", stringsAsFactors = F, header = T, na.strings = "-")
str(EddyPAR)
#converting datetime formate to POSIXct. to be consistent with PAM dataset.
EddyPAR$datetime <- as.POSIXct(strptime(EddyPAR$datetime, tz='Etc/GMT+6', format= "%m/%d/%Y %H:%M"))
#select variables  datetime, PAR, tide from Eddy dataset
myvars <- c("datetime", "PAR_IN", "Tide_level", "tide_relative_soil")
EddyParTide <- EddyPAR[myvars]

#join (merge) two data frames (PAM dataset and Eddy dataset), here join Eddy to PAM which has less records.
#Inner join by datetime: returns only the rows in which the left table have matching keys in the right table.
EddyPam <- merge(x = newPAM, y = EddyParTide, by = "datetime", all.x = TRUE)
str(EddyPam)
#filter to values that greater than 0
EddyPamg0 <- subset(EddyPam, PAR_IN>0 )
EddyPamg0$Date <- as.POSIXct(strptime(EddyPamg0$Date, tz='Etc/GMT+6', format= "%m/%d/%Y"))

###Method three: remove any PAM par that is 30% higher than it's corresponding Eddy Par
#add a new column called flag
EddyPamg0$flag <- 2
#If PAM head #3 PAR greate than or equal to 130% Eddy PAR, change its flag column to 1
#EddyPamg0[EddyPamg0$X3.PAR >= EddyPamg0$PAR_IN * 1.3,]$flag <- 1
#Counts the number of flag 1 and flag 2
#table(EddyPamg0$flag)
#If PAM head #4 PAR greate than or equal to 130% Eddy PAR, change its flag column to 1
#EddyPamg0[EddyPamg0$X4.PAR >= EddyPamg0$PAR_IN * 1.3,]$flag <- 1
#table(EddyPamg0$flag)
#If PAM head #5 PAR greate than or equal to 130% Eddy PAR, change its flag column to 1
EddyPamg0[EddyPamg0$X5.PAR >= EddyPamg0$PAR_IN * 1.3,]$flag <- 1
table(EddyPamg0$flag)
#If PAM head #6 PAR greate than or equal to 130% Eddy PAR, change its flag column to 1
EddyPamg0[EddyPamg0$X6.PAR >= EddyPamg0$PAR_IN * 1.3,]$flag <- 1
table(EddyPamg0$flag)
#subset rows that PAM PAR does not exceed 130% Eddy PAR
Removep3 <- subset(EddyPamg0, flag == 2)

#head #5 Fv/Fm vs datetime, method 1. Fv and Fm is the chlorophyll fluorescence yield from plants.
# However for PAM head 3,4,6, selected vegetation samples were either lost or not in the detection area. 
Removep1$`X3.Fv/Fm` <- (Removep1$X3.Fm. - Removep1$X3.F)/Removep1$X3.Fm.
Removep1$`X4.Fv/Fm` <- (Removep1$X4.Fm. - Removep1$X4.F)/Removep1$X4.Fm.
Removep1$`X5.Fv/Fm` <- (Removep1$X5.Fm. - Removep1$X5.F)/Removep1$X5.Fm.
Removep1$`X6.Fv/Fm` <- (Removep1$X6.Fm. - Removep1$X6.F)/Removep1$X6.Fm.
ggplot(data = Removep1, aes(x = datetime)) +
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head50cm"))+
  theme(axis.text.x=element_text(angle=90))+
  xlab("Datetime") +
  scale_colour_manual("", 
                      values = c("Head50cm"="red")) +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="Fv/Fm. time (Method1, Head#5)")
ggsave("Fv_Fm vs datetime_method1.png")

#head #5 Fv/Fm vs datetime, method 2
Removep2$`X3.Fv/Fm` <- (Removep2$X3.Fm. - Removep2$X3.F)/Removep2$X3.Fm.
Removep2$`X4.Fv/Fm` <- (Removep2$X4.Fm. - Removep2$X4.F)/Removep2$X4.Fm.
Removep2$`X5.Fv/Fm` <- (Removep2$X5.Fm. - Removep2$X5.F)/Removep2$X5.Fm.
Removep2$`X6.Fv/Fm` <- (Removep2$X6.Fm. - Removep2$X6.F)/Removep2$X6.Fm.
ggplot(data = Removep2, aes(x = datetime)) +
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head50cm"))+
  theme(axis.text.x=element_text(angle=90))+
  xlab("Datetime") +
  scale_colour_manual("", 
                      values = c("Head50cm"="red")) +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="Fv/Fm. time (Method2, Head#5)")
ggsave("Fv_Fm vs datetime_method2.png")

#head #5 Fv/Fm vs datetime, method 3
Removep3$`X3.Fv/Fm` <- (Removep3$X3.Fm. - Removep3$X3.F)/Removep3$X3.Fm.
Removep3$`X4.Fv/Fm` <- (Removep3$X4.Fm. - Removep3$X4.F)/Removep3$X4.Fm.
Removep3$`X5.Fv/Fm` <- (Removep3$X5.Fm. - Removep3$X5.F)/Removep3$X5.Fm.
Removep3$`X6.Fv/Fm` <- (Removep3$X6.Fm. - Removep3$X6.F)/Removep3$X6.Fm.
ggplot(data = Removep3, aes(x = datetime)) +
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head50cm"))+
  theme(axis.text.x=element_text(angle=90))+
  xlab("Datetime") +
  scale_colour_manual("", 
                      values = c("Head50cm"="red")) +
  scale_y_continuous("Fv/Fm (r.u.))") +
  labs(title="Fv/Fm. time (Method3, Head#5)")
ggsave("Fv_Fm vs datetime_method3.png")

#ETR verse time, method 1
#convert a factor type to numeric for head 3,4,6 ETR records
Removep1$X3.ETR=as.numeric(levels(Removep1$X3.ETR))[Removep1$X3.ETR]
Removep1$X4.ETR=as.numeric(levels(Removep1$X4.ETR))[Removep1$X4.ETR]
#head #5 doesn't exist empty ETR cells because the selected plants were in the original position the entire time
Removep1$X6.ETR=as.numeric(levels(Removep1$X6.ETR))[Removep1$X6.ETR]
ggplot(data = Removep1, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3ETR")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4ETR")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5ETR")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6ETR")) +
  scale_colour_manual("", 
                      breaks = c("Head#3ETR", "Head#4ETR", "Head#5ETR", "Head#6ETR"),
                      values = c("Head#3ETR"="green", "Head#4ETR"="pink", 
                                 "Head#5ETR"="blue" , "Head#6ETR"="orange")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,900)) + 
  labs(title="ETR verse. time (Method1)")
ggsave("ETR Vs time_method1.png")

##ETR verse time, method 2
#convert a factor type to numeric for head 3,4,6 ETR records
Removep2$X3.ETR=as.numeric(levels(Removep2$X3.ETR))[Removep2$X3.ETR]
Removep2$X4.ETR=as.numeric(levels(Removep2$X4.ETR))[Removep2$X4.ETR]
#head #5 doesn't exist empty ETR cells because the selected plants were in the original position the entire time
Removep2$X6.ETR=as.numeric(levels(Removep2$X6.ETR))[Removep2$X6.ETR]
ggplot(data = Removep2, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3ETR")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4ETR")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5ETR")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6ETR")) +
  scale_colour_manual("", 
                      breaks = c("Head#3ETR", "Head#4ETR", "Head#5ETR", "Head#6ETR"),
                      values = c("Head#3ETR"="green", "Head#4ETR"="pink", 
                                 "Head#5ETR"="blue" , "Head#6ETR"="orange")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,900)) + 
  labs(title="ETR verse. time (Method2)")
ggsave("ETR Vs time_method2.png")

###ETR verse time, method 3
Removep3$X3.ETR=as.numeric(levels(Removep3$X3.ETR))[Removep3$X3.ETR]
Removep3$X4.ETR=as.numeric(levels(Removep3$X4.ETR))[Removep3$X4.ETR]
#head #5 doesn't exist empty ETR cells because the selected plants were in the original position the entire time
Removep3$X6.ETR=as.numeric(levels(Removep3$X6.ETR))[Removep3$X6.ETR]
ggplot(data = Removep3, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3ETR")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4ETR")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5ETR")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6ETR")) +
  scale_colour_manual("", 
                      breaks = c("Head#3ETR", "Head#4ETR", "Head#5ETR", "Head#6ETR"),
                      values = c("Head#3ETR"="green", "Head#4ETR"="pink", 
                                 "Head#5ETR"="blue" , "Head#6ETR"="orange")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,900)) + 
  labs(title="ETR verse. time (Method3)")
ggsave("ETR Vs time_method3.png")

#All PAR method 3 Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm
ggplot(data = Removep3, aes(x = Time)) +
  geom_line(aes(y = PAR_IN, colour = "EddyPAR")) +
  #geom_line(aes(y = X3.PAR, colour = "Head#3_80cm")) +
  #geom_line(aes(y = X4.PAR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.PAR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.PAR, colour = "Head#6_10cm")) +
  facet_wrap( ~ Date, ncol = 8)
scale_colour_manual("", 
                    breaks = c("EddyPAR", "Head#5_50cm", "Head#6_10cm"),
                    values = c("EddyPAR"="red",  "Head#5_50cm"="blue" , "Head#6_10cm"="orange")) +
  xlab("Datetime") +
  scale_y_continuous("PAR (?mol/(s*m2))", limits = c(0,2000)) + 
  labs(title="All PAR (Method3)")+
  theme_bw(base_size = 15)
ggsave("All PAR_method3.png")

Removep3$Hour<- format(as.POSIXct(strptime(Removep3$datetime,"%Y-%m-%d %H:%M",tz="Etc/GMT+6")) ,format = "%H:%M")

Removep3$Hour1 <- as.numeric(levels(Removep3$Time))[Removep3$Time]

Removep3 %>%
  ggplot(aes(x = datetime, y = PAR_IN)) +
  geom_point(color = "firebrick3") +
  facet_wrap( ~ Date, ncol = 3) +
  labs(title = "Daily PAR - Grand Bay",
       y = "PAR (?mol m-2 s-1)",
       x = "Time of Day") + theme_bw(base_size = 15)+
  scale_x_datetime(date_breaks = 'hour', date_labels = '%H:%M' )+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#test
# import data
boulder_daily_precip <- read.csv("805325precipdailysum_20032013.csv",
                                 header = TRUE,
                                 na.strings = 999.99)
# view structure of data
str(boulder_daily_precip)

# Create a new data.frame with the newly formatted date field
boulder_daily_precip <- boulder_daily_precip %>%
  mutate(DATE = as.Date(DATE, format = "%m/%d/%y"))

# plot the data using ggplot2 and pipes
ggplot(data=boulder_daily_precip, aes(x = DATE, y = DAILY_PRECIP)) +
  geom_point(color = "darkorchid4") +
  labs(title = "Precipitation - Boulder, Colorado",
       subtitle = "Note using pipes",
       y = "Daily precipitation (inches)",
       x = "Date") + theme_bw(base_size = 15)

boulder_daily_precip %>%
  ggplot(aes(x = DATE, y = DAILY_PRECIP)) +
  geom_point(color = "darkorchid4") +
  labs(title = "Precipitation - Boulder, Colorado",
       subtitle = "The data frame is sent to the plot using pipes",
       y = "Daily precipitation (inches)",
       x = "Date") + theme_bw(base_size = 15)

# plot the data using ggplot2 and facet_wrap
boulder_daily_precip %>%
  na.omit() %>%
  ggplot(aes(x = DATE, y = DAILY_PRECIP)) +
  geom_point(color = "darkorchid4") +
  facet_wrap( ~ YEAR ) +
  labs(title = "Precipitation - Boulder, Colorado",
       subtitle = "Use facets to plot by a variable - year in this case",
       y = "Daily precipitation (inches)",
       x = "Date") + theme_bw(base_size = 15) +
  # adjust the x axis breaks
  scale_x_date(date_breaks = "5 years", date_labels = "%m-%Y")

# plot the data using ggplot2 and pipes
boulder_daily_precip %>%
  ggplot(aes(x = JULIAN, y = DAILY_PRECIP)) +
  geom_point(color = "darkorchid4") +
  facet_wrap( ~ YEAR, ncol = 3) +
  labs(title = "Daily Precipitation - Boulder, Colorado",
       subtitle = "Data plotted by year",
       y = "Daily Precipitation (inches)",
       x = "Day of Year") + theme_bw(base_size = 15)

#sunny day plots 04/12/2018 based on method 1
#sunny day 04/12/2018 Fv/Fm plots 
sunnyday <- subset(Removep1, Date == as.Date("2018-04-12") )
ggplot(data = sunnyday, aes(x = datetime)) +
  geom_line(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_line(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_line(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  theme(axis.text.x=element_text(angle=90))+
  xlab("Datetime") +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="April. 12 suunyday Fv/Fm. vs time (Method1)")
ggsave("sunny Fv_Fm vs datetime_method1.png")

#sunny day 04/12/2018 ETR vs. time
ggplot(data = sunnyday, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,700)) + 
  labs(title="April. 12 sunnyday ETR verse. time (Method1)")
ggsave("suuny ETR Vs time_method1.png")

#sunny day 04/12/2018 All PAR method 1
ggplot(data = sunnyday, aes(x = datetime)) +
  geom_line(aes(y = PAR_IN, colour = "EddyPAR")) +
  geom_line(aes(y = X3.PAR, colour = "Head#3PAR")) +
  geom_line(aes(y = X4.PAR, colour = "Head#4PAR")) +
  geom_line(aes(y = X5.PAR, colour = "Head#5PAR")) +
  geom_line(aes(y = X6.PAR, colour = "Head#6PAR")) +
  scale_colour_manual("", 
                      breaks = c("EddyPAR", "Head#3PAR", "Head#4PAR", "Head#5PAR", "Head#6PAR"),
                      values = c("EddyPAR"="red", "Head#3PAR"="green", "Head#4PAR"="deeppink", 
                                 "Head#5PAR"="blue" , "Head#6PAR"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("PAR (?mol/(s*m2))", limits = c(0,3000)) + 
  labs(title="April. 12 suunyday All PAR (Method1)")
ggsave("sunnyday All PAR_method1.png")

#sunny day plots 04/12/2018 based on method 2
#sunny day 04/12/2018 Fv/Fm plots 
sunnyday2 <- subset(Removep2, Date == as.Date("2018-04-12") )
ggplot(data = sunnyday2, aes(x = datetime)) +
  geom_line(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_line(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_line(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  theme(axis.text.x=element_text(angle=90))+
  xlab("Datetime") +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="April. 12 suunyday Fv/Fm. vs time (Method2)")
ggsave("sunny Fv_Fm vs datetime_method2.png")

#sunny day 04/12/2018 ETR vs. time
ggplot(data = sunnyday2, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,700)) + 
  labs(title="April. 12 sunnyday ETR verse. time (Method2)")
ggsave("suuny ETR Vs time_method2.png")

#sunny day 04/12/2018 All PAR method 2
ggplot(data = sunnyday2, aes(x = datetime)) +
  geom_line(aes(y = PAR_IN, colour = "EddyPAR")) +
  geom_line(aes(y = X3.PAR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.PAR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.PAR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.PAR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("EddyPAR", "Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("EddyPAR"="red", "Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("PAR (?mol/(s*m2))", limits = c(0,3000)) + 
  labs(title="April. 12 suunyday All PAR (Method2)")
ggsave("sunnyday All PAR_method2.png")

#sunny day plots 04/12/2018 based on method 3
#sunny day 04/12/2018 Fv/Fm plots 
sunnyday3 <- subset(Removep3, Date == as.Date("2018-04-12") )
ggplot(data = sunnyday3, aes(x = datetime)) +
  geom_line(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_line(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_line(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  theme(axis.text.x=element_text(angle=90))+
  xlab("Datetime") +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="April. 12 suunyday Fv/Fm. vs time (Method3)")
ggsave("sunny Fv_Fm vs datetime_method3.png")

#sunny day 04/12/2018 ETR vs. time
ggplot(data = sunnyday3, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,700)) + 
  labs(title="April. 12 sunnyday ETR verse. time (Method3)")
ggsave("suuny ETR Vs time_method3.png")

#sunny day 04/12/2018 All PAR method 3
ggplot(data = sunnyday3, aes(x = datetime)) +
  geom_line(aes(y = PAR_IN, colour = "EddyPAR")) +
  geom_line(aes(y = X3.PAR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.PAR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.PAR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.PAR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("EddyPAR", "Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("EddyPAR"="red", "Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("PAR (?mol/(s*m2))", limits = c(0,3000)) + 
  labs(title="April. 12 suunyday All PAR (Method3)")
ggsave("sunnyday All PAR_method3.png")

#cloudy day plots 03/16/2018 based on method 1
#Cloudy day 03/16/2018 Fv/Fm plots 
Cloudyday1 <- subset(Removep1, Date == as.Date("2018-03-16") )
ggplot(data = Cloudyday1, aes(x = datetime)) +
  geom_line(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_line(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_line(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="March. 16 cloudyday Fv/Fm. vs time (Method1)")
ggsave("cloudy day Fv_Fm vs datetime_method1.png")

#Cloudy day 04/12/2018 ETR vs. time method 1
ggplot(data = Cloudyday1, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,200)) + 
  labs(title="March. 16 cloudyday ETR verse. time (Method1)")
ggsave("cloudy day ETR Vs time_method1.png")

#Cloudy day 04/12/2018 All PAR method 1
ggplot(data = Cloudyday1, aes(x = datetime)) +
  geom_line(aes(y = PAR_IN, colour = "EddyPAR")) +
  geom_line(aes(y = X3.PAR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.PAR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.PAR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.PAR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("EddyPAR", "Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("EddyPAR"="red", "Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("PAR (?mol/(s*m2))", limits = c(0,1000)) + 
  labs(title="March. 16 cloudyday All PAR (Method1)")
ggsave("cloudy day All PAR_method1.png")


#cloudy day plots 03/16/2018 based on method 2
#Cloudy day 03/16/2018 Fv/Fm plots 
Cloudyday2 <- subset(Removep2, Date == as.Date("2018-03-16"))
ggplot(data = Cloudyday2, aes(x = datetime)) +
  geom_line(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_line(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_line(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="March. 16 cloudyday Fv/Fm. vs time (Method2)")
ggsave("cloudy day Fv_Fm vs datetime_method2.png")

#Cloudy day 03/16/2018 ETR vs. time method 2
ggplot(data = Cloudyday2, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,200)) + 
  labs(title="March. 16 cloudyday ETR verse. time (Method2)")
ggsave("cloudy day ETR Vs time_method2.png")

#Cloudy day 03/16/2018 All PAR method 2
ggplot(data = Cloudyday2, aes(x = datetime)) +
  geom_line(aes(y = PAR_IN, colour = "EddyPAR")) +
  geom_line(aes(y = X3.PAR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.PAR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.PAR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.PAR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("EddyPAR", "Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("EddyPAR"="red", "Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("PAR (?mol/(s*m2))", limits = c(0,1000)) + 
  labs(title="March. 16 cloudyday All PAR (Method2)")
ggsave("cloudy day All PAR_method2.png")

#cloudy day plots 03/16/2018 based on method 3
#Cloudy day 03/16/2018 Fv/Fm plots 
Cloudyday3 <- subset(Removep3, Date == as.Date("2018-03-16"))
ggplot(data = Cloudyday3, aes(x = datetime)) +
  geom_line(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_line(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_line(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="March. 16 cloudyday Fv/Fm. vs time (Method3)")
ggsave("cloudy day Fv_Fm vs datetime_method3.png")

#Cloudy day 03/16/2018 ETR vs. time method 3
ggplot(data = Cloudyday3, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,200)) + 
  labs(title="March. 16 cloudyday ETR verse. time (Method3)")
ggsave("cloudy day ETR Vs time_method3.png")

#Cloudy day 03/16/2018 All PAR method 3
ggplot(data = Cloudyday3, aes(x = datetime)) +
  geom_line(aes(y = PAR_IN, colour = "EddyPAR")) +
  geom_line(aes(y = X3.PAR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.PAR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.PAR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.PAR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("EddyPAR", "Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("EddyPAR"="red", "Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("PAR (?mol/(s*m2))", limits = c(0,1000)) + 
  labs(title="March. 16 cloudyday All PAR (Method3)")
ggsave("cloudy day All PAR_method3.png")


#Mix day plots 04/09/2018 based on method 1
#Mix day 04/09/2018 Fv/Fm plots 
Mixday1 <- subset(Removep1, Date == as.Date("2018-04-09"))
ggplot(data = Mixday1, aes(x = datetime)) +
  geom_line(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_line(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_line(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="April. 09 Mixday Fv/Fm. vs time (Method1)")
ggsave("Mix day Fv_Fm vs datetime_method1.png")

#Mix day 04/09/2018 ETR vs. time method 1
ggplot(data = Mixday1, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,200)) + 
  labs(title="April. 09 Mixyday ETR verse. time (Method1)")
ggsave("Mix day ETR Vs time_method1.png")

#Mix day 04/09/2018 All PAR method 1
ggplot(data = Mixday1, aes(x = datetime)) +
  geom_line(aes(y = PAR_IN, colour = "EddyPAR")) +
  geom_line(aes(y = X3.PAR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.PAR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.PAR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.PAR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("EddyPAR", "Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("EddyPAR"="red", "Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("PAR (?mol/(s*m2))", limits = c(0,1500)) + 
  labs(title="April. 09 mixday All PAR (Method1)")
ggsave("Mix day All PAR_method1.png")


#Mix day plots 04/09/2018 based on method 2
#Mix day 04/09/2018 Fv/Fm plots 
Mixday2 <- subset(Removep2, Date == as.Date("2018-04-09"))
ggplot(data = Mixday2, aes(x = datetime)) +
  geom_line(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_line(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_line(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="April. 09 Mixday Fv/Fm. vs time (Method2)")
ggsave("Mix day Fv_Fm vs datetime_method2.png")

#Mix day 04/09/2018 ETR vs. time method 2
ggplot(data = Mixday2, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,200)) + 
  labs(title="April. 09 Mixday ETR verse. time (Method2)")
ggsave("Mix day ETR Vs time_method2.png")

#Mix day 04/09/2018 All PAR method 2
ggplot(data = Mixday2, aes(x = datetime)) +
  geom_line(aes(y = PAR_IN, colour = "EddyPAR")) +
  geom_line(aes(y = X3.PAR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.PAR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.PAR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.PAR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("EddyPAR", "Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("EddyPAR"="red", "Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("PAR (?mol/(s*m2))", limits = c(0,1500)) + 
  labs(title="April. 09 cloudyday All PAR (Method2)")
ggsave("Mix day All PAR_method2.png")

#Mix day plots 04/09/2018 based on method 3
#Mix day 04/09/2018 Fv/Fm plots 
Mixday3 <- subset(Removep3, Date == as.Date("2018-04-09"))
ggplot(data = Mixday3, aes(x = datetime)) +
  geom_line(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_line(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_line(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="April. 09 Mixday Fv/Fm. vs time (Method3)")
ggsave("Mix day Fv_Fm vs datetime_method3.png")

#Mix day 04/09/2018 ETR vs. time method 3
ggplot(data = Mixday3, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,200)) + 
  labs(title="April. 09 Mixday ETR verse. time (Method3)")
ggsave("Mix day ETR Vs time_method3.png")

#Mix day 04/09/2018 All PAR method 3
ggplot(data = Mixday3, aes(x = datetime)) +
  geom_line(aes(y = PAR_IN, colour = "EddyPAR")) +
  geom_line(aes(y = X3.PAR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.PAR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.PAR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.PAR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("EddyPAR", "Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("EddyPAR"="red", "Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("PAR (?mol/(s*m2))", limits = c(0,1500)) + 
  labs(title="April. 09 Mixday All PAR (Method3)")
ggsave("Mix day All PAR_method3.png")

#dry day plots 04/30/2018 based on the tide level relative to soil height.
#dry day 04/30/2018 Fv/Fm plots 
Dryday <- subset(Removep1, Date == as.Date("2018-04-30"))
ggplot(data = Dryday, aes(x = datetime)) +
  geom_line(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_line(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_line(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="April. 30 Dryday Fv/Fm. vs time (Method1)")
ggsave("Dry day Fv_Fm vs datetime_method1.png")

#Dry day 04/30/2018 ETR vs. time method 1
ggplot(data = Dryday, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,800)) + 
  labs(title="April. 30 Dryday ETR verse. time (Method1)")
ggsave("Dry day ETR Vs time_method1.png")

#Dry day 04/30/2018 All PAR method 1
ggplot(data = Dryday, aes(x = datetime)) +
  geom_line(aes(y = PAR_IN, colour = "EddyPAR")) +
  geom_line(aes(y = X3.PAR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.PAR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.PAR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.PAR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("EddyPAR", "Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("EddyPAR"="red", "Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("PAR (?mol/(s*m2))", limits = c(0,1500)) + 
  labs(title="April.30 Dryday All PAR (Method1)")
ggsave("Dry day All PAR_method1.png")

#dry day plots 04/30/2018 based on the tide level relative to soil height.
#dry day 04/30/2018 Fv/Fm plots method 2
Dryday2 <- subset(Removep2, Date == as.Date("2018-04-30"))
ggplot(data = Dryday2, aes(x = datetime)) +
  geom_line(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_line(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_line(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="April. 30 Dryday Fv/Fm. vs time (Method2)")
ggsave("Dry day Fv_Fm vs datetime_method2.png")

#Dry day 04/30/2018 ETR vs. time method 2
ggplot(data = Dryday2, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,800)) + 
  labs(title="April. 30 Dryday ETR verse. time (Method2)")
ggsave("Dry day ETR Vs time_method2.png")

#Dry day 04/30/2018 All PAR method 2
ggplot(data = Dryday2, aes(x = datetime)) +
  geom_line(aes(y = PAR_IN, colour = "EddyPAR")) +
  geom_line(aes(y = X3.PAR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.PAR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.PAR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.PAR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("EddyPAR", "Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("EddyPAR"="red", "Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("PAR (?mol/(s*m2))", limits = c(0,1500)) + 
  labs(title="April.30 Dryday All PAR (Method2)")
ggsave("Dry day All PAR_method2.png")

#dry day plots 04/30/2018 based on the tide level relative to soil height.
#dry day 04/30/2018 Fv/Fm plots method 3
Dryday3 <- subset(Removep3, Date == as.Date("2018-04-30"))
ggplot(data = Dryday3, aes(x = datetime)) +
  geom_line(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_line(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_line(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="April. 30 Dryday Fv/Fm. vs time (Method3)")
ggsave("Dry day Fv_Fm vs datetime_method3.png")

#Dry day 04/30/2018 ETR vs. time method 3
ggplot(data = Dryday3, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,800)) + 
  labs(title="April. 30 Dryday ETR verse. time (Method3)")
ggsave("Dry day ETR Vs time_method3.png")

#Dry day 04/30/2018 All PAR method 3
ggplot(data = Dryday3, aes(x = datetime)) +
  geom_line(aes(y = PAR_IN, colour = "EddyPAR")) +
  geom_line(aes(y = X3.PAR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.PAR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.PAR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.PAR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("EddyPAR", "Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("EddyPAR"="red", "Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("PAR (?mol/(s*m2))", limits = c(0,1500)) + 
  labs(title="April.30 Dryday All PAR (Method3)")
ggsave("Dry day All PAR_method3.png")


#High tide day plots 04/22/2018 based on the tide level relative to soil height.
#High tide day 04/22/2018 Fv/Fm plots method 1
Tideday1 <- subset(Removep1, Date == as.Date("2018-04-22"))
ggplot(data = Tideday1, aes(x = datetime)) +
  geom_line(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_line(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_line(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="April. 22 High tide day Fv/Fm. vs time (Method1)")
ggsave("High tide day Fv_Fm vs datetime_method1.png")

#High tide day 04/22/2018 ETR vs. time method 1
ggplot(data = Tideday1, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,600)) + 
  labs(title="April. 22 High tide day ETR verse. time (Method1)")
ggsave("High tide day ETR Vs time_method1.png")

#High tide day 04/22/2018 All PAR method 1
ggplot(data = Tideday1, aes(x = datetime)) +
  geom_line(aes(y = PAR_IN, colour = "EddyPAR")) +
  geom_line(aes(y = X3.PAR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.PAR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.PAR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.PAR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("EddyPAR", "Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("EddyPAR"="red", "Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("PAR (?mol/(s*m2))", limits = c(0,1500)) + 
  labs(title="April.22 High tide day All PAR (Method1)")
ggsave("High tide day All PAR_method1.png")

#High tide day plots 04/22/2018 based on the tide level relative to soil height.
#High tide day 04/22/2018 Fv/Fm plots method 1
Tideday2 <- subset(Removep2, Date == as.Date("2018-04-22"))
ggplot(data = Tideday2, aes(x = datetime)) +
  geom_line(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_line(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_line(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="April. 22 High tide day Fv/Fm. vs time (Method2)")
ggsave("High tide day Fv_Fm vs datetime_method2.png")

#High tide day 04/22/2018 ETR vs. time method 2
ggplot(data = Tideday2, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,600)) + 
  labs(title="April. 22 High tide day ETR verse. time (Method2)")
ggsave("High tide day ETR Vs time_method2.png")

#High tide day 04/22/2018 All PAR method 2
ggplot(data = Tideday2, aes(x = datetime)) +
  geom_line(aes(y = PAR_IN, colour = "EddyPAR")) +
  geom_line(aes(y = X3.PAR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.PAR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.PAR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.PAR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("EddyPAR", "Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("EddyPAR"="red", "Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("PAR (?mol/(s*m2))", limits = c(0,1500)) + 
  labs(title="April.22 High tide day All PAR (Method2)")
ggsave("High tide day All PAR_method2.png")

#High tide day plots 04/22/2018 based on the tide level relative to soil height.
#High tide day 04/22/2018 Fv/Fm plots method 3 
Tideday3 <- subset(Removep3, Date == as.Date("2018-04-22"))
ggplot(data = Tideday3, aes(x = datetime)) +
  geom_line(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_line(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_line(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_line(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("Fv/Fm (r.u.))") + 
  labs(title="April. 22 High tide day Fv/Fm. vs time (Method3)")
ggsave("High tide day Fv_Fm vs datetime_method3.png")

#High tide day 04/22/2018 ETR vs. time method 3
ggplot(data = Tideday3, aes(x = datetime)) +
  geom_line(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,600)) + 
  labs(title="April. 22 High tide day ETR verse. time (Method3)")
ggsave("High tide day ETR Vs time_method3.png")

#High tide day 04/22/2018 All PAR method 3
ggplot(data = Tideday3, aes(x = datetime)) +
  geom_line(aes(y = PAR_IN, colour = "EddyPAR")) +
  geom_line(aes(y = X3.PAR, colour = "Head#3_80cm")) +
  geom_line(aes(y = X4.PAR, colour = "Head#4_28cm")) +
  geom_line(aes(y = X5.PAR, colour = "Head#5_50cm")) +
  geom_line(aes(y = X6.PAR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("EddyPAR", "Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("EddyPAR"="red", "Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Datetime") +
  scale_y_continuous("PAR (?mol/(s*m2))", limits = c(0,1500)) + 
  labs(title="April.22 High tide day All PAR (Method3)")
ggsave("High tide day All PAR_method3.png")

#Tide height vs ETR
#Method 1
ggplot(data = Removep1, aes(x = tide_relative_soil)) +
  geom_point(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_point(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_point(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_point(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Tide height (centimeter)") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,700)) + 
  labs(title=" Tide height vs. ETR  (Method1)")
ggsave("Tide height Vs ETR_method1.png")

#Method 2
ggplot(data = Removep2, aes(x = tide_relative_soil)) +
  geom_point(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_point(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_point(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_point(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Tide height(centimeter)") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,700)) + 
  labs(title=" Tide height vs. ETR  (Method2)")
ggsave("Tide height Vs ETR_method2.png")

#Method 3
ggplot(data = Removep3, aes(x = tide_relative_soil)) +
  geom_point(aes(y = X3.ETR, colour = "Head#3_80cm")) +
  geom_point(aes(y = X4.ETR, colour = "Head#4_28cm")) +
  geom_point(aes(y = X5.ETR, colour = "Head#5_50cm")) +
  geom_point(aes(y = X6.ETR, colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Tide height (centimeter)") +
  scale_y_continuous("ETR ?mol electrons/(s*m2)", limits = c(0,700)) + 
  labs(title=" Tide height vs. ETR  (Method3)")
ggsave("Tide height Vs ETR_method3.png")

#Tide height vs Y(II)
#convert a factor type to numeric for head 3,4,6 Y(II) records
Removep1$X3.Y..II.=as.numeric(levels(Removep1$X3.Y..II.))[Removep1$X3.Y..II.]
Removep1$X4.Y..II.=as.numeric(levels(Removep1$X4.Y..II.))[Removep1$X4.Y..II.]
#head #5 Y(II) columns is in numeric format
Removep1$X6.Y..II.=as.numeric(levels(Removep1$X6.Y..II.))[Removep1$X6.Y..II.]

#Method 1
ggplot(data = Removep1, aes(x = tide_relative_soil)) +
  geom_point(aes(y = X3.Y..II., colour = "Head#3_80cm")) +
  geom_point(aes(y = X4.Y..II., colour = "Head#4_28cm")) +
  geom_point(aes(y = X5.Y..II., colour = "Head#5_50cm")) +
  geom_point(aes(y = X6.Y..II., colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Tide height (Centimeter)") +
  scale_y_continuous("Y(II), (r.u.)") + 
  labs(title=" Tide height vs. Y(II)  (Method1)")
ggsave("Tide height Vs Y(II)_Method1.png")


#Method 2
#convert a factor type to numeric for head 3,4,6 Y(II) records
Removep2$X3.Y..II.=as.numeric(levels(Removep2$X3.Y..II.))[Removep2$X3.Y..II.]
Removep2$X4.Y..II.=as.numeric(levels(Removep2$X4.Y..II.))[Removep2$X4.Y..II.]
#head #5 Y(II) columns is in numeric format
Removep2$X6.Y..II.=as.numeric(levels(Removep2$X6.Y..II.))[Removep2$X6.Y..II.]
ggplot(data = Removep2, aes(x = tide_relative_soil)) +
  geom_point(aes(y = X3.Y..II., colour = "Head#3_80cm")) +
  geom_point(aes(y = X4.Y..II., colour = "Head#4_28cm")) +
  geom_point(aes(y = X5.Y..II., colour = "Head#5_50cm")) +
  geom_point(aes(y = X6.Y..II., colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Tide height (Centimeters)") +
  scale_y_continuous("Y(II), (r.u.)") + 
  labs(title=" Tide height vs. Y(II)  (Method2)")
ggsave("Tide height Vs Y(II)_Method2.png")


#Method 3
#convert a factor type to numeric for head 3,4,6 Y(II) records
Removep3$X3.Y..II.=as.numeric(levels(Removep3$X3.Y..II.))[Removep3$X3.Y..II.]
Removep3$X4.Y..II.=as.numeric(levels(Removep3$X4.Y..II.))[Removep3$X4.Y..II.]
#head #5 Y(II) columns is in numeric format
Removep3$X6.Y..II.=as.numeric(levels(Removep3$X6.Y..II.))[Removep3$X6.Y..II.]
ggplot(data = Removep3, aes(x = tide_relative_soil)) +
  geom_point(aes(y = X3.Y..II., colour = "Head#3_80cm")) +
  geom_point(aes(y = X4.Y..II., colour = "Head#4_28cm")) +
  geom_point(aes(y = X5.Y..II., colour = "Head#5_50cm")) +
  geom_point(aes(y = X6.Y..II., colour = "Head#6_10cm")) +
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Tide height (Centimeters)") +
  scale_y_continuous("Y(II), (r.u.)") + 
  labs(title=" Tide height vs. Y(II)  (Method3)")
ggsave("Tide height Vs Y(II)_Method3.png")

#Tide height vs Fv/Fm
#Method 1
ggplot(data = Removep1, aes(x = tide_relative_soil)) +
  geom_point(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_point(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_point(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_point(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Tide height (Centimeters)") +
  scale_y_continuous("Fv/Fm (r.u.))", limits = c(-0.2,1)) + 
  labs(title="Tide height vs Fv/Fm. (Method1)")
ggsave("Tide height vs Fv_Fm_method1.png")

#Method 2
ggplot(data = Removep2, aes(x = tide_relative_soil)) +
  geom_point(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_point(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_point(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_point(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Tide height (CentiMeters)") +
  scale_y_continuous("Fv/Fm (r.u.))", limits = c(-0.2,1)) + 
  labs(title="Tide height vs Fv/Fm. (Method2)")
ggsave("Tide height vs Fv_Fm_method2.png")

#Method 3
ggplot(data = Removep3, aes(x = tide_relative_soil)) +
  geom_point(aes(y = `X3.Fv/Fm`, colour = "Head#3_80cm"))+
  geom_point(aes(y = `X4.Fv/Fm`, colour = "Head#4_28cm"))+
  geom_point(aes(y = `X5.Fv/Fm`, colour = "Head#5_50cm"))+
  geom_point(aes(y = `X6.Fv/Fm`, colour = "Head#6_10cm"))+
  scale_colour_manual("", 
                      breaks = c("Head#3_80cm", "Head#4_28cm", "Head#5_50cm", "Head#6_10cm"),
                      values = c("Head#3_80cm"="green", "Head#4_28cm"="deeppink", 
                                 "Head#5_50cm"="blue" , "Head#6_10cm"="darkorange1")) +
  xlab("Tide height (Centimeters)") +
  scale_y_continuous("Fv/Fm (r.u.))", limits = c(-0.2,1)) + 
  labs(title="Tide height vs Fv/Fm. (Method3)")
ggsave("Tide height vs Fv_Fm_method3.png")








