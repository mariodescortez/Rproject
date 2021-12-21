# Loading data to R environment
load("~/DataGroupAssignment.Rdata")

# Loading packages
library(rio)
library(dplyr)
library(tidyr)
library(ggplot2)
library(qdapTools)
library(lubridate)
library(readr)
library(scales)
library(patchwork)
library(gganimate)
library(data.table)

#############################################################################
#  DEMOGRAPHICS TABLE
#############################################################################


# Demographics Data Table:

# Check data types of Demographics
glimpse(Demographics) 
head(Demographics,5)

# Translating to date types
Demographics$RegDate<-as.Date(Demographics$RegDate)
Demographics$FirstPay<-as.Date(Demographics$FirstPay, "%Y%m%d")
Demographics$FirstAct<-as.Date(Demographics$FirstAct,"%Y%m%d")
Demographics$FirstSp<-as.Date(Demographics$FirstSp,"%Y%m%d")
Demographics$FirstCa<-as.Date(Demographics$FirstCa,"%Y%m%d")
Demographics$FirstGa<-as.Date(Demographics$FirstGa,"%Y%m%d")
Demographics$FirstPo<-as.Date(Demographics$FirstPo,"%Y%m%d")

# No duplicate Ids in the Demographics table
length(unique(Demographics$UserID)) == length(Demographics$UserID)

# Translating Female and Male to binary
Demographics$Gender <- ifelse(Demographics$Gender == 0,"Female","Male" )

# Importing the appendices files 
Countries <- import("C:/Users/mcortez/Desktop/Group Assignment/Appendices Group Assignment.xlsx", which=2)
Languages <- import("C:/Users/mcortez/Desktop/Group Assignment/Appendices Group Assignment.xlsx", which=3)
Applications <- import("C:/Users/mcortez/Desktop/Group Assignment/Appendices Group Assignment.xlsx", which=4)

# Translating the codes to text appendices strings:
Demographics <- merge(Demographics, Countries, by.x = "Country", by.y = "Country", all.x=TRUE)
Demographics <- merge(Demographics, Languages, by = "Language", all.x=TRUE)
Demographics <- merge(Demographics, Applications, by = "ApplicationID", all.x=TRUE)

# Delete the columns we're not going to use:
Demographics$ApplicationID <- NULL
Demographics$Language <- NULL
Demographics$Country <- NULL

str(Demographics)

# Timeline, FirstAct does not equal missing and variable RegDate ranges from February 1, 2005 through February 27, 2005
sum(is.na(Demographics$FirstAct))
# 2 NA values for FirstAct, we will remove them:
Demographics[26959,]
Demographics[34032,]
Demographics <- Demographics[!is.na(Demographics$FirstAct),]
dim(Demographics)
# Filter dates for RegDate:
Demographics <- Demographics[Demographics$RegDate >= "2005-02-01" & Demographics$RegDate <= "2005-02-27", ]

# Features for Demographic table:
# Activation Period
Demographics$Activation_Period <- as.Date(Demographics$FirstAct,"%Y%m%d")-as.Date(Demographics$RegDate,"%Y%m%d")

# LOR
Demographics$LOR <- as.Date("2005-09-30")-as.Date(Demographics$RegDate,"%Y%m%d")
head(Demographics,2)

# Pay Date until now
Demographics$First_Pay_Now <- as.Date("2005-09-30")-as.Date(Demographics$FirstPay,"%Y%m%d")
head(Demographics,2)

# Active Date until now
Demographics$First_Act_Now <- as.Date("2005-09-30")-as.Date(Demographics$FirstAct,"%Y%m%d")
head(Demographics,2)

# Sport Date until now
Demographics$First_Sp_Now <- as.Date("2005-09-30")-as.Date(Demographics$FirstSp,"%Y%m%d")
head(Demographics,2)
# 1234 na dates 
sum(is.na(Demographics$First_Sp_Now))

# Casino Date until now
Demographics$First_Casino_Now <- as.Date("2005-09-30")-as.Date(Demographics$FirstCa,"%Y%m%d")
head(Demographics,2)
# 36759 na dates 
sum(is.na(Demographics$First_Casino_Now))

# Games Date until now
Demographics$First_Ga_Now <- as.Date("2005-09-30")-as.Date(Demographics$FirstGa,"%Y%m%d")
head(Demographics,2)
# 39374 na dates 
sum(is.na(Demographics$First_Ga_Now))

# Poker Date until now
Demographics$First_Poker_Now <- as.Date("2005-09-30")-as.Date(Demographics$FirstPo,"%Y%m%d")
head(Demographics,2)
# 40505 na dates 
sum(is.na(Demographics$First_Poker_Now))

write.table(Demographics, file = "C:/Users/mcortez/Desktop/Group5_Project_BRA_OpenSource/data/Interim/demographics.csv", sep=",")

#####Plotting Graphs

# Plot for gender
Gender <- Demographics[!is.na(Demographics$Gender),]
plot_gender <- ggplot(Gender, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  ylab("Count of Customers")
plot_gender
Demographics$
# Language
Language <- Demographics[!is.na(Demographics$`Language Description`),]
plot_language <- ggplot(Language, aes(x =reorder(`Language Description`, `Language Description`, function(x)-length(x)), fill = `Language Description` )) +
  geom_bar() +
  ylab("Count of Customers") + xlab('Languages')
plot_language

# Top 5 Countries 
Country <- Demographics[!is.na(Demographics$`Country Name`),]
Country_Top10 <- Country %>% count(`Country Name`)
Country_Top10 <- Country_Top10[order(Country_Top10$n, decreasing = TRUE),]
plot_Countries <- ggplot(data=subset(head(Country_Top10, 5)), aes(x = `Country Name`, y=n, fill = `Country Name`)) +
  geom_bar(stat='identity') + ylab("Count of Customers") + xlab('Country') 
plot_Countries

# Top 5 Applications
Applications_sorted <- Demographics[!is.na(Demographics$`Application Description`),]
Applications_sorted <- Applications_sorted %>% count(`Application Description`)
Applications_sorted <- Applications_sorted[order(Applications_sorted$n, decreasing = TRUE),]
plot_Applications <- ggplot(data=subset(head(Applications_sorted, 5)), aes(x = `Application Description`, y=n, fill = `Application Description`)) +
  geom_bar(stat='identity') + ylab("Count of Customers") + xlab('Application') 
plot_Applications

#Activation Dates count per type of "Games" 
Sp_Now <- Demographics[!is.na(Demographics$First_Sp_Now ),]
Sp_Now_Count <- Sp_Now %>% count()
Ca_Now <- Demographics[!is.na(Demographics$First_Casino_Now),]
Ca_Now_Count <- Ca_Now %>% count()
Ga_Now <- Demographics[!is.na(Demographics$First_Ga_Now),]
Ga_Now_Count <- Ga_Now %>% count()
Po_Now <- Demographics[!is.na(Demographics$First_Poker_Now),]
Po_Now_Count <- Po_Now %>% count()
first_column <- c("Sp_Now_Count","Ca_Now_Count","Ga_Now_Count","Po_Now_Count")
second_column <- c(as.integer(Sp_Now_Count),as.integer(Ca_Now_Count),as.integer(Ga_Now_Count),as.integer(Po_Now_Count))
df <- data.frame(first_column, second_column)
# This shows us that Sports Games are the most popular one for the players
plot_Part_Count_First_Dates <- ggplot(df, aes(x=first_column , y=second_column, fill=first_column)) +
  geom_bar(stat='identity') + ylab("Count of Customers") + xlab('Type of Game Activations ') 
plot_Part_Count_First_Dates

# LOR
plot_LOR <- ggplot(Demographics, aes(x=LOR)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins=10)+
  geom_density(alpha=.2, fill="#FF6666") +
  xlim(c(210, 245))
plot_LOR



#############################################################################
#  DAILY AGGREGATION TABLE
#############################################################################

#Analyze table structure
str(UserDailyAggregation)
#check nulls in dataframe
summary(UserDailyAggregation)
colSums(is.na.data.frame(UserDailyAggregation))
#There are no nulls in this dataframe

#Date is of char datatype. convert it to date type and load it to dummy table
UserDailyAggregation$Date <- ymd(UserDailyAggregation$Date)
str(UserDailyAggregation)

#remove(Products)
#Add product IDs desription for easy understanding
#step1 : Create a temp table with product description as per appendix 1
Products <- data.frame(matrix(ncol = 2, nrow = 8))
colnames(Products)<- c("ProductID","Product_Description")
Products$ProductID <- c(1, 2, 3, 4, 5, 6, 7, 8)
Products$"Product_Description" <- c("Sports book fixed-odd" ,"Sports book live-action" ,"Poker BossMedia" ,"Casino BossMedia" ,"Supertoto" ,"Games VS" ,"Games bwin" ,"Casino Chartwell")
Products$ProductID <- as.numeric(Products$ProductID)
str(Products)

#Creating New Columns for each customer
#creating another temp table as temp_mean for mean calculation
temp_mean <- as_tibble(UserDailyAggregation)
temp_mean$Date <- ymd(temp_mean$Date)

temp_mean <- temp_mean %>% 
  group_by(UserID,ProductID) %>% 
  summarise(cust_mean_Stakes = round(mean(Stakes),2), 
            cust_mean_Winnings = round(mean(Winnings),2),
            cust_mean_Bets = round(mean(Bets),2),
            cust_mean_Stakes = round(max(Stakes),2)
  )

#Creating New Columns for each customer
#creating another temp table as temp_total for mean calculation
temp_total <- as_tibble(UserDailyAggregation)
temp_total$Date <- ymd(temp_total$Date)

temp_total <- temp_total %>% 
  group_by(UserID,ProductID) %>% 
  summarise(cust_Total_Stakes = round(sum(Stakes),2), 
            cust_Total_Winnings = round(sum(Winnings),2),
            cust_Total_Bets = round(sum(Bets),2),
            cust_Total_Stakes = round(sum(Stakes),2),
            cust_Total_Days = n_distinct(Date)
  )
#Merge temp_total and temp_mean with each other as Mean_Total
Mean_Total <- merge(temp_total, temp_mean)

#Merge Mean_Total with product table to add product description
Cust_Mean_Total_Prod <- merge(Mean_Total, Products)


###########
#############
#############
#Create more variables based on each product
#common categories of product are sports,poker, casino and games
#Create a separate entity for Casino
casino <- as_tibble(UserDailyAggregation)
casino$Date <- ymd(casino$Date)

str(casino)

casino <- casino %>% 
  filter( ProductID == 4 | ProductID == 8 ) %>% 
  group_by(UserID) %>% 
  summarise(Total_Stakes_Casino = round(sum(Stakes),2), 
            Total_Win_Casino = round(sum(Winnings),2),
            Total_Bets_Casino = round(sum(Bets),2),
            Total_Stakes_Casino = round(sum(Stakes),2),
            Total_Days_played_Casino = n_distinct(Date),
            mean_Stakes_Casino = round(mean(Stakes),2), 
            mean_Winnings_Casino = round(mean(Winnings),2),
            mean_Bets_Casino = round(mean(Bets),2),
            Highest_Played_Month_casino = names(which.max(table(month(as.Date(Date,format="%Y%m%d"))))),
            Highest_Played_Day_casino =   names(which.max(table(weekdays(as.Date(Date)))))
  ) 

#Replace all NAs with 0
casino <- casino %>% replace(is.na(.), 0)

###Create a aspearate entity for sports
sports <- as_tibble(UserDailyAggregation)
sports$Date <- ymd(sports$Date)

sports <- sports %>% 
  filter(ProductID == 1 | ProductID == 2) %>%
  group_by(UserID) %>% 
  summarise(Total_Stakes_sports = round(sum(Stakes),2), 
            Total_Win_sports = round(sum(Winnings),2),
            Total_Bets_sports = round(sum(Bets),2),
            Total_Stakes_sports = round(sum(Stakes),2),
            Total_Days_played_sports = n_distinct(Date),
            mean_Stakes_sports = round(mean(Stakes),2), 
            mean_Winnings_sports = round(mean(Winnings),2),
            mean_Bets_sports = round(mean(Bets),2),
            Highest_Played_Month_sports = names(which.max(table(month(as.Date(Date,format="%Y%m%d"))))),
            Highest_Played_Day_sports =   names(which.max(table(weekdays(as.Date(Date)))))
  )   

#Replace all NAs with 0
sports <- sports %>% replace(is.na(.), 0)


###Create a aspearate entity for poker
poker <- as_tibble(UserDailyAggregation)
poker$Date <- ymd(poker$Date)

poker <- poker %>% 
  filter(ProductID == 3) %>%
  group_by(UserID) %>% 
  summarise(Total_Stakes_poker = round(sum(Stakes),2), 
            Total_Win_poker = round(sum(Winnings),2),
            Total_Bets_poker = round(sum(Bets),2),
            Total_Stakes_poker = round(sum(Stakes),2),
            Total_Days_played_poker = n_distinct(Date),
            mean_Stakes_poker = round(mean(Stakes),2), 
            mean_Winnings_poker = round(mean(Winnings),2),
            mean_Bets_poker = round(mean(Bets),2),
            Highest_Played_Month_poker = names(which.max(table(month(as.Date(Date,format="%Y%m%d"))))),
            Highest_Played_Day_poker =   names(which.max(table(weekdays(as.Date(Date)))))
  )   

###Create a separate entity for Games
games <- as_tibble(UserDailyAggregation)
games$Date <- ymd(games$Date)

games <- games %>% 
  filter(ProductID == 6 | ProductID == 7) %>%
  group_by(UserID) %>% 
  summarise(Total_Stakes_games = round(sum(Stakes),2), 
            Total_Win_games = round(sum(Winnings),2),
            Total_Bets_games = round(sum(Bets),2),
            Total_Stakes_games = round(sum(Stakes),2),
            Total_Days_played_games = n_distinct(Date),
            mean_Stakes_games = round(mean(Stakes),2), 
            mean_Winnings_games = round(mean(Winnings),2),
            mean_Bets_games = round(mean(Bets),2),
            Highest_Played_Month_games = names(which.max(table(month(as.Date(Date,format="%Y%m%d"))))),
            Highest_Played_Day_games =   names(which.max(table(weekdays(as.Date(Date)))))
  )  



###Create a separate entity for supertoto
supertoto <- as_tibble(UserDailyAggregation)
supertoto$Date <- ymd(supertoto$Date)

supertoto <- supertoto %>% 
  filter(ProductID == 5) %>%
  group_by(UserID) %>% 
  summarise(Total_Stakes_supertoto = round(sum(Stakes),2), 
            Total_Win_supertoto = round(sum(Winnings),2),
            Total_Bets_supertoto = round(sum(Bets),2),
            Total_Stakes_supertoto = round(sum(Stakes),2),
            Total_Days_played_supertoto = n_distinct(Date),
            mean_Stakes_supertoto = round(mean(Stakes),2), 
            mean_Winnings_supertoto = round(mean(Winnings),2),
            mean_Bets_supertoto = round(mean(Bets),2),
            Highest_Played_Month_supertoto = names(which.max(table(month(as.Date(Date,format="%Y%m%d"))))),
            Highest_Played_Day_supertoto =   names(which.max(table(weekdays(as.Date(Date)))))
  ) 


#merge all products table together and #Drop duplicated column
casino_sports <- full_join(casino,sports,by =c("UserID"))
str(casino_sports)


ca_sp_ga <- full_join(x=casino_sports, y=games, by="UserID")
str(ca_sp_ga)

all_products <- full_join(x=ca_sp_ga,y=supertoto, by="UserID")
str(all_products)
#rm(all_products)

#Highest Stakes Among All products
all_products$Highest_Stakes <- pmax(all_products$Total_Stakes_sports,all_products$Total_Stakes_Casino,all_products$Total_Stakes_games,all_products$Total_Stakes_supertoto)  
#Highest Winnings Among All products
all_products$Highest_Wins <- pmax(all_products$Total_Win_sports,all_products$Total_Win_Casino,all_products$Total_Win_games,all_products$Total_Win_supertoto)  
#Highest Bets Among All products
all_products$Highest_Bets <- pmax(all_products$Total_Bets_sports,all_products$Total_Bets_Casino,all_products$Total_Bets_games,all_products$Total_Bets_supertoto)  

#Replacing all NA values in all the columns of all_products dataframe to 0

all_products <- all_products %>% 
  as.data.frame() %>%
  replace(is.na(.), 0)


#####Plotting Graphs

#Best play month for sports
sports_M_plot <- all_products %>%
  filter (Highest_Played_Month_sports != 0) %>%
  select (UserID, Highest_Played_Month_sports)

ggplot(sports_M_plot,aes(x = Highest_Played_Month_sports, fill = Highest_Played_Month_sports)) +
  geom_bar() +
  coord_polar() +
  theme_void() 

#Best play day for sports

sports_D_plot <- all_products %>%
  filter (Highest_Played_Day_sports != 0) %>%
  select (UserID, Highest_Played_Day_sports)

ggplot(sports_D_plot,aes(x = Highest_Played_Day_sports, fill = Highest_Played_Day_sports)) +
  geom_bar() +
  theme_void()+
  labs(title = "Highest Played day in a week for sports", x="Weekdays", y="count")


#Best play month for games
games_M_plot <- all_products %>%
  filter (Highest_Played_Month_games != 0) %>%
  select (UserID, Highest_Played_Month_games)

ggplot(games_M_plot,aes(x = Highest_Played_Month_games, fill = Highest_Played_Month_games)) +
  geom_bar() +
  coord_polar() +
  theme_void() 

#Best play day for games
games_D_plot <- all_products %>%
  filter (Highest_Played_Day_games != 0) %>%
  select (UserID, Highest_Played_Day_games)

ggplot(games_D_plot,aes(x = Highest_Played_Day_games, fill = Highest_Played_Day_games)) +
  geom_bar() +
  theme_void()+
  labs(title = "Highest Played day in a week for games", x="Weekdays", y="count")

#Best play month for supertoto
supertoto_M_plot <- all_products %>%
  filter (Highest_Played_Month_supertoto != 0) %>%
  select (UserID, Highest_Played_Month_supertoto)

ggplot(supertoto_M_plot,aes(x = Highest_Played_Month_supertoto, fill = Highest_Played_Month_supertoto)) +
  geom_bar() +
  coord_polar() +
  theme_void() 

#Best play day for supertoto
supertoto_D_plot <- all_products %>%
  filter (Highest_Played_Day_supertoto != 0) %>%
  select (UserID, Highest_Played_Day_supertoto)

ggplot(supertoto_D_plot,aes(x = Highest_Played_Day_supertoto, fill = Highest_Played_Day_supertoto)) +
  geom_bar() +
  theme_void()+
  labs(title = "Highest Played day in a week for supertoto", x="Weekdays", y="count")

#Best play month for casino
casino_M_plot <- all_products %>%
  filter (Highest_Played_Month_casino != 0) %>%
  select (UserID, Highest_Played_Month_casino)

ggplot(casino_M_plot,aes(x = Highest_Played_Month_casino, fill = Highest_Played_Month_casino)) +
  geom_bar() +
  coord_polar() +
  theme_void() 

#Best play day for casino
casino_D_plot <- all_products %>%
  filter (Highest_Played_Day_casino != 0) %>%
  select (UserID, Highest_Played_Day_casino)

ggplot(casino_D_plot,aes(x = Highest_Played_Day_casino, fill = Highest_Played_Day_casino)) +
  geom_bar() +
  theme_void()+
  labs(title = "Highest Played day in a week for casino", x="Weekdays", y="count")

str(all_products)

#####Exporting Staging intermediate Data 
write.table(casino, file = "C:/Users/mcortez/Desktop/Group5_Project_BRA_OpenSource/data/Interim/casino.csv", sep=",")
write.table(sports, file = "C:/Users/mcortez/Desktop/Group5_Project_BRA_OpenSource/data/Interim/sports.csv", sep=",")
write.table(games, file = "C:/Users/mcortez/Desktop/Group5_Project_BRA_OpenSource/data/Interim/games.csv", sep=",")
write.table(supertoto, file = "C:/Users/mcortez/Desktop/Group5_Project_BRA_OpenSource/data/Interim/supertoto.csv", sep=",")

#############################################################################
#  POKER CHIPS TABLE
#############################################################################


str(PokerChipConversions)
ls.str(PokerChipConversions) 
length(PokerChipConversions)
dim(PokerChipConversions)

head(PokerChipConversions,2)

min(PokerChipConversions$TransDateTime)
max(PokerChipConversions$TransDateTime)

# Correcting type of date column POSIXct POSIXct POSIXlt
PokerChipConversions$TransDateTime<-as.POSIXlt(PokerChipConversions$TransDateTime, format="%Y-%m-%d%H:%M")

# Filter dates for PokerChipConversions:
PokerChipConversions <- PokerChipConversions[PokerChipConversions$TransDateTime >= "2005-02-01" & PokerChipConversions$TransDateTime <= "2005-09-30", ]

#Creating a start date for calculating the recency
PokerChipConversions$AnalysisDate <- format(as.Date("2005-09-30"),"%d-%m-%Y")

#Converting time and date to create two new columns 
PokerChipConversions$TransDay <- format(PokerChipConversions$TransDateTime, format="%d")
PokerChipConversions$TransMonth <- format(PokerChipConversions$TransDateTime, format="%m")
PokerChipConversions$TransTime <- format(PokerChipConversions$TransDateTime, format="%H:%M")

#Changing the format of date to d-m-y 
PokerChipConversions$TransDate <- format(as.Date(PokerChipConversions$TransDateTime), "%d-%m-%Y")
PokerChipConversions$TransDateTime <- NULL

#Adding weekday of sale of Poker Chips
PokerChipConversions$TransDay <- weekdays(as.Date(PokerChipConversions$TransDate))
PokerChipConversions$TransMonth <- month(as.Date(PokerChipConversions$TransDate))
PokerChipConversions$MonthName <- month.name[PokerChipConversions$TransMonth]
PokerChipUpdated <- PokerChipConversions

#Checking for NA values
sapply(PokerChipUpdated, function(x) sum(is.na(x)))

#Giving a summary of our dataset
summary(PokerChipUpdated)

#Number of  Transaction Days Calculation (Recency) for each user and sorting users by month
PokerChipUpdated$TransNbrDays <- as.Date(as.character(PokerChipUpdated$AnalysisDate), format="%d-%m-%Y") - as.Date(as.character(PokerChipUpdated$TransDate), format="%d-%m-%Y")
PokerChipUpdated[order(PokerChipUpdated$UserID),]

#Checking for the total number of Transaction Types  
PokerChipUpdated %>% group_by(TransType) %>% tally()

#Splitting the TransType to 24- Sell 124 - Buy 
PokerChipUpdated$TransType <- ifelse(PokerChipUpdated$TransType == 124, "Buy", "Sell")
head(PokerChipUpdated, 5) 

#Split Trans Amount into Buy and Sell
PokerChipUpdated$ChipBought <- ifelse(PokerChipUpdated$TransType == "Buy", PokerChipUpdated$TransAmount, 0)
PokerChipUpdated$ChipSold <- ifelse(PokerChipUpdated$TransType == "Sell", PokerChipUpdated$TransAmount, 0)

#Calculating the Revenue by User and Counting number of transaction for each user
PokerChipConversions1 <- PokerChipUpdated %>% filter(TransType == "Buy" | TransType == "Sell") %>% group_by(UserID) %>% summarise(TotalChipBought = round(sum(ChipBought), digits = 3), TotalChipSold= round(sum(ChipSold), digits = 3), NumberofTransactions=max(n()), Revenue= round(sum(TransAmount), digits = 3), TransRecentDate = max(TransDate)) %>% ungroup()
head(PokerChipConversions1,5)

#Creating Aggregate of columns for each user transaction
PokerChipTransTotal <- PokerChipUpdated %>% group_by(UserID) %>% summarise(HighestTransMonth = max(MonthName), HighestTransDay = max(TransDay), HighestTransTime = max(TransTime), HighestNbrDays = max(TransNbrDays))
head(PokerChipTransTotal, 5) 

#Merging two datasets - PokerChipConversions and PokerChipTransTotal together after analysis 
PokerTrans <- merge(PokerChipTransTotal, PokerChipConversions1, by= "UserID")
head(PokerTrans, 5) 

#Performing Statistical analysis on the Transaction Amount to get better insight 
PokerChipUpdated %>% group_by(UserID) %>% summarize(TransAmountMean = round(mean(TransAmount), digits = 3), LowestTransAmount = round(min(TransAmount), digits = 3), HighestTransAmount = round(max(TransAmount), digits = 3), TransAmountSD = round(sd(TransAmount)/sqrt(n()), digits = 3 )) -> PokerChipRev
head(PokerChipRev, 3)

#Merging all tables together to create final table
PokerNew <- merge(PokerChipRev, PokerTrans, by= "UserID")
head(PokerNew, 10)

#Arranging datatable and Ranking Data in the Revenue Column  
arrange(PokerNew, desc(Revenue))
PokerNew$PokerRevRank <- rank(PokerNew$Revenue)


#####Plotting Graphs
#Graph plot showing highest transaction weekday 
TransDayPlot <- PokerNew %>%
  filter (HighestTransDay != 0) %>%
  select (UserID, HighestTransDay)

ggplot(TransDayPlot,aes(x = HighestTransDay, fill = HighestTransDay)) +
  geom_bar() +
  theme_void()+
  labs(title = "Highest Transaction Weekday", x="Weekdays", y="count")


#Graph Plot showing the highest transaction months  
TransMonthPlot <- PokerNew %>%
  filter (HighestTransMonth != 0) %>%
  select (UserID, HighestTransMonth)

ggplot(TransMonthPlot,aes(x = HighestTransMonth, fill= HighestTransMonth)) +
  geom_bar() +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  theme_void() +
  labs(title = "Highest Transaction Month", x="Months", y="Count")

#Graph for Top 10 Revenue Generators
RevenuePlot <- top_n(PokerNew, n= 10, Revenue) %>% ggplot(., aes(UserID, Revenue)) + geom_point(shape=21, color="black", fill="#69b3a2", size=6) + geom_line(color="grey") + theme_bw() + labs(title = "Top Users with Highest Revenue", x= "UserID", y="EU €") 


#Graph for Top 10 Spending Customers
TransactionPlot <-top_n(PokerNew, n= 10, HighestTransAmount) %>% ggplot(., aes(UserID, HighestTransAmount)) + geom_point(shape=21, color="black", fill="#69b3a2", size=6) + geom_line(color="grey") + theme_bw() + labs(title = "Top Users with Highest Amount Spent on Transactions", x=" ", y= "Amount Spent on Transactions")

#Graph showing Number of transactions for the months and the amount of transactions
DaysPlot <- ggplot(PokerNew, aes(x= HighestNbrDays, y=NumberofTransactions)) + geom_point() + geom_line() + facet_grid(rows = vars(HighestTransDay)) + labs(title="Variation of Number of Transactions During Weekdays") + scale_x_continuous(breaks = seq(from = 4,to = 8,by=0.5)) + theme_bw()

#Checking for the total number of columns
ncol(PokerNew)
nrow(PokerNew)

#Checking for the data types of each column in the data set
str(PokerNew)

write.csv(PokerNew, "C:/Users/eomuvwie/OneDrive - IESEG/Business Reporting Tools Open Source/Group assignment folder-20211211/Submission/Submission/Group5_Project_BRA_OpenSource/data/Interim/pokerchip.csv", row.names = TRUE)


#############################################################################
#  DATA MART
#############################################################################

# Merging the data to create our Data Mart:

# Demographics
# all_products
# PokerNew

Datamart <- merge(x = Demographics, y = all_products, by = "UserID", all.x = TRUE)
Datamart <- merge(x = Datamart, y = PokerNew, by = "UserID", all.x = TRUE)

# Populating num columns with 0s
Datamart <- Datamart %>% map_if(is.numeric,~ifelse(is.na(.x),0,.x)) %>% as.data.table

# Checking count of null values replaced in the previous step
colSums(is.na.data.frame(Datamart))

# Exporting the final Datamart
# write.table(Datamart, file = "C:/Users/mcortez/OneDrive - IESEG/Open Source Programming/Group Assignment/Datamart.csv", sep=",")


