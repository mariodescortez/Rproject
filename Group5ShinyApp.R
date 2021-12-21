#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Buisness Analytics tools Open source - Group5
# 
#


#Install all required libraries
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("shiny")) install.packages("shiny"); library("shiny")
if(!require("qdapTools")) install.packages("qdapTools"); library("qdapTools")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("DT")) install.packages("DT"); library("DT")
if(!require("plotly")) install.packages("plotly"); library("plotly")
if(!require("shinythemes")) install.packages("shinythemes"); library("shinythemes")
if(!require("gganimate")) install.packages("gganimate"); library("gganimate")
if(!require("shinyWidgets")) install.packages("shinyWidgets"); library("shinyWidgets")
if(!require("maptools")) install.packages("maptools"); library("maptools")
if(!require("fBasics")) install.packages("fBasics"); library("fBasics")
if(!require("foreign")) install.packages("foreign"); library("foreign")
if(!require("stringr")) install.packages("stringr"); library("stringr")
if(!require("rsconnect")) install.packages("rsconnect"); library("rsconnect")
if(!require("showtext")) install.packages("showtext"); library("showtext")


#load source data
load("~/DataGroupAssignment.Rdata")
GApp<-unique(all_products$ApplicationName)


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

ui <- fluidPage(
  #THEME OF THE PROJECT
  theme = shinytheme("cosmo"),
  #TITLE OF THE PROJECT
  titlePanel(titlePanel("Group Assignment Open Source Programming on Gambling Project Group 5")),
  
    ###Main program    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  id= "tabsetpanel", 
                  
                  tabPanel(title = "User Information",
                           plotOutput("Gender_new")),
                  
                  tabPanel(title = "Player Distribution by Language",
                           plotOutput("language")),
                  
                  tabPanel(title = "Player Distribution by Country",
                           plotOutput("country")),
                  tabPanel(title = "Top Applications",
                           plotOutput("applications")),
                  tabPanel(title = "Activation Dates",
                           plotOutput("act_dates")),
                  tabPanel(title = "Length of Relation",
                           plotOutput("LOR")),
                  tabPanel(title="Poker Transaction by Weekdays",
                           plotOutput("TransDayPlot")),
                  tabPanel(title="Poker Transactions by Month",
                           plotOutput("TransMonthPlot")),
                  tabPanel(title="Poker Transactions Revenue",
                           plotOutput("RevenuePlot")),
                  tabPanel(title="Poker Transaction Amounts",
                           plotOutput("TransactionPlot")),
                  tabPanel(title="Poker Number of Transactions",
                           plotOutput("DaysPlot")),

                  tabPanel(title = "Casino Stats",
                           plotOutput("casino_D_plot"),plotOutput("casino_M_plot")),
                  tabPanel(title="Games Stats",
                           plotOutput("games_D_plot"),plotOutput("games_M_plot")),
                  tabPanel(title="Sports Stats",
                           plotOutput("sports_D_plot"),plotOutput("sports_M_plot")),
                  tabPanel(title="supertoto Stats",
                           plotOutput("supertoto_D_plot"),plotOutput("supertoto_M_plot"))
                  
      )
    )
  )
)
server <- function(input, output){
  
##Plot Demographics
  output$Gender_new <- renderPlot({
    Gender <- Demographics[!is.na(Demographics$Gender),]
    ggplot(Gender, aes(x = Gender, fill = as.factor(Gender),position='stack',  stat='identity')) +
      geom_bar() +
      labs(title = "Distribution per gender", x="Gender", y="Count Customers")+
      scale_color_manual(labels = c("T999", "T888"), values = c("blue", "red")) +
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
  })

  output$language <- renderPlot({
    Language <- Demographics[!is.na(Demographics$`Language Description`),]
    #rename(Language, `Language Description` = Language)
    ggplot(Language, aes(x =reorder(`Language Description`, `Language Description`, function(x)-length(x)), fill = `Language Description` )) +
      geom_bar() +
      theme_void()+
      labs(title = "Distribution per Language", x="Language", y="Count Customers")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))

  })
  
  output$country <- renderPlot({
    Country <- Demographics[!is.na(Demographics$`Country Name`),]
    Country_Top10 <- Country %>% count(`Country Name`)
    Country_Top10 <- Country_Top10[order(Country_Top10$n, decreasing = TRUE),]
    ggplot(data=subset(head(Country_Top10, 5)), aes(x = `Country Name`, y=n, fill = `Country Name`)) +
      geom_bar(stat='identity') + ylab("Count of Customers") + xlab('Country') +
      labs(title = "Distribution by Country", x="Country", y="Count of Customers")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  })
  
  output$applications <- renderPlot({
    Applications_sorted <- Demographics[!is.na(Demographics$`Application Description`),]
    Applications_sorted <- Applications_sorted %>% count(`Application Description`)
    Applications_sorted <- Applications_sorted[order(Applications_sorted$n, decreasing = TRUE),]
    ggplot(data=subset(head(Applications_sorted, 5)), aes(x = `Application Description`, y=n, fill = `Application Description`)) +
      geom_bar(stat='identity') + ylab("Count of Customers") + xlab('Application') +
      labs(title = "Most Used Applications by Count Customers", x="Application", y="Count of Customers")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  })
  
  output$act_dates <- renderPlot({
    
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
    ggplot(data=df, aes(x=first_column , y=second_column, fill=first_column)) +
      geom_bar(stat='identity') + ylab("Count of Customers") + xlab('Type of Game Activations ') +
      labs(title = "Aggregation of Activation Dates per Type", x="Type of Activation", y="Count of Customers")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  })
  
  output$LOR <- renderPlot({
    ggplot(Demographics, aes(x=LOR)) +
    geom_histogram(aes(y=..density..), colour="black", fill="white", bins=10)+
    geom_density(alpha=.2, fill="#FF6666") +
    xlim(c(210, 245))

  })  

 #Graph plot of PokerChips 
  output$TransDayPlot <- renderPlot({
    ggplot(TransDayPlot,aes(x = HighestTransDay, fill = HighestTransDay)) +
    geom_bar() +
    theme_void()+
    labs(title = "Highest Transaction Weekday", x="Weekdays", y="count")  
 })

 #Graph Plot showing the highest transaction months  
 output$TransMonthPlot <-renderPlot({
   ggplot(TransMonthPlot,aes(x = HighestTransMonth, fill= HighestTransMonth)) +
   geom_bar() +
   scale_fill_brewer(palette=4) +
   coord_polar(theta="y") +
   theme_void() +
   labs(title = "Highest Transaction Month", x="Months", y="Count")
 })

 #Graph for Top 10 Revenue Generators
 output$RevenuePlot <- renderPlot({
   top_n(PokerNew, n= 10, Revenue) %>% ggplot(., aes(UserID, Revenue)) + geom_point(shape=21, color="black", fill="#69b3a2", size=6) + geom_line(color="grey") + theme_bw() + labs(title = "Top Users with Highest Revenue", x= "UserID", y="EU €") 
 })

#Graph for Top 10 Spending Customers
 output$TransactionPlot <- renderPlot({
   top_n(PokerNew, n= 10, HighestTransAmount) %>% ggplot(., aes(UserID, HighestTransAmount)) + geom_point(shape=21, color="black", fill="#69b3a2", size=6) + geom_line(color="grey") + theme_bw() + labs(title = "Top Users with Highest Amount Spent on Transactions", x=" ", y= "Amount Spent on Transactions")
 })

#Graph showing Number of transactions for the months and the recency of transactions
  output$DaysPlot <- renderPlot({
    ggplot(PokerNew, aes(x= HighestNbrDays, y=NumberofTransactions)) + geom_point() + geom_line() + facet_grid(rows = vars(HighestTransDay)) + labs(title="Variation of Number of Transactions During Weekdays") + scale_x_continuous(breaks = seq(from = 4,to = 8,by=0.5)) + theme_bw()
  })

##Plot Sports    
  output$sports_D_plot <- renderPlot({
    sports_Daily<-all_products %>% dplyr::filter (Highest_Played_Day_sports != 0) %>% select (UserID, Highest_Played_Day_sports)
    ggplot(sports_Daily,aes(x = Highest_Played_Day_sports, fill = Highest_Played_Day_sports)) +
      geom_bar() +
      theme_void()+
      labs(title = "Highest Played day in a week for sports", x="Weekdays", y="count")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  })
  
  output$sports_M_plot <- renderPlot({
    sports_Monthly<-all_products %>% dplyr::filter (Highest_Played_Month_sports != 0) %>% select (UserID, Highest_Played_Month_sports)
    ggplot(sports_Monthly,aes(x = Highest_Played_Month_sports, fill = Highest_Played_Month_sports)) +
      geom_bar() +
      theme_void()+
      labs(title = "Highest Played day in a week for sports", x="Weekdays", y="count")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  }) 
  
###Plot Games  
  output$games_D_plot <- renderPlot({
    Games_Daily<-all_products %>% dplyr::filter (Highest_Played_Day_games != 0) %>% select (UserID, Highest_Played_Day_games)
    ggplot(Games_Daily,aes(x = Highest_Played_Day_games, fill = Highest_Played_Day_games)) +
      geom_bar() +
      theme_void()+
      labs(title = "Highest Played day in a week for games", x="Weekdays", y="count")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme_linedraw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  })
 
  output$games_M_plot <- renderPlot({
    Games_Monthly<-all_products %>% dplyr::filter (Highest_Played_Month_games != 0) %>% select (UserID, Highest_Played_Month_games)
    ggplot(Games_Monthly,aes(x = Highest_Played_Month_games, fill = Highest_Played_Month_games)) +
      geom_bar() +
      theme_void()+
      labs(title = "Highest Played day in a week for games", x="Weekdays", y="count")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme_linedraw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  }) 
###Plot Casino
  output$casino_D_plot <- renderPlot({
    casino_Daily<-all_products %>% dplyr::filter (Highest_Played_Day_casino != 0) %>% select (UserID, Highest_Played_Day_casino)
    ggplot(casino_Daily,aes(x = Highest_Played_Day_casino, fill = Highest_Played_Day_casino)) +
      geom_bar() +
      theme_void()+
      labs(title = "Highest Played day in a week for casino", x="Weekdays", y="count")+
      theme_linedraw()+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  })
  
  output$casino_M_plot <- renderPlot({
    casino_Monthly<-all_products %>% dplyr::filter (Highest_Played_Month_casino != 0) %>% select (UserID, Highest_Played_Month_casino)
    ggplot(casino_Monthly,aes(x = Highest_Played_Month_casino, fill = Highest_Played_Month_casino)) +
      geom_bar() +
      theme_void()+
      labs(title = "Highest Played day in a week for casino", x="Weekdays", y="count")+
      theme_linedraw()+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  })
  
  ####Plot supertoto
  output$supertoto_D_plot <- renderPlot({
    supertoto_Daily<-all_products %>% dplyr::filter (Highest_Played_Day_supertoto != 0) %>% select (UserID, Highest_Played_Day_supertoto)
    ggplot(supertoto_Daily,aes(x = Highest_Played_Day_supertoto, fill = Highest_Played_Day_supertoto)) +
      geom_bar() +
      theme_void()+
      labs(title = "Highest Played day in a week for supertoto", x="Weekdays", y="count")+
      theme(plot.title = element_text(hjust = 0.5))+
      theme_linedraw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  })
  
  output$supertoto_M_plot <- renderPlot({
    supertoto_Monthly<-all_products %>% dplyr::filter (Highest_Played_Month_supertoto != 0) %>% select (UserID, Highest_Played_Month_supertoto)
    ggplot(supertoto_Monthly,aes(x = Highest_Played_Month_supertoto, fill = Highest_Played_Month_supertoto)) +
      geom_bar() +
      theme_void()+
      labs(title = "Highest Played day in a week for supertoto", x="Weekdays", y="count")+
      theme_linedraw()+
      theme(plot.title = element_text(hjust = 0.5))+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))
    
  }) 
  
}

# Run shny app 
shinyApp(ui = ui, server = server)


