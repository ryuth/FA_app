library(lubridate)
library(mailR)
library(shiny)
library(RMySQL)
library(dplyr)
library(plyr)
library(DT)
library(twitteR)
library(shinythemes)

Free_agent <- read.csv("FA.csv")
team <- read.csv("team.csv")
team$Team <- as.character(team$Team)

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

ui <- shinyUI(
  fluidPage(
    
    shinythemes::themeSelector(),
    
    theme = shinytheme("superhero"),
    {
    
    sidebarLayout(
      sidebarPanel(
        
        uiOutput("uiLogin"),
        
        #Part 1 Drop down menu to choose FA
        selectInput("FA_players", "Choose a FA:", choices = Free_agent$Name),
      
        #Part 3 Action button
        
        p("Note: After clicking the submit button, allow 5-10 seconds for the message confirming successful submission of contract offer. Buffering time may differ based on condition of your internet connectivity"),
        br(),
        actionButton("choose","Submit Your Contract Offer to the Selected FA"),
        br(),
        p("You can set the value to $0 in the contract years that you don't want to commit to. e.g. If you want to give 1-year contract, set 2017 salary but set everything else to 0. (Everything is defaulted to zero for you. Please check all the parameters before submitting contracts"),
        br(),
        p("Note: Raises in salary from one year to the next are not allowed to be higher than 100%. Decreases in salary from one year to the next are not allowed to be lower than -50% "),
        
        #Action button for checking point values
        
        actionButton("check","Check points"),
        
        p("Note about 'check points' button: Fill out all contract info below to figure out your points. Continue to play around with the parameters until you outbid the current highest points."),
        
        #Part 5
        
        selectInput("club_option", "Yes or no to club option:", 
                    choices = c("YES","NO"),selected="NO"),
        
        selectInput("vest_option", "Yes or no to vesting option:", 
                    choices = c("YES","NO"),selected="NO"),
        
        sliderInput("n15", "Guaranteed Year:",
                    min = 1, max = 10, value = 1, step = 1),
        
        uiOutput("tickers"),
        
        textInput("n16", "Signing Bonus. (Avoid entering $ sign)",
                  value = 0),
        
        selectInput("n17", "Contract Type:",choices = c("Major League Contract","Minor League Contract")),
        
        wellPanel(
          downloadButton('downloadData', 'Download contract details of signed FAs'),  
          downloadButton('downloadData2','Download bidding history for your team.')
        ),
        
        tags$head(
          tags$style(HTML("
                          @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                          
                          h1 {
                          font-family: 'Lobster',cursive;
                          font-weight: bold;
                          line-height: 1.1;
                          color: #000000;
                          }
                          
                          "))
          ),
        
        tags$head(
          tags$style(HTML("
                          @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                          
                          h3 {
                          font-family: 'Open Sans';
                          font-weight: bold;
                          line-height: 1.1;
                          color: #0013F7;
                          }
                          
                          "))
          ),
        
        tags$head(tags$style("#timer{color: blue;
                             font-size: 20px;
                             font-style: italic;
  }"
        )
        ),
        
        tags$head(tags$style("#values{color: red;
                             font-size: 20px;
                             font-style: bold;
                             }"
        )
        )
        ),
      mainPanel(
        
        h1("World Series of Fantasy Baseball Free Agency"),
        
        tabsetPanel(type = "tabs", 
                    tabPanel("main",h3("Login Status."),
                             verbatimTextOutput("pass"),
                             h3("Timer"),
                             verbatimTextOutput("timer"),
                             h3("Point checker"),
                             verbatimTextOutput("points"),
                             h3("Update"),
                             verbatimTextOutput("values"),h3("10 Most Recent Bidding (Most recent bid at the top. Time in ET)"),
                             tableOutput("recent")
                             ),
                    
                    tabPanel("Announcements",h3("Announcements"),
                             verbatimTextOutput("announce")),
                    
                    tabPanel("Summary", h3("Who Signed Where?"),
                             tableOutput("signed")),
                    
                    tabPanel("History",  h3("Bidding history of FA in target"),
                             tableOutput("table")),
                    
                    tabPanel("Progress",  h3("Your Team Bidding Progress (BETA Version)"),
                             tableOutput("sort_by_team")),
               
                    tabPanel("all result", h3("Highest bidding point for each of 2016 FA. (Time in ET)"),
                             dataTableOutput("all_results"))
                    
      )
        )
    
    )
  
})
)

server <- shinyServer(function(input, output, session){
  
  USER <- reactiveValues(Logged = FALSE)
  
  TEAM <- reactiveValues(name = NA)
  
  output$uiLogin <- renderUI({
    if (USER$Logged == FALSE) {
      wellPanel(
        textInput("Username", "Username: (Case sensitive)"),
        textInput("Password", "Password:"),
        br(),
        actionButton("Login", "Log in")
      )
    }
  })
  
  output$pass <- eventReactive(input$Login,{
    
    con <- dbConnect(drv = MySQL(),user="userid",password="password",dbname="db_name",host="host_address")
    
    PASSWORD <- dbReadTable(con,"Password",row.names=NULL)
    
    dbDisconnect(con)
    
    Username <- isolate(input$Username)
    Password <- isolate(input$Password)
    Id.username <- which(PASSWORD$user == Username)
    Id.password <- which(PASSWORD$password == Password)
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      if (Id.username == Id.password) {
        USER$Logged <- TRUE
        TEAM <- reactiveValues(name = PASSWORD$team[which(PASSWORD$user == input$Username)])
        "Log in successful. (Exit the browser to logoff)"
      } 
    } else  {
      "User name or password failed!!!"
    }
  })
  
  output$tickers <- renderUI({
    
    num_year <- as.integer(input$n15)
    lapply(1:num_year, function(i) {
      list(textInput(paste0("n",i), label = paste0("Year", i+2016," (Avoid entering $ sign)"), value = 0))
    })
    
  })
  
  output$timer <- renderText({
    
    if (USER$Logged == TRUE){
      con <- dbConnect(drv = MySQL(),user="userid",password="password",dbname="db_name",host="host_address")
      tbl <- dbReadTable(con,"FA_TABLE",row.names=NULL)
      clock <- dbReadTable(con,"clock2",row.names=NULL)
      
      dbDisconnect(con)
      
      ineligible <- as.character(clock$Player[clock$clockend %in% "YES"])
      
      tbl <- tbl[tbl$Player %in% input$FA_players,]
      tbl <- tbl[order(tbl$Bid_Num, decreasing = TRUE),]
      tbl <- tbl[1,]
      
      if(tbl$Player[1] %in% ineligible)
      {
        print_it <- paste0("Time is up on ",tbl$Player[1])
      }
      
      if((tbl$Bid_Num[1] == 1) & (!tbl$Player[1] %in% ineligible))
      {
        tbl$Start_Time[1] <-  "Clock hasn't started"
        
        tbl$End_Time[1] <-  "Clock hasn't started"
        
        tbl$Time_left[1] <- "Clock hasn't started"
        
        print_it <- tbl$Time_left[1]
      }
      
      if((tbl$Bid_Num[1] > 1) & (!tbl$Player[1] %in% ineligible))
      {
        
        end_time <-  as.POSIXct(tbl$End_Time[1])
        start_time <- as.POSIXct(tbl$Start_Time[1])
        time_diff <- (as.numeric(as.POSIXct(end_time),units="sec") - as.numeric(as.POSIXct(start_time),units="sec")) - (as.numeric(as.POSIXct(now(tzone="EST")),units="sec") - as.numeric(as.POSIXct(start_time),units="sec")) + 18000
        hour <- time_diff %/% 3600
        min <- time_diff %% 3600
        second <- min %% 60
        min <- min %/% 60
        second <- floor(second)
        print_it <- paste0(hour," hours ",min," mins ",second," seconds to go")
        invalidateLater(1000, session)
        
        print_it
      }
      
      print_it
      
    }
  })
  
  # PART5
  
  sliderValues <- eventReactive(input$choose,{
    
    con <- dbConnect(drv = MySQL(),user="userid",password="password",dbname="db_name",host="host_address")

    clock <- dbReadTable(con,"tb_name",row.names=NULL)
    
    dbDisconnect(con)
    
    ineligible <- as.character(clock$Player[clock$clockend %in% "YES"])
    
    finished <- "NO"
    
    if(input$FA_players %in% ineligible)
    {
      finished <- "YES"
      word <- paste0(input$FA_players," already signed FA contract.")  
    }
    
    if(input$Username == "Tony")
    {
      word <- "You are not authorized to sign any FA"
      word
    }
    
    if ((USER$Logged == TRUE) & (input$Username != "Tony") & (finished == "NO")){
      con <- dbConnect(drv = MySQL(),user="userid",password="password",dbname="db_name",host="host_address")
      tbl <- dbReadTable(con,"tb_name",row.names=NULL)
      
      years <- input$n15
      
      illegal_minor <- FALSE
      
      ifelse((length(input$n1) > 0) & (years %in% c(1:10)),assign(paste0("c",1),input$n1),assign(paste0("c",1),0))
      ifelse((length(input$n2) > 0) & (years %in% c(2:10)),assign(paste0("c",2),input$n2),assign(paste0("c",2),0))
      ifelse((length(input$n3) > 0) & (years %in% c(3:10)),assign(paste0("c",3),input$n3),assign(paste0("c",3),0))
      ifelse((length(input$n4) > 0) & (years %in% c(4:10)),assign(paste0("c",4),input$n4),assign(paste0("c",4),0))
      ifelse((length(input$n5) > 0) & (years %in% c(5:10)),assign(paste0("c",5),input$n5),assign(paste0("c",5),0))
      ifelse((length(input$n6) > 0) & (years %in% c(6:10)),assign(paste0("c",6),input$n6),assign(paste0("c",6),0))
      ifelse((length(input$n7) > 0) & (years %in% c(7:10)),assign(paste0("c",7),input$n7),assign(paste0("c",7),0))
      ifelse((length(input$n8) > 0) & (years %in% c(8:10)),assign(paste0("c",8),input$n8),assign(paste0("c",8),0))
      ifelse((length(input$n9) > 0) & (years %in% c(9:10)),assign(paste0("c",9),input$n9),assign(paste0("c",9),0))
      ifelse((length(input$n10) > 0) & (years %in% c(10)),assign(paste0("c",10),input$n10),assign(paste0("c",10),0))
      ifelse((length(input$n16) > 0),assign(paste0("c",16),input$n16),assign(paste0("c",16),0))
      
      ifelse((exists("c1") == TRUE) & (years %in% c(1:10)), c1 <- as.numeric(gsub(",", "", c1)), c1 <- 0)
      ifelse((exists("c2") == TRUE) & (years %in% c(2:10)), c2 <- as.numeric(gsub(",", "", c2)), c2 <- 0)
      ifelse((exists("c3") == TRUE) & (years %in% c(3:10)), c3 <- as.numeric(gsub(",", "", c3)), c3 <- 0)
      ifelse((exists("c4") == TRUE) & (years %in% c(4:10)), c4 <- as.numeric(gsub(",", "", c4)), c4 <- 0)
      ifelse((exists("c5") == TRUE) & (years %in% c(5:10)), c5 <- as.numeric(gsub(",", "", c5)), c5 <- 0)
      ifelse((exists("c6") == TRUE) & (years %in% c(6:10)), c6 <- as.numeric(gsub(",", "", c6)), c6 <- 0)
      ifelse((exists("c7") == TRUE) & (years %in% c(7:10)), c7 <- as.numeric(gsub(",", "", c7)), c7 <- 0)
      ifelse((exists("c8") == TRUE) & (years %in% c(8:10)), c8 <- as.numeric(gsub(",", "", c8)), c8 <- 0)
      ifelse((exists("c9") == TRUE) & (years %in% c(9:10)), c9 <- as.numeric(gsub(",", "", c9)), c9 <- 0)
      ifelse((exists("c10") == TRUE) & (years %in% c(10)), c10 <- as.numeric(gsub(",", "", c10)), c10 <- 0)
      ifelse((exists("c16") == TRUE), c16 <- as.numeric(gsub(",", "", c16)), c16 <- 0)
      
      ifelse((exists("c1") == TRUE), c1 <- as.numeric(gsub("$", "", c1)), c1 <- 0)
      ifelse((exists("c2") == TRUE), c2 <- as.numeric(gsub("$", "", c2)), c2 <- 0)
      ifelse((exists("c3") == TRUE), c3 <- as.numeric(gsub("$", "", c3)), c3 <- 0)
      ifelse((exists("c4") == TRUE), c4 <- as.numeric(gsub("$", "", c4)), c4 <- 0)
      ifelse((exists("c5") == TRUE), c5 <- as.numeric(gsub("$", "", c5)), c5 <- 0)
      ifelse((exists("c6") == TRUE), c6 <- as.numeric(gsub("$", "", c6)), c6 <- 0)
      ifelse((exists("c7") == TRUE), c7 <- as.numeric(gsub("$", "", c7)), c7 <- 0)
      ifelse((exists("c8") == TRUE), c8 <- as.numeric(gsub("$", "", c8)), c8 <- 0)
      ifelse((exists("c9") == TRUE), c9 <- as.numeric(gsub("$", "", c9)), c9 <- 0)
      ifelse((exists("c10") == TRUE), c10 <- as.numeric(gsub("$", "", c10)), c10 <- 0)
      ifelse((exists("c16") == TRUE), c16 <- as.numeric(gsub("$", "", c16)), c16 <- 0)
      
      ifelse((c1 > 0) & (c1 < 535000), c1 <- as.numeric(535000),c1 <- c1)
      ifelse((c2 > 0) & (c2 < 535000), c2 <- as.numeric(535000),c2 <- c2)
      ifelse((c3 > 0) & (c3 < 535000), c3 <- as.numeric(535000),c3 <- c3)
      ifelse((c4 > 0) & (c4 < 535000), c4 <- as.numeric(535000),c4 <- c4)
      ifelse((c5 > 0) & (c5 < 535000), c5 <- as.numeric(535000),c5 <- c5)
      ifelse((c6 > 0) & (c6 < 535000), c6 <- as.numeric(535000),c6 <- c6)
      ifelse((c7 > 0) & (c7 < 535000), c7 <- as.numeric(535000),c7 <- c7)
      ifelse((c8 > 0) & (c8 < 535000), c8 <- as.numeric(535000),c8 <- c8)
      ifelse((c9 > 0) & (c9 < 535000), c9 <- as.numeric(535000),c9 <- c9)
      ifelse((c10 > 0) & (c10 < 535000), c10 <- as.numeric(535000),c10 <- c10)
      
      
      if(input$club_option %in% "YES")
      {
        option_money <- 0
        option_buy_out <- 0
        
        years <- input$n15
        
        all_year <- as.numeric(c1) + as.numeric(c2) + as.numeric(c3) + as.numeric(c4) + as.numeric(c5) + as.numeric(c6) + as.numeric(c7) + as.numeric(c8) + as.numeric(c9) + as.numeric(c10)
        
        option_money <- as.numeric(round_any(as.numeric(all_year) / as.numeric(years) * 1.25,1000))
        
        option_buy_out <- round_any(as.numeric((option_money * 0.1)),100000)
      }
      
      if(!input$club_option %in% "YES")
      {
        option_money <- 0
        
        option_money <- as.numeric(option_money)
        
        option_buy_out <- 0
        
        option_buy_out <- as.numeric(option_buy_out)
      }
      
      if(input$vest_option %in% "YES")
      {
        vest_money <- 0
        vest_buy_out <- 0
        
        years <- input$n15
        
        all_year_vest <- as.numeric(c1) + as.numeric(c2) + as.numeric(c3) + as.numeric(c4) + as.numeric(c5) + as.numeric(c6) + as.numeric(c7) + as.numeric(c8) + as.numeric(c9) + as.numeric(c10)
        
        vest_money <- as.numeric(round_any(as.numeric(all_year_vest) / as.numeric(years) * 1.25,1000))
        
        vest_buy_out <- round_any(as.numeric((vest_money * 0.1)),100000)
        
      }
      
      if(!input$vest_option %in% "YES")
      {
        vest_money <- 0
        
        vest_money <- as.numeric(vest_money)
        
        vest_buy_out <- 0
        
        vest_buy_out <- as.numeric(vest_buy_out)
      }
      
      years <- input$n15
      
      total <- as.numeric(c1) + as.numeric(c2) + as.numeric(c3) + as.numeric(c4) + as.numeric(c5) + as.numeric(c6) + as.numeric(c7) + as.numeric(c8) + as.numeric(c9) + as.numeric(c10) + as.numeric(option_buy_out) + as.numeric(vest_buy_out) + as.numeric(c16)
      
      AAV <- as.numeric(total) / as.numeric(years)
      
      points <- as.numeric(round_any((as.numeric(total) + (as.numeric(AAV) * 3) + (as.numeric(c1) * 1) - (as.numeric(option_buy_out) * 1) + (as.numeric(vest_buy_out) * 1)) / (1000000) - (as.numeric(years) * 1.5),1))
      
      points <- as.numeric(points)
      
      tbl4 <- tbl[tbl$Player == input$FA_players,]
      
      max_point_player <- max(tbl4$Points)
      
      if(points > max_point_player)
      {
        success <- TRUE
      }
      
      if(points < max_point_player)
      {
        success <- FALSE 
        
      }
      
      if(points == max_point_player)
      {
        success <- FALSE
        
      }
      
      year <- c("c1","c2","c3","c4","c5","c6","c7","c8","c9","c10")
      existing <- vector()
      
      ifelse(c1 >= 535000, existing[1] <- TRUE, existing[1] <- FALSE)
      ifelse(c2 >= 535000, existing[2] <- TRUE, existing[2] <- FALSE)
      ifelse(c3 >= 535000, existing[3] <- TRUE, existing[3] <- FALSE)
      ifelse(c4 >= 535000, existing[4] <- TRUE, existing[4] <- FALSE)
      ifelse(c5 >= 535000, existing[5] <- TRUE, existing[5] <- FALSE)
      ifelse(c6 >= 535000, existing[6] <- TRUE, existing[6] <- FALSE)
      ifelse(c7 >= 535000, existing[7] <- TRUE, existing[7] <- FALSE)
      ifelse(c8 >= 535000, existing[8] <- TRUE, existing[8] <- FALSE)
      ifelse(c9 >= 535000, existing[9] <- TRUE, existing[9] <- FALSE)
      ifelse(c10 >= 535000, existing[10] <- TRUE, existing[10] <- FALSE)
      
      if((length(which(existing == TRUE)) > 1) & (input$n17 == "Minor League Contract"))
      {
        success <- FALSE
        illegal_minor <- TRUE
        
      }
      
      if((success == TRUE) & (input$Username != "Tony") & (illegal_minor == FALSE)){
        
        tbl <- tbl[(tbl$Player == input$FA_players),]
        
        difference <- 0
        
        tbl <- tbl[tbl$Player %in% input$FA_players,]
        tbl <- tbl[order(tbl$Bid_Num, decreasing = TRUE),]
        tbl <- tbl[1,]
        
        tbl$Club <- as.character(tbl$Club)
        
        #tbl$Club <- as.character(tbl$Club)
        
        # Initial end and start time
        end_time <- ymd_hms(tbl$End_Time[1],tz="EST")
        start_time <- ymd_hms(tbl$Start_Time[1],tz="EST")
        
        # Time at the bidding
        
        bid_time <- now(tzone = "EST")
        
        # Time difference between at the time of bidding to the deadline
        if(tbl$Bid_Num[1] == 1)
        {
          difference <- 86400 * 10
          
        }
        
        if(tbl$Bid_Num[1] > 1)
        {
          difference <- as.numeric(end_time - start_time,units="secs") - as.numeric(bid_time - start_time,units="secs")
          
        }

        # Max time difference possible. (240 hrs)
        
        max <- (240*3600)
        
        # Time added at the first bidding
        
        if((tbl$Bid_Num[1]+1) == 2)
        {
          X2nd_bid <- 86400 * 10
          difference <- difference + X2nd_bid
          
        }
        
        if((tbl$Bid_Num[1]+1) == 3)
        {
          X3rd_bid <- 86400 * 5
          difference <- difference + X3rd_bid
          
        }
        
        if((tbl$Bid_Num[1]+1) == 4)
        {
          X4th_bid <- 86400 * 2
          difference <- difference + X4th_bid
          
        }
        
        if((tbl$Bid_Num[1]+1) == 5)
        {
          X5th_bid <- 86400 * 1
          difference <- difference + X5th_bid
          
        }
        
        if((tbl$Bid_Num[1]+1) >= 6)
        {
          X6th_bid <- 86400 * 0.5
          difference <- difference + X6th_bid
        }
        
        # If "difference" is larger than max, difference equals max
        
        if(difference >= max)
        {
          difference <- max
          end_time <- bid_time + difference
          start_time <- bid_time
        }
        
        # If "difference" is less than max, difference equals difference
        
        if(difference < max)
        {
          difference <- difference
          end_time <- bid_time + difference
          start_time <- bid_time
        }
        
        tbl55 <- dbReadTable(con,"tb_name",row.names=NULL)
        
        test <- data.frame(matrix("NA",nrow=1,ncol=27),stringsAsFactors = FALSE)
        
        colnames(test) <- c("row_names","Player","Club","Year_Guaranteed","summary","Signing_Bonus","X2017","X2018","X2019","X2020",
                            "X2021","X2022","X2023","X2024","X2025","X2026","Club_Option","Buyout_1","Vesting",
                            "Buyout2","Points","AAV","Total","Bid_Num","Start_Time","End_Time","Contract_Status")
        
        test$row_names[1] <- NA
        
        test$Player[1] <- input$FA_players
        
        test$Club[1] <- tbl55$team[as.character(tbl55$user) %in% as.character(input$Username)]
        
        years <- input$n15
        
        test$Year_Guaranteed[1] <- years
        
        test$Signing_Bonus[1] <- c16
        
        test$Points[1] <- points
        
        ifelse((total %in% c(1,0) & (tbl$Bid_Num[1] %in% c(1))),summary <- paste0("$0M for ",as.numeric(years),"yr(s)"),summary <- paste0("$",round((total) / 1000000,digits=2),"M for ",years,"yr(s)"))
        
        
        test[1,] <- as.character(c(NA,input$FA_players,as.character(tbl55$team[tbl55$user %in% input$Username]),years,summary,c16,c1,
                                   c2,c3,c4,c5,c6,c7,c8,c9,c10,option_money,option_buy_out,vest_money,vest_buy_out,
                                   points,AAV,total,tbl$Bid_Num[tbl$Player %in% input$FA_players] + 1,strftime(start_time,"%Y-%m-%d %H:%M:%S",tz="EST"),strftime(end_time,"%Y-%m-%d %H:%M:%S",tz="EST"),input$n17))
        
        test <- test[1,]
        
        email <- read.csv("email.csv")
        
        email$email <- as.character(email$email)
        
        email$Team <- as.character(email$Team)
        
        check <- tbl$Points[!is.na(tbl$Points)]
        
        if(all(points > check))
        {
          max_points <- max(tbl$Points,na.rm=TRUE)
          
          teams_to_send_email <- tbl$Club[which(tbl$Points == max_points)]
          
          teams_to_send_email <- unique(teams_to_send_email)
          
          for(jack in 1:length(teams_to_send_email))
          {
            recipients <- email$email[email$X %in% teams_to_send_email[jack]]
            
            namer <- unique(email$Team[email$X %in% teams_to_send_email[jack]])
            
            body <- paste("Hello ",namer,", You have been outbidded by an unknown team for: ",input$FA_players,". ",
                          "An unknown club bidded ",test$Points[1], " points for the lead. Challenge that bid by following this link: https://wsfbdraft.shinyapps.io/free_agents/",
                          " Thank you.
                          
                          Best,
                          
                          James Ryu",sep="")
            
            send.mail(from = "email_address",
                      to = recipients,
                      subject = paste("You have been outbidded by somebody for the following FA: ",input$FA_players,sep=""),
                      body = body,
                      smtp = list(host.name = "smtp.gmail.com", port = 587, user.name = "username", passwd = "password", tls = TRUE),
                      authenticate = TRUE,
                      send = TRUE)
          }
        }
        
        dbWriteTable(conn=con,"FA_TABLE",test,append=T)
        dbWriteTable(conn=con,"FA_TABLE_backup",test,append=T)
        
      }
      
      dbDisconnect(con)
      
      if(success == TRUE)
      {
        word <- paste("Submitted contracts to ",unlist(strsplit(input$FA_players," "))[2]," ",sub(",","",unlist(strsplit(input$FA_players," "))[1]),sep="")
        
      }
      
      if(success == FALSE)
      {
        word <- paste0("Your bid to ", input$FA_players, " at ",points," is not higher than ",max_point_player,". Try again.")
      }
      
      if((success == FALSE) & (illegal_minor == TRUE))
      {
        word <- "You entered illegal minor contract. You can't give multi-year minor league contract"
      }
      
      word
    }
    
    
    word
  })
  
  # Show the values using an HTML table
  output$values <- renderPrint({
    if (USER$Logged == TRUE){
      sliderValues()
    }
  })
  
  output$table <- renderTable({
    
    if (USER$Logged == TRUE){
      
      con <- dbConnect(drv = MySQL(),user="jamesryu",password="ripken77",dbname="fa_data",host="mysql.rfbdatabase.com")
      tbl <- dbReadTable(con,"FA_TABLE",row.names=NULL)
      dbDisconnect(con)
      
      tbl3 <- tbl[tbl$Player %in% input$FA_players,]
      tbl3 <- tbl3[tbl3$Club != "NONE",]
      tbl3 <- tbl3[,c("Player","Year_Guaranteed","Points","Start_Time","Contract_Status","summary")]
      tbl3 <- tbl3[order(tbl3$Points,decreasing=FALSE),]
      tbl3
    }
  })
  
  sliderValues2 <- eventReactive(input$check,{
    
    if (USER$Logged == TRUE){
      
      option_money <- 0
      option_buy_out <- 0
      vest_money <- 0
      vest_buy_out <- 0
      
      years <- input$n15
      
      ifelse((length(input$n1) > 0) & (years %in% c(1:10)),assign(paste0("c",1),input$n1),assign(paste0("c",1),0))
      ifelse((length(input$n2) > 0) & (years %in% c(2:10)),assign(paste0("c",2),input$n2),assign(paste0("c",2),0))
      ifelse((length(input$n3) > 0) & (years %in% c(3:10)),assign(paste0("c",3),input$n3),assign(paste0("c",3),0))
      ifelse((length(input$n4) > 0) & (years %in% c(4:10)),assign(paste0("c",4),input$n4),assign(paste0("c",4),0))
      ifelse((length(input$n5) > 0) & (years %in% c(5:10)),assign(paste0("c",5),input$n5),assign(paste0("c",5),0))
      ifelse((length(input$n6) > 0) & (years %in% c(6:10)),assign(paste0("c",6),input$n6),assign(paste0("c",6),0))
      ifelse((length(input$n7) > 0) & (years %in% c(7:10)),assign(paste0("c",7),input$n7),assign(paste0("c",7),0))
      ifelse((length(input$n8) > 0) & (years %in% c(8:10)),assign(paste0("c",8),input$n8),assign(paste0("c",8),0))
      ifelse((length(input$n9) > 0) & (years %in% c(9:10)),assign(paste0("c",9),input$n9),assign(paste0("c",9),0))
      ifelse((length(input$n10) > 0) & (years %in% c(10)),assign(paste0("c",10),input$n10),assign(paste0("c",10),0))
      ifelse((length(input$n16) > 0),assign(paste0("c",16),input$n16),assign(paste0("c",16),0))
      
      years <- input$n15
      
      ifelse((exists("c1") == TRUE) & (years %in% c(1:10)), c1 <- as.numeric(gsub(",", "", c1)), c1 <- 0)
      ifelse((exists("c2") == TRUE) & (years %in% c(2:10)), c2 <- as.numeric(gsub(",", "", c2)), c2 <- 0)
      ifelse((exists("c3") == TRUE) & (years %in% c(3:10)), c3 <- as.numeric(gsub(",", "", c3)), c3 <- 0)
      ifelse((exists("c4") == TRUE) & (years %in% c(4:10)), c4 <- as.numeric(gsub(",", "", c4)), c4 <- 0)
      ifelse((exists("c5") == TRUE) & (years %in% c(5:10)), c5 <- as.numeric(gsub(",", "", c5)), c5 <- 0)
      ifelse((exists("c6") == TRUE) & (years %in% c(6:10)), c6 <- as.numeric(gsub(",", "", c6)), c6 <- 0)
      ifelse((exists("c7") == TRUE) & (years %in% c(7:10)), c7 <- as.numeric(gsub(",", "", c7)), c7 <- 0)
      ifelse((exists("c8") == TRUE) & (years %in% c(8:10)), c8 <- as.numeric(gsub(",", "", c8)), c8 <- 0)
      ifelse((exists("c9") == TRUE) & (years %in% c(9:10)), c9 <- as.numeric(gsub(",", "", c9)), c9 <- 0)
      ifelse((exists("c10") == TRUE) & (years %in% c(10)), c10 <- as.numeric(gsub(",", "", c10)), c10 <- 0)
      ifelse((exists("c16") == TRUE), c16 <- as.numeric(gsub(",", "", c16)), c16 <- 0)
      
      ifelse((exists("c1") == TRUE), c1 <- as.numeric(gsub("$", "", c1)), c1 <- 0)
      ifelse((exists("c2") == TRUE), c2 <- as.numeric(gsub("$", "", c2)), c2 <- 0)
      ifelse((exists("c3") == TRUE), c3 <- as.numeric(gsub("$", "", c3)), c3 <- 0)
      ifelse((exists("c4") == TRUE), c4 <- as.numeric(gsub("$", "", c4)), c4 <- 0)
      ifelse((exists("c5") == TRUE), c5 <- as.numeric(gsub("$", "", c5)), c5 <- 0)
      ifelse((exists("c6") == TRUE), c6 <- as.numeric(gsub("$", "", c6)), c6 <- 0)
      ifelse((exists("c7") == TRUE), c7 <- as.numeric(gsub("$", "", c7)), c7 <- 0)
      ifelse((exists("c8") == TRUE), c8 <- as.numeric(gsub("$", "", c8)), c8 <- 0)
      ifelse((exists("c9") == TRUE), c9 <- as.numeric(gsub("$", "", c9)), c9 <- 0)
      ifelse((exists("c10") == TRUE), c10 <- as.numeric(gsub("$", "", c10)), c10 <- 0)
      ifelse((exists("c16") == TRUE), c16 <- as.numeric(gsub("$", "", c16)), c16 <- 0)
      
      
      if(input$club_option == "YES")
      {
        years <- input$n15
        
        all_year <- as.numeric(c1) + as.numeric(c2) + as.numeric(c3) + as.numeric(c4) + as.numeric(c5) + as.numeric(c6) + as.numeric(c7) + as.numeric(c8) + as.numeric(c9) + as.numeric(c10)
        
        option_money <- as.numeric(round_any(as.numeric(all_year) / as.numeric(years) * 1.25,1000))
        
        option_buy_out <- round_any(as.numeric((as.numeric(option_money) * 0.1)),100000)
      }
      
      if(input$club_option != "YES")
      {
        option_money <- 0
        
        option_buy_out <- 0
        
      }
      
      if(input$vest_option == "YES")
      {
        all_year_vest <- as.numeric(c1) + as.numeric(c2) + as.numeric(c3) + as.numeric(c4) + as.numeric(c5) + as.numeric(c6) + as.numeric(c7) + as.numeric(c8) + as.numeric(c9) + as.numeric(c10)
        
        years <- input$n15
        
        vest_money <- as.numeric(round_any(as.numeric(all_year_vest) / as.numeric(years) * 1.25,1000))
        
        vest_buy_out <- as.numeric(round_any(as.numeric((as.numeric(vest_money) * 0.1)),100000))
        
      }
      
      if(input$vest_option != "YES")
      {
        vest_money <- 0
        
        vest_buy_out <- 0
      }
      
      years <- input$n15
      
      total <- as.numeric(c1) + as.numeric(c2) + as.numeric(c3) + as.numeric(c4) + as.numeric(c5) + as.numeric(c6) + as.numeric(c7) + as.numeric(c8) + as.numeric(c9) + as.numeric(c10) + as.numeric(option_buy_out) + as.numeric(vest_buy_out) + as.numeric(c16)
      
      AAV <- as.numeric(total) / as.numeric(years)
      
      points <- as.numeric(round_any((as.numeric(total) + (as.numeric(AAV) * 3) + (as.numeric(c1) * 1) - (as.numeric(option_buy_out) * 1) + (as.numeric(vest_buy_out) * 1)) / (1000000) - (as.numeric(years) * 1.5),1))
      
      points <- as.numeric(points)
      
      points
    }
  }
  )
  
  output$points <- renderPrint({
    if (USER$Logged == TRUE){
      sliderValues2()
    }
  })
  
  recent_email <- reactive({
    
    # Connects to database
    
    invalidateLater(20000,session)
    
    dbConnect(drv = MySQL(),user="userid",password="password",dbname="db_name",host="host_address")

    # Loads FA_TABLE from mysql
    
    tbl <- dbReadTable(con,"tb_name",row.names=NULL)
    
    # Subsets the tbl by desired columns and saves it to tbl7
    
    tbl7 <- tbl[,c("Player","Bid_Num","Start_Time","End_Time","Points")]
    
    # Get a list of all FA names
    
    name_list <- unique(tbl7$Player)
    
    # Vector that keeps the row of FA with highest bid_number
    
    retain_row <- vector()
    
    # For loop that will assign the row to retain to the vector called 'retain_row'
    
    for(k in 1:length(name_list))
    {
      max_bid <- max(tbl7$Points[tbl7$Player %in% name_list[k]],na.rm = TRUE)
      
      retain_row[k] <- which((tbl7$Points == max_bid) & (tbl7$Player == name_list[k]))
    }
    
    # Subsets the tbl7 by row number saved on "retain_row"
    
    tbl7 <- tbl7[retain_row,]
    
    # Create column called "Time_Left"
    
    tbl7$Time_Left <- ""
    
    # If Bid_Num is more than 1, add time left. If Bid_Num is not more than 1, Then add "NA"
    
    for(l in 1:nrow(tbl7))
    {
      #ifelse(tbl7$Bid_Num[l] > 1, tbl7$Time_Left[l] <- round(as.numeric(as.POSIXct(tbl7$End_Time[l]),units="sec") - as.numeric(as.POSIXct(now(tzone="EST")),units="sec"),digits=0),tbl7$Time_Left[l] <- "NA")
      
      ifelse(tbl7$Bid_Num[l] > 1, tbl7$Time_Left[l] <- round(as.numeric(as.POSIXct(tbl7$End_Time[l]),units="sec") - as.numeric(as.POSIXct(now(tzone="EST")),units="sec"),digits=0) + 18000,tbl7$Time_Left[l] <- "NA")
    }
    
    # Remove row with NA value in Time Left column
    
    tbl7 <- tbl7[!tbl7$Time_Left %in% c(NA,"NA"),]
    
    tbl7$Time_Left <- as.numeric(tbl7$Time_Left)
    
    # Read "clock" table from mysql server
    
    clock <- dbReadTable(con,"clock2",row.names=NULL)
    
    clock$send <- as.character(clock$send)
    
    # 24hr, 12hr, 1hr, and 0hr convert to seconds
    
    t24 <- 3600 * 24 # 86400
    t12 <- 3600 * 12 # 43200
    t1 <- 3600 # 3600
    t0 <- 0 # 0
    
    # Checks the clock24, clock12, and clock1 variable. Clock shows "YES" when email already has been
    # sent out at the hours specified. (e.g. 24, 12, or 1 hr). "NO" when email has not been sent out.
    # Here is what this loop does:
    # 1) If email has been sent out at the hours specified (So "YES" is given) but 'time remaining' 
    # for specific player from tbl7 is greater than the hours (24,12 or 1), then reset the clock24,
    # clock12, and clock1 of specific player to "yes" from "no", making player eligible for mass email again.
    # 2) If email has not been sent out at the hours specified (So "NO" is given) but 'time remaining'
    # for specific player from tbl7 is less than the hours (24: less than 24 but more than 12, 
    #12: less than 12 but more than 1, or 1: less than 1 but has not been timed out), then send out a
    # mass email about the player.
    
    for(m in 1:nrow(clock))
    {
      #clock <- dbReadTable(con,"tb_name",row.names=NULL)
      
      if(length(which(tbl7$Player %in% clock$Player[m])) > 0)
      {
        # If time left for particular player is more than 24 hours and labeled "YES", that means 
        # email has been sent out before, but bidding increased time left, making it eligible for email
        # alert again. So switch label to "NO"
        
        # Run this if time left of particular is more than 24 hours
        if(((tbl7$Time_Left[which(tbl7$Player %in% clock$Player[m])]) >= t24) == TRUE)
        {
          # "NO" assigned if email was already sent out before and it has more than 24 hours left.
          # "YES" assigned if email was not sent out before and it has more than 24 
          ifelse((clock$clock24[m] == "YES") == TRUE,clock$clock24[m] <- "NO",clock$clock24[m] <- "NO")
          
          clock$clock12[m] <- "NO"
          clock$clock1[m] <- "NO"
          
        }
        
        # Run this if time left of particular player between 12 and 24
        if((((tbl7$Time_Left[which(tbl7$Player %in% clock$Player[m])]) < t24) == TRUE) & (((tbl7$Time_Left[which(tbl7$Player %in% clock$Player[m])]) >= t12) == TRUE))
        {
          # If email has been sent out 24hr and time remaining is between 12 and 24, keep it "YES". If not email hasn't been sent, keep it "NO" so you can send email.
          ifelse((clock$clock24[m] == "YES") == TRUE, clock$clock24[m] <- "YES", clock$clock24[m] <- "NO")
          
          # Email has not been sent out, write "24" into "send" form. This is a way to signal the system
          # to send 24-hour warning email.
          if(clock$clock24[m] == "NO")
          {
            clock$send[m] <- 24
            clock$clock24[m] <- "YES"
          }
        }
        
        
        ###
        
        if(((tbl7$Time_Left[which(tbl7$Player %in% clock$Player[m])]) >= t12) == TRUE)
        {
          # "NO" assigned if email was already sent out before and it has more than 12 hours left.
          # "YES" assigned if email was not sent out before and it has more than 12
          ifelse((clock$clock12[m] == "YES") == TRUE,clock$clock12[m] <- "NO",clock$clock12[m] <- "NO")
          
          clock$clock1[m] <- "NO" 
        }
        
        # Run this if time left of particular between 12 and 24
        if((((tbl7$Time_Left[which(tbl7$Player %in% clock$Player[m])]) < t12) == TRUE) & (((tbl7$Time_Left[which(tbl7$Player %in% clock$Player[m])]) >= t1) == TRUE))
        {
          # 
          ifelse((clock$clock12[m] == "YES") == TRUE, clock$clock12[m] <- "YES", clock$clock12[m] <- "NO")
          
          if(clock$clock12[m] == "NO")
          {
            clock$send[m] <- 12
            clock$clock12[m] <- "YES"
            clock$clock24[m] <- "YES"
          }
        }
        
        ###
        
        if(((tbl7$Time_Left[which(tbl7$Player %in% clock$Player[m])]) >= t1) == TRUE)
        {
          # "NO" assigned if email was already sent out before and it has more than 1 hour left.
          # "YES" assigned if email was not sent out before and it has more than 1
          ifelse((clock$clock1[m] == "YES") == TRUE,clock$clock1[m] <- "NO",clock$clock1[m] <- "NO")
          
        }
        
        # Run this if time left of particular between 0 and 1
        if((((tbl7$Time_Left[which(tbl7$Player %in% clock$Player[m])]) < t1) == TRUE) & (((tbl7$Time_Left[which(tbl7$Player %in% clock$Player[m])]) > 0) == TRUE))
        {
          # 
          ifelse((clock$clock1[m] == "YES") == TRUE, clock$clock1[m] <- "YES", clock$clock1[m] <- "NO")
          
          if(clock$clock1[m] == "NO")
          {
            clock$send[m] <- 1
            clock$clock1[m] <- "YES"
            clock$clock12[m] <- "YES"
            clock$clock24[m] <- "YES"
          }
        }
        
        # Insert code for 0hr email
        
        if(((tbl7$Time_Left[which(tbl7$Player %in% clock$Player[m])]) <= 0) == TRUE)
        {
          # "NO" assigned if email was already sent out before and it has more than 1 hour left.
          # "YES" assigned if email was not sent out before and it has more than 1
          ifelse((clock$clockend[m] == "YES") == TRUE,clock$clockend[m] <- "YES",clock$clockend[m] <- "NO")
          
          if(clock$clockend[m] == "NO")
          {
            clock$send[m] <- 0 
            clock$clockend[m] <- "YES"
            clock$clock1[m] <- "YES"
            clock$clock12[m] <- "YES"
            clock$clock24[m] <- "YES"
          }
        }
        
      }
      
      if(length(which(tbl7$Player %in% clock$Player[m])) == 0)
      {
        next;
      }

    }
    
    mail_out <- which(clock$send %in% c("0","1","12","24"))
    
    if(length(mail_out) > 0)
    {
      for(d in 1:length(mail_out))
      {
        #clock <- dbReadTable(con,"tb_name",row.names=NULL)
        
        which_hour <- clock$send[mail_out[d]]
        
        if(which_hour %in% c("1","12","24"))
        {
          t <- try(updateStatus(paste0(which_hour,"-hour alert for ",unlist(strsplit(as.character(clock$Player[mail_out[d]])," "))[2]," ",sub(",","",unlist(strsplit(as.character(clock$Player[mail_out[d]])," "))[1]),". Make your bid by ", tbl7$End_Time[tbl7$Player %in% clock$Player[mail_out[d]]],"ET")))
          
          if("try-error" %in% class(t)){
            clock$send[mail_out[d]] <- NA
          }
          
          if(!("try-error" %in% class(t)))
          {
            updateStatus(paste0(which_hour,"-hour alert for ",unlist(strsplit(as.character(clock$Player[mail_out[d]])," "))[2]," ",sub(",","",unlist(strsplit(as.character(clock$Player[mail_out[d]])," "))[1]),". Make your bid by ", tbl7$End_Time[tbl7$Player %in% clock$Player[mail_out[d]]],"ET"))
            
          }

        }
        
        if(which_hour %in% c("0",0))
        {
          tbl_highest <- tbl[tbl$Player == clock$Player[mail_out[d]],]
          
          tbl_highest <- tbl_highest[order(tbl_highest$Points,decreasing = TRUE),]
          
          tbl_highest <- tbl_highest[1,]
          
          t <- try(updateStatus(paste0("Going once, going twice, and..SOLD! ",unlist(strsplit(as.character(clock$Player[mail_out[d]])," "))[2]," ",sub(",","",unlist(strsplit(as.character(clock$Player[mail_out[d]])," "))[1])," signs with ",tbl_highest$Club[1]," on a deal worth ",tbl_highest$summary[1])))
          
          if("try-error" %in% class(t)){
            clock$send[mail_out[d]] <- NA
          }
          
          if(!("try-error" %in% class(t)))
          {
            updateStatus(paste0("Going once, going twice, and..SOLD! ",unlist(strsplit(as.character(clock$Player[mail_out[d]])," "))[2]," ",sub(",","",unlist(strsplit(as.character(clock$Player[mail_out[d]])," "))[1])," signs with ",tbl_highest$Club[1]," on a deal worth ",tbl_highest$summary[1]))
            
          }
          

        }
        
        #clock <- clock[,c("Player","clock24","clock12","clock1","clockend","send")]
        
        #dbWriteTable(con,"clock2",clock,overwrite=TRUE)
      }

      clock$send[which(clock$send %in% c(0,1,12,24))] <- NA
      
    }
    
    clock <- clock[,c("Player","clock24","clock12","clock1","clockend","send")]
    
    dbWriteTable(con,"tb_name",clock,overwrite=TRUE)
    dbWriteTable(con,"tb_name",clock,overwrite=TRUE)
    
    write.csv(clock,"clocker.csv",row.names = FALSE)
    dbDisconnect(con)
    
    tbl8 <- tbl[(nrow(tbl)):(nrow(tbl)-9),]
    
    tbl8 <- tbl8[,c("row_names","Player","Year_Guaranteed","summary","End_Time")]
    
    tbl8$row_names <- c(1:10)
    
    tbl8
    
  })
  
  output$recent <- renderTable({
    recent_email()
  })
  
  output$signed <- renderTable({
    
    if(USER$Logged == TRUE){
      
      invalidateLater(300000,session)
      
      con <- dbConnect(drv = MySQL(),user="userid",password="password",dbname="db_name",host="host_address")

      clock <- dbReadTable(con,"tb_name",row.names=NULL)
      
      tbl <- dbReadTable(con,"tb_name",row.names=NULL)
      
      signed_player <- unique(as.character(clock$Player[clock$clockend == "YES"]))
      
      tblss <- tbl[(tbl$Player %in% signed_player),]
      
      if(nrow(tblss) > 0)
      {
        
        test <- aggregate(x=tblss$Points,by=list(tblss$Player),FUN="max")
        
        retainer <- vector()
        
        for(v in 1:nrow(test))
        {
          retainer[v] <- which((tblss$Points %in% test$x[v]) & (tblss$Player %in% test$Group.1[v]))[1]
        }
        
        tblss <- tblss[retainer,]
        
        tblss <- tblss[tblss$Club != "NONE",]
        
        tblss <- tblss[,c("Player","Club","Year_Guaranteed","summary","Points","Bid_Num","Contract_Status")]
        
      }
      
      if(nrow(tblss) > 0)
      {
        tblss <- tblss
      }
      
      if(nrow(tblss) == 0)
      {
        tblss <- "No FA has been signed yet"
      }
      
      dbDisconnect(con)
      
      tblss
    }
    
    
    
  })
  
  output$sort_by_team <- renderTable({
    
    if(USER$Logged == TRUE){
      
      con <- dbConnect(drv = MySQL(),user="userid",password="password",dbname="db_name",host="host_address")

      tbl56 <- dbReadTable(con,"tb_name",row.names=NULL)
      
      tbl10 <- dbReadTable(con,"tb_name",row.names=NULL)
      
      dbDisconnect(con)
      
      club_name <- tbl56$team[tbl56$user == input$Username]
      
      tbl11 <- tbl10[tbl10$Club == tbl56$team[tbl56$user == input$Username],]
      
      all_bidded <- unique(tbl11$Player)
      
      tbl14 <- data.frame(matrix(NA,nrow=1,ncol=6))
      
      colnames(tbl14) <- c("Player","Year_Guaranteed","summary","Points","Winning_Bid_Points","Your_Rank")
      
      if(length(all_bidded) > 0)
      {
        for(t in 1:length(all_bidded))
        {
          tbl12 <- tbl10[tbl10$Player %in% all_bidded[t],]
          
          tbl12 <- tbl12[tbl12$Club != "NONE",]
          
          tbl12 <- tbl12[order(tbl12$Points,decreasing=TRUE),]
          
          max_point <- unique(max(tbl12$Points[tbl12$Club %in% club_name]))
          
          rank <- which((tbl12$Points == max_point) & (tbl12$Club %in% club_name))
          
          rank <- rank[1]
          
          seg <- data.frame(matrix(NA,nrow=1,ncol=6))
          
          colnames(seg) <- c("Player","Year_Guaranteed","summary","Points","Winning_Bid_Points","Your_Rank")
          
          seg[1:6] <- c(all_bidded[t],tbl12$Year_Guaranteed[rank],tbl12$summary[rank],max_point,tbl12$Points[1],rank)
          
          tbl14 <- rbind(tbl14,seg)
          
          tbl14 <- tbl14[!tbl14$Player %in% c(NA),]
          
        }
        
      }
      
      if(length(all_bidded) == 0)
      {
        tbl14 <- "You have not pursued any FA yet."
      }
      
      
      tbl14
      
    }
    
  })
  
  output$all_results <- renderDataTable({
    
    if(USER$Logged == TRUE){
      
      con <- dbConnect(drv = MySQL(),user="userid",password="password",dbname="db_name",host="host_address")
      tbl <- dbReadTable(con,"FA_TABLE",row.names=NULL)
      
      clock <- dbReadTable(con,"tb_name",row.names=NULL)
      
      dbDisconnect(con)
      
      # Get a names of all FA
      
      all_names <- unique(tbl$Player)
      
      # Order them by highest to lowest point
      
      tbl3 <- tbl[order(tbl$Points,decreasing=TRUE),]
      
      # House highest bidding point for all players
      
      high_bid <- vector()
      
      # Assign highest bidding point for all players
      
      for(i in 1:length(all_names))
      {
        max_point <- max(tbl3$Bid_Num[tbl3$Player %in% all_names[i]],na.rm = TRUE)
        
        high_bid[i] <- which((tbl3$Bid_Num %in% max_point) & (tbl3$Player %in% all_names[i]))
      }
      
      # Retain only row that contains highest bid for each player
      
      tbl3 <- tbl3[high_bid,]
      
      # Retain only the columns specified below in tbl2
      
      tbl2 <- tbl[,c("Player","Points","summary","Start_Time","End_Time","Bid_Num")]
      
      tbl2$Position <- ""
      
      # In 'highest' vector, only keep max points from each player
      
      highest <- vector()
      
      # Keep only the rows with highest bid
      
      for(i in 1:length(all_names))
      {
        max_point <- max(tbl2$Points[tbl2$Player %in% all_names[i]],na.rm = TRUE)
        
        highest[i] <- which((tbl2$Points %in% max_point) & (tbl2$Player %in% all_names[i]))[1]
      }
      
      tbl2 <- tbl2[highest,]
      
      tbl2$Time_left <- ""
      
      Free_agent <- read.csv("FA.csv")
      
      for(k in 1:nrow(tbl2))
      {
        tbl2$Start_Time[k] <- tbl2$Start_Time[tbl3$Player %in% tbl2$Player[k]]
        
        tbl2$End_Time[k] <- tbl2$End_Time[tbl3$Player %in% tbl2$Player[k]]
        
        tbl2$Position[k] <- as.character(Free_agent$Position[Free_agent$Name %in% tbl2$Player[k]])
        
        if((tbl2$Bid_Num[k] == 1))
        {
          tbl2$Start_Time[k] <-  "Clock hasn't started"
          
          tbl2$End_Time[k] <-  "Clock hasn't started"
          
          tbl2$Time_left[k] <- "Clock hasn't started"
        }
        
        
        if((tbl2$Bid_Num[k] > 1))
        {
          tbl2$Time_left[k] <- as.numeric(as.numeric(as.POSIXct(tbl2$End_Time[tbl2$Player %in% tbl3$Player[k]]),units="sec") - as.numeric(as.POSIXct(now(tzone="EST")),units="sec")) + 18000
          
          tbl2$Time_left[k] <- as.character(seconds_to_period(tbl2$Time_left[k]))
          
          tbl2$Time_left[k] <- paste(as.character(unlist(strsplit(tbl2$Time_left[k]," "))[1]),as.character(unlist(strsplit(tbl2$Time_left[k]," "))[2]),as.character(unlist(strsplit(tbl2$Time_left[k]," "))[3]),paste(substr(unlist(strsplit(tbl2$Time_left[k]," "))[4],1,4)," S",sep=""),sep=" ")
          
          tbl2$Time_left[k] <- sub(pattern = "NAS",replacement = "",x = tbl2$Time_left[k])
          
          tbl2$Time_left[k] <- paste0(tbl2$Time_left[k]," left.") 
        }
        
      }
      
      
      if((tbl2$Bid_Num[k] %in% c(1)))
      {
        tbl2$summary[k] <- "--"
      }
      
      tbl2 <- tbl2[order(tbl2$Player,decreasing=TRUE),]
      
      ineligibles <- clock$Player[clock$clockend == "YES"]
      
      tbl2 <- tbl2[!tbl2$Player %in% ineligibles,]
      
      tbl2$Bid_Num <- NULL
      
      tbl2 <- DT::datatable(tbl2,options=list(pageLength = 10))
      
      tbl2 
      
    }
    
  })
  
  output$announce <- renderText({
  
    "
    Jan-16-2016 2:10 AM: New free agents:
    
    Huff, David
    Giavotella, Johnny
    Rasmus, Cory
    Gentry, Craig
    Pena, Brayan
    Hamilton, Josh
    Fryer, Eric
    Freeman, Mike
    Lobaton, Jose

    Thank you,

    JR

    --------

    Here are the list of just added FAs:
    Javy Guerra
    Al Albuquerque
    Hector Santiago
    A.J. Achter
    Justin Miller
    Brandon Barnes
    David Lough
    Emmanuel Burris
    David Buchanan
    Steve Clevenger
    Eric Sogard
    Matt McBride
    Felix Doubront
    Jarrod Parker
    Collin Cowgill
    Michael Kirkman
    Tom Wilhelmsen
    Brett Lawrie
    Avisail Garcia
    Daniel Webb
    Neil Ramirez
    Kyle Gibson
    Eduardo Escobar
    Oswaldo Arcia
    Jake Elmore
    Daniel Fields
    Chris Bassitt
    Tyler Olson
    Jabari Blash
    Mark Canha
    Tyler Ladendorf
    Andrew Lambo
    Josh Rodriguez
    Erasmo Ramirez
    Austin Adams
    Ben Revere 
    Charlie Furbush
    Dillon Gee
    Alexi Amarista
    Vicente Campos
    Jose Pirela
    Christian Friedrich
    Blake Wood
    Ryan Flaherty
    Cesar Ramos
    Eric Surkamp
    Rene Rivera
    Jeff Walters
    Eric Campbell
    Aaron Brooks
    Spencer Patton
    Clayton Richard
    Cody Ege"
  })
  
  output$downloadData <- downloadHandler(
    
    filename = function() { 
      paste('contract_info.csv', sep='') 
    },
    content = function(filename,results) {
      
      con <- dbConnect(drv = MySQL(),user="userid",password="password",dbname="db_name",host="host_address")

      clocked <- dbReadTable(con,"tb_name",row.names=NULL)
      
      clocked_name <- clocked$Player[clocked$clockend %in% "YES"]
      
      results <- dbReadTable(con,"tb_name",row.names=NULL)
      
      total_dollar_table <- dbReadTable(con,"tb_name",row.names = NULL)
      
      results <- results[,c("Player","Club","Year_Guaranteed","summary","Signing_Bonus","X2017","X2018","X2019","X2020","X2021","X2022","X2023","X2024","X2025","X2026","Club_Option","Buyout_1","Vesting","Buyout2","Points","AAV","Total","Contract_Status","End_Time")]
      
      results <- results[results$Player %in% clocked_name,]
      
      vecs <- vector()
      
      for(n in 1:length(clocked_name))
      {
        maximal <- max(results$Points[results$Player %in% clocked_name[n]])[1]
        
        if(length(which((results$Player %in% clocked_name[n]) & (results$Points %in% maximal))) == 1)
        {
          vecs[n] <- which((results$Player %in% clocked_name[n]) & (results$Points %in% maximal))
          
        }
        
        if(length(which((results$Player %in% clocked_name[n]) & (results$Points %in% maximal))) > 1)
        {
          vecs[n] <- which((results$Player %in% clocked_name[n]) & (results$Points %in% maximal))[length(which((results$Player %in% clocked_name[n]) & (results$Points %in% maximal)))]
        }
        
      }
      
      results <- results[vecs,]
      
      for(r in 5:22)
      {
        results[,r] <- prettyNum(results[,r],big.mark = ",",scientific=FALSE)
      }
      
      total_dollar_table <- results
      
      dbWriteTable(conn=con,"tb_name",total_dollar_table,overwrite=TRUE)
      
      dbDisconnect(con)
      
      write.csv(results, filename,row.names = FALSE)
      
    }
  )
  
  output$downloadData2 <- downloadHandler(
    
    filename = function() { 
      paste('your_bidding_history.csv', sep='') 
    },
    
    content = function(filename,tables) {
      
      if(USER$Logged == TRUE){
        con <- dbConnect(drv = MySQL(),user="userid",password="password",dbname="db_name",host="host_address")

        tbl56 <- dbReadTable(con,"tb_name",row.names=NULL)
        
        tables <- dbReadTable(con,"tb_name",row.names=NULL)
        
        dbDisconnect(con)
        
        team_selected <- tbl56$team[as.character(tbl56$user) %in% as.character(input$Username)]
        
        tables <- tables[tables$Club %in% team_selected,]
        
        tables <- tables[order(tables$Start_Time, decreasing=FALSE),]
        
        #tables <- tables[,c(6:24)]
        
        tables <- tables[,c("Player","Club","Year_Guaranteed","summary","Signing_Bonus","X2017","X2018","X2019","X2020","X2021","X2022","X2023","X2024","X2025","X2026","Club_Option","Buyout_1","Vesting","Buyout2","Points","AAV","Total","Contract_Status")]
        
        for(s in 5:22)
        {
          tables[,s] <- prettyNum(tables[,s],big.mark = ",",scientific=FALSE)
        }
        
        write.csv(tables,filename,row.names = FALSE)
      }
    }
    
  )
  
})


shinyApp(ui = ui, server = server)
