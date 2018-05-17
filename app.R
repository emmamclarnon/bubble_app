#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("shiny")
#install.packages("plotly")
library(dplyr)
library(shiny)
library("shinythemes")#themes for dashboard
library(DT)#text analysis
#install.packages("shinydashboard")
library(shinydashboard)#dashboard
library(plotly)
library(bubbles)
library(lubridate)
library(readr)

setwd("C:/Users/AT003502/Desktop/datasciacc/deskpro/")

#function to import data and get rid of nas
table_import <- function (dataframe){
  df<-read.csv(dataframe)
  df<-df[df$Referrer.URL!="n/a",] %>%
    arrange((Service))
  
  df$Timestamp<-as.POSIXct(df$Timestamp, format="%d/%m/%Y %H:%M")
  return (df)
}

###dummy new ticket data - simulating new data coming in
time.data<-read_csv("DeskPro.csv")
time.data %>%
  transmute(
    submit_datetime = dmy_hm(Timestamp)
    ,submit_date = date(submit_datetime)
    ,submit_hour = hour(submit_datetime) %/%0.5 #summarise every 30 mins
    ,Service = Service
    ,hits = Count
  ) %>%
  group_by(
    Service
    ,submit_date
    ,submit_hour
  ) %>%
  summarise(
    hour_hits = sum(hits)
    
  ) %>%
  arrange(
    submit_hour,
    submit_date
    ,Service
  ) %>%
  filter(
    row_number() == n()
  ) -> events.data2

#sample 1000 rows of data
sub2<-events.data2[sample(nrow(events.data2),1000),]
date=events.data2$submit_date

#select the most recent dates for each service
sub_recent=sub2 %>%
  group_by(Service)%>%
  slice(which.max(submit_date))

hourly_agg = sub_recent


###historical data to get median values - acts as sliding window
df= read.csv("DeskPro.csv", header=T)
str(df)
calendar=read.csv("calendar.csv", header=F)
names(calendar)=c("date_hour")
calendar$date_hour = strptime(calendar$date_hour, format="%d/%m/%Y %H:%M")
df$date_hour = strptime(df$Timestamp, format='%d/%m/%Y %H:%M')
df=merge(df, calendar, all.x=TRUE)
df$Count[is.na(df$Count)] = 0


df$time = strftime(df$date_hour ,format="%H")
df$time = as.numeric(df$time)
df$date = strptime(df$date_hour, format='%Y-%m-%d')


df$date=as.POSIXct(df$date)
df$date_hour=as.POSIXct(df$date_hour)
df<- df %>% mutate(year = year(date),
                   month = month(date, label=TRUE),
                   day = day(date),
                   wday=wday(date, label=T))

library(modeest)
df$date<-ymd(df$date)

#get median value for all historical data and choose most recent date for each service
df_average<-df %>%
  filter (!is.na(Service))%>%
  group_by(Service, date) %>%
  summarise(
    mode = mlv (Count, method= 'mfv')[['M']],
    med = median(Count, na.rm=T),
    mean=mean(Count, na.rm=T),
    total =sum(Count, na.rm=T)) %>%
  slice(which.max(date))

df_url<-df %>%
  filter (!is.na(Service))%>%
  transmute(
    url=Referrer.URL,
    Service= Service,
    Date = date,
    Count=Count) %>%
  group_by(Service, Date, url) %>%
  summarise(
    total =sum(Count, na.rm=T)) %>%
  slice(which.max(Date))

#join new data and old data so can compare median and count values
library(dplyr)
library(tidyverse)
join = df_average %>%
  filter (!is.na(Service))%>%
  left_join (hourly_agg, by = "Service")

join2 = join %>%
  mutate(test= med - hour_hits)

join2= join2 %>%
  mutate(flags = case_when(
    test ==0 ~ "greenyellow",
    test <0 ~ "red"))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title="Deskpro Dashboard"),
  dashboardSidebar(disable = F),#no extra pages so have disabled this so far
  dashboardBody(
    fluidRow(
      theme = shinythemes::shinytheme("cerulean"),
      box(status = "info", #solidHeader = TRUE,
          title = "Number of tickets raised per service",bubblesOutput("packagePlot", width = "100%", height = 600)),
      # ),
      box(selectInput("Service","Service",
                      c("All", unique(as.character(df_url$Service)))),
          dateRangeInput('dateRange', label = h4('Date Range'), start = "2018-01-01", end = Sys.Date()), #Calendar for the user to input date
          
          DT::dataTableOutput("table"))
    
      
      
      #box(width=4, footer="Hover over bars for date, time and volume of raised tickets. Drag cusor to zoom in, double click to zoom out.", plotlyOutput("ticketChart"))))
  
   )
  )  
)
#)
#)
#adds header and sidebar options - to add extra pages to the sidebar

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <-join2
  Service <-as.factor(join2$Service)
  output$packagePlot <- renderBubbles({
    order <- unique(join2$Service)
    df <- join2%>%
      
      group_by(Service, flags) %>%
      tally(hour_hits) %>%
      arrange(desc(n), tolower(Service)) %>%
      # Just show the top 60, otherwise it gets hard to see
      head(30)
    
    bubbles(df$n, df$Service, key = df$Service, color=df$flags)
    #bubbles(hourly_url_agg$Service, hourly_url_agg$Service, key = hourly_url_agg$Service)
  })
  
  #uses historical data
  serv <-df_url
  Service <-as.factor(df_url$Service)
  #work out the mean for reported issues
  #mean_issues<-(hourly_url_agg$flag2)
  # output$table1<-
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable(rownames=F,options=list(
    lengthMenu = list(c(3, 10, 15, 20), c('3', '10','15', '20' )),
    pageLength = 10,
    # 
    initComplete = JS(
      
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#038fd2', 'color': '#fff'});",
      "}")
  ),
  #if all not selected, filter by the service                                             
  {
    serv <-df_url
    #time.data$Service<-as.factor(time.data$Service)
    if (input$Service != "All") {
      serv <- serv[serv$Service == input$Service,]
    }
    serv
    df_url=df_url[df_url$Date>=(input$dateRange[1]) & df_url$Date<=(input$dateRange[2]),]
    })
  

  #format the table using reported issues column, any value above the mean
  #colour it in red
  #%>%formatStyle(
  #  'flags',
  #  backgroundColor = styleEqual(unique(join2$flags), c("red","white"))
  )
 # )
#)

  
# output$ticketChart<-renderPlotly({
 #   plot_ly(df,
 #           x=df$date,
  #          y=df$Count,
  #          type="scatter",
  #          mode="lines",
           # linetype= df$Service,
  #          showlegend = FALSE)%>%
      #hoverinfo="text",
      #text=paste(repo,"<br>",created_at,"<br> ",title),
      #marker=list(color=colStatus)) %>%
      
  #    layout(hovermode = "closest",
   #          xaxis=list(title="",tickangle=55, ticklen=3, dtick=15, ticks="outside"), font=list(family="arial", size=10),
    #         yaxis=list(title="Volume of tickets raised per hour"), font=list(family="arial"),
   #          title="Deskpro Tickets",
   #          titlefont=list(size=16, family="arial"),
    #         margin=list(b=100,r=50)
    #  )
    
  #}) 
}
# Run the application 
shinyApp(ui = ui, server = server)

