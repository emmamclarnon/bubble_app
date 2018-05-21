#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(shiny)
library("shinythemes")#themes for dashboard
library(DT)
library(shinydashboard)#dashboard
library(plotly)
library(bubbles)
library(lubridate)
library(readr)
library(dplyr)
library(tidyverse)
setwd("C:/Users/AT003502/Desktop/datasciacc/deskpro/")

#function to import data and get rid of nas
#table_import <- function (dataframe){
 # df<-read.csv(dataframe)
 # df<-df[df$Referrer.URL!="n/a",] %>%
 #   arrange((Service))
  
#  df$Timestamp<-as.POSIXct(df$Timestamp, format="%d/%m/%Y %H:%M")
#  return (df)
#}



###historical data to get median values - acts as daily sliding window
df= read.csv("one_month.csv", header=T)
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
                   month = month(date),
                   day = day(date),
                   wday=wday(date, label=T))

df$date<-ymd(df$date)

library(modeest)
#get median value for all historical data and choose most recent date for each service and works out daily median value
df_average<-df %>%
  filter (!is.na(Service))%>%
  group_by(Service, date) %>%
  summarise(
    mode = mlv (Count, method= 'mfv')[['M']],
    med = median(Count, na.rm=T),
    mean=mean(Count, na.rm=T),
    total =sum(Count, na.rm=T)) %>%
  slice(which.max(date))

#df for DT
df_url<-df %>%
  filter (!is.na(Service))%>%
  transmute(
    url=Referrer.URL,
    Service= Service,
    Date = date,
    Count=Count) %>%
  group_by(url,Service, Date) %>%
  summarise(
    Total =sum(Count, na.rm=T),
    Mean = mean(Count, na.rm=T),
    Mode = mlv (Count, method= 'mfv')[['M']],
    Median= median(Count, na.rm=T))%>%
  slice(which.max(Date))

#dummy data coming in
time.data<-read.csv("old_24.csv",header=T)

time.data$date_hour = strptime(time.data$Timestamp, format='%d/%m/%Y %H:%M')
time.data=merge(time.data, calendar, all.x=TRUE)
time.data$Count[is.na(time.data$Count)] = 0


time.data$time = strftime(time.data$date_hour ,format="%H")
time.data$time = as.numeric(time.data$time)
time.data$date = strptime(time.data$date_hour, format='%Y-%m-%d')


time.data$date=as.POSIXct(time.data$date)
time.data$date_hour=as.POSIXct(time.data$date_hour)
time.data<- time.data %>% mutate(year = year(date),
                                 month = month(date),
                                 day = day(date),
                                 wday=wday(date, label=T))

time.data$date<-ymd(time.data$date)


time.data %>%
  transmute(
    submit_datetime = dmy_hm(Timestamp)
    ,submit_date = date(submit_datetime)
    ,submit_hour = hour(submit_datetime) %/%0.5
    ,month = month(submit_datetime)#summarise every 30 mins
    ,Service = Service
    ,hits = Count
  ) %>%
  group_by(
    Service
    ,submit_date
    ,submit_hour
    ,month
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

events.dat2 = events.data2 %>%
  group_by(
    Service
    ,submit_date
    ,submit_hour) %>%
  slice(which.max(month))

#sample 1000 rows of data
sub2<-events.data2
date=events.data2$submit_date

#select the most recent dates for each service
sub_recent=sub2 %>%
  group_by(Service)%>%
  slice(which.max(submit_date))

hourly_agg = sub_recent


#join new data and old data so can compare median and count values

join = df_average %>%
  filter (!is.na(Service))%>%
  left_join (hourly_agg, by = "Service")

# join2 = join %>%
#   mutate(test= med - hour_hits)
# 
# join2= join2 %>%
#   mutate(flags = case_when(
#     test >=0 ~ "white",
#     test <0 ~ "red",
#     TRUE ~ "white"))

#add the additional data to the table to include for next time
df.append <- df %>%
  rbind(time.data) %>%
  filter (!is.na(date_hour))

df.append <- df.append %>%
  select(Timestamp, Service, Referrer.URL, Ticket, Count)

#write to the csv
write.table(df.append, "test2.csv", sep = ",", col.names = F ,append = T)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title="Deskpro Dashboard"),
  dashboardSidebar(radioButtons("radio", label=("Choose average:"),
                                choices=list("Mean"=1, "Median"=2, "Mode" =3), selected=2
  ),
  sidebarMenu(
    menuItem("Services", tabName = "services"),
    menuItem("URL Table", tabName="urls")
  )),
  dashboardBody(
    tabItems(
      tabItem("services",
              tags$style(HTML("

                              
                              .box.box-solid.box-primary>.box-header {
                              color:#ffffff;
                              background:	#007BA7;                    }
                              
                              .box.box-solid.box-primary{
                              border-bottom-color:#ffffff;
                              border-left-color:#ffffff;
                              border-right-color:#ffffff;
                              border-top-color:#ffffff;
                              }
                              
                              ")),
              fluidRow(
                theme = shinythemes::shinytheme("cerulean"),
                box(status = "primary", solidHeader = TRUE,
                    title = "Number of tickets raised per service",bubblesOutput("packagePlot", width = "100%", height = 600))
                # ),
              )
    ),
    tabItem("urls",
            box(solidHeader=T,selectInput("Service","Service", 
                                          c("All", unique(as.character(df_url$Service)))),
                dateRangeInput('dateRange', label = h4('Date Range'), start = "2018-01-01", end = Sys.Date()), #Calendar for the user to input date
                
                DT::dataTableOutput("table"))
            
            )

   )
  )  
)
#)
#)
#adds header and sidebar options - to add extra pages to the sidebar

# Define server logic required to draw a histogram
server <- function(input, output) {

  # Service <-(join2$Service)
   output$packagePlot <- renderBubbles({
     if (input$radio==1){
       join2= join %>%
         select(Service, date, mean, hour_hits)
       join2=join2 %>%
         mutate(test = mean-hour_hits)
       join2= join2 %>%
         mutate(flags = case_when(
           test ==0 ~ "lightgray",
           test <0 ~ "red",
           TRUE ~ "lightgray"))
       join2
     } 
     if (input$radio==2){
       join2= join %>%
         select(Service, date, med, hour_hits)
       join2=join2 %>%
         mutate(test = med-hour_hits)
       join2= join2 %>%
         mutate(flags = case_when(
           test ==0 ~ "lightgray",
           test <0 ~ "red",
           TRUE ~ "lightgray"))
       join2
       
     }
     if (input$radio==3){
       join2= join %>%
         select(Service, date, mode, hour_hits)
       join2=join2 %>%
         mutate(test = mode-hour_hits)
       join2= join2 %>%
         mutate(flags = case_when(
           test ==0 ~ "lightgray",
           test <0 ~ "red",
           TRUE ~ "lightgray"))
     
     join2
   }
     join2
     order <- unique(join2$Service)
     bub <- join2%>%
       group_by(Service, flags) %>%
       tally(hour_hits) %>%
       arrange(desc(n), tolower(Service)) %>%
  #     # Just show the top 60, otherwise it gets hard to see
       head(30)
  #
     bubbles(bub$n, bub$Service, key = bub$Service, color=bub$flags, tooltip =(bub$n) )
  #   #bubbles(hourly_url_agg$Service, hourly_url_agg$Service, key = hourly_url_agg$Service)
   })

  #uses historical data
  df_url
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
    df_url
    
    if (input$radio==1){
        df_url= df_url %>%
             select(Service, Date, Mean, Total)
    }
    
    df_url
    
    if (input$radio==2){
         df_url= df_url %>%
             select(Service, Date, Median, Total)
    }
    
    df_url
    if (input$radio==3){
         df_url= df_url %>%
             select(Service, Date, Mean, Total)
    }
    
    df_url
    #time.data$Service<-as.factor(time.data$Service)
    if (input$Service != "All"){
      df_url <- df_url[df_url$Service == input$Service,]
    }
    df_url
  
    df_url=df_url[df_url$Date>=(input$dateRange[1]) & df_url$Date<=(input$dateRange[2]),]
    
   
  })
  )

    # 
    # serv
    # if (input$radio==2){
    #   serv= df_url %>%
    #     select(Service, Date, Median, Total)
    # }
    # serv
    # if (input$radio==3){
    #   serv= df_url %>%
    #     select(Service, Date, Mode, Total)
    # }
    # serv


  #format the table using reported issues column, any value above the mean
  #colour it in red
#  %>%formatStyle(
  # 'col',
  #  backgroundColor = styleEqual(unique(df_url_color$url), c("white", "red"))
 # )
  #)
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

