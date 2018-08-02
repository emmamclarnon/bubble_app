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
library(r2d3)
library(modeest)

options(digits=3)
#library(shiny.router)
#setwd("C:/Users/AT003502/Desktop/datasciacc/js")

#function to import data and get rid of nas
#table_import <- function (dataframe){
# df<-read.csv(dataframe)
# df<-df[df$Referrer.URL!="n/a",] %>%
#   arrange((Service))

#  df$Timestamp<-as.POSIXct(df$Timestamp, format="%d/%m/%Y %H:%M")
#  return (df)
#}



###historical data to get median values - acts as daily sliding window
df= read.csv("old_24.csv", header=T)
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


#get average values for all historical data, works out daily median value per service 
#and chooses most recent date for each service - sort of daily rolling average
#this table is used for bubble chart
df_average<-df %>%
  filter (!is.na(Service),Service!="unknown")%>%
  group_by(Service, date) %>%
  summarise(
    mode = (round(mlv (Count, method= 'mfv')[['M']],2)),
    med = (round(median(Count, na.rm=T),2)),
    mean=(round(mean(Count, na.rm=T),2)),
    total =(round(sum(Count, na.rm=T),2)))%>%
  slice(which.max(date))

#df for DT in tab 2 - shows urls as well as services
#this table is used for the data displayed in tab 2 of the dashboard
df_url<-df %>%
  filter (!is.na(Service),
          Referrer.URL!="n/a",
          Referrer.URL!="/home",
          Referrer.URL!="None",
          Service!="unknown")%>%
  transmute(
    Service= Service,
    Date = date,
    URL=Referrer.URL,
    Count=Count) %>%
  group_by(Service, Date, URL) %>%
  summarise(
    Total =(sum(Count, na.rm=T)))#%>%#,
   # Mean = (round(mean(Count, na.rm=T),2)),
    #Mode = (round(mlv (Count, method= 'mfv')[['M']],2)),
    #Median= (round(median(Count, na.rm=T),2)))%>%
  #slice(which.max(Date))

df_url <- arrange(df_url, desc(Total))
df_url<-arrange(df_url, (Service))
# df_url<-df_url %>%
#   filter(url!="n/a")
# df_url<-df_url %>%
#   filter(url!="/home")
# df_url<-df_url %>%
#   filter(url!="None")
# df_url<-df_url %>%
#   filter(Service!="unknown")
#dummy data coming in
#this is to show what happens when "new" data is consumed - e.g. a 24 hour period of data
#takes data in and aggregates it on hourly basis - works out number of tickets raised per 30 mins, per service
time.data<-read.csv("new_24.csv",header=T)

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
    ,submit_hour = hour(submit_datetime) #summarise numer of tickets to every 30 mins
    ,month = month(submit_datetime)
    ,Service = Service
    ,hits = Count
  ) %>%
  filter (!is.na(Service),Service!="unknown")%>%
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

time.data %>%
  transmute(
    submit_datetime = dmy_hm(Timestamp)
    ,Submit_date = date(submit_datetime)
    ,Submit_hour = hour(submit_datetime) #summarise numer of tickets to every 30 mins
    ,month = month(submit_datetime)
    ,Service = Service
    ,hits = Count,
    URL=Referrer.URL
  )%>% filter (!is.na(Service),Service!="unknown")%>%
  
  group_by(
    Service
    ,Submit_date
    ,Submit_hour
    ,URL
  ) %>%
  summarise(
    Tickets_per_hour = sum(hits)
    
  ) %>%
  arrange(
    desc(Submit_date),
    desc(Submit_hour)
    ,Service
  ) %>%
  filter(
    !is.na(Service),
    Service!="unknown",
    URL!="n/a",
    URL!="/home",
    URL!="None",
    URL!="unknown",
    row_number() == n()
  )-> bub_urls
#events.dat2 = events.data2 %>%
#  group_by(
#   Service
#    ,submit_date
#   ,submit_hour) %>%
#  slice(which.max(month))

#sample 1000 rows of data
#sub2<-events.data2
#date=events.data2$submit_date

#select the most recent dates for each service
#hourly_agg=events.data2 %>%
  #group_by(Service)%>%
  #slice(which.max(submit_hour))


df %>%
  group_by(
    Service,year, month, day,time,date
  ) %>%
  summarise(
    hour_hit = sum(Count)
  ) %>%
  group_by(
    Service#,
    # hour_hit
  ) %>%
  summarise(
    mean=(round(mean(hour_hit),2)),
    mode=(round(mlv(hour_hit,method='mfv')[['M']],2)),
    median=(round(median(hour_hit),2))
  ) -> df_average_2

events.data2 %>%
  rename(hits=hour_hits) %>%
  group_by(
    Service
  ) %>%
  summarise(
    hour_hits = mean(hits)
  ) -> hourly_agg_2



#hourly_agg = sub_recent
#merging the tables of historical data and the 24 hour data so can compare average and count values

join = df_average_2 %>%
  filter (!is.na(Service))%>%
  inner_join (hourly_agg_2, by = "Service") %>%
  rename(med=median)

join2_mean= join %>%
  select(Service, mean, hour_hits)

join2_mean=join2_mean %>%
  mutate(test = (mean)-hour_hits)
join2_mean= join2_mean %>%
  mutate(flags = case_when(
    test >0 ~ "lightgray",
    test <0 ~ "red",
    TRUE ~ "lightgray"))
join2_mean=join2_mean%>%
  filter(flags=="red")

write.csv(join2_mean, "join2_mean.csv", row.names=F)

join2_mean=read.csv("join2_mean.csv", header=T)
order_mean <- unique(join2_mean$Service)


bub_mean <- join2_mean%>%
  group_by(Service, flags) %>%
  tally(hour_hits) %>%
  arrange(desc(n), tolower(Service)) %>%
  # Just show the top 60, otherwise it gets hard to see
  head(90)
bub_mean= bub_mean %>%
  select(Service,n) %>%
  rename(id=Service,
         value=n)
write.csv(bub_mean, "bub_mean.csv", row.names=F)


join2_med= join %>%
  select(Service, med, hour_hits)

join2_med=join2_med %>%
  mutate(test = (med)-hour_hits)
join2_med= join2_med %>%
  mutate(flags = case_when(
    test >0 ~ "lightgray",
    test <0 ~ "red",
    TRUE ~ "lightgray"))
join2_med=join2_med%>%
  filter(flags=="red")

write.csv(join2_med, "join2_med.csv", row.names=F)

join2_med=read.csv("join2_med.csv", header=T)
order_med <- unique(join2_med$Service)


bub_med <- join2_med%>%
  group_by(Service, flags) %>%
  tally(hour_hits) %>%
  arrange(desc(n), tolower(Service)) %>%
  # Just show the top 60, otherwise it gets hard to see
  head(90)
bub_med= bub_med %>%
  select(Service,n) %>%
  rename(id=Service,
         value=n)
write.csv(bub_med, "bub_med.csv", row.names=F)


join2_mode= join %>%
  select(Service, mode, hour_hits)

join2_mode= join2_mode %>%
  mutate(test = (mode)-hour_hits)
join2_mode=  join2_mode %>%
  mutate(flags = case_when(
    test >0 ~ "lightgray",
    test <0 ~ "red",
    TRUE ~ "lightgray"))
join2_mode= join2_mode%>%
  filter(flags=="red")

write.csv( join2_mode, " join2_mode.csv", row.names=F)

join2_mode=read.csv(" join2_mode.csv", header=T)
order_mode <- unique( join2_mode$Service)


bub_mode <-  join2_mode%>%
  group_by(Service, flags) %>%
  tally(hour_hits) %>%
  arrange(desc(n), tolower(Service)) %>%
  # Just show the top 60, otherwise it gets hard to see
  head(90)
bub_mode= bub_mode %>%
  select(Service,n) %>%
  rename(id=Service,
         value=n)
write.csv(bub_mode, "bub_mode.csv", row.names=F)


#r2d3(data = read.csv("bub1.csv"), d3_version = 4, script = ("bubbles.js"))




# join2 = join %>%
#   mutate(test= med - hour_hits)
# 
# join2= join2 %>%
#   mutate(flags = case_when(
#     test >=0 ~ "white",
#     test <0 ~ "red",
#     TRUE ~ "white"))

#add the additional data to the table to include for next time
#df.append <- df %>%
#  rbind(time.data) %>%
# filter (!is.na(date_hour))

#df.append <- df.append %>%
#  select(Timestamp, Service, Referrer.URL, Ticket, Count)

#write to the csv
#write.table(df.append, "test2.csv", sep = ",", col.names = F ,append = T)

# Define UI for application that draws a histogram
# Define UI for application that draws a histogram

ui <- 
  dashboardPage(
    dashboardHeader(title="Deskpro Dashboard"),
    dashboardSidebar(#radioButtons("radio", label=("Choose average type:"),
                                  #choices=list("Mean"=1, "Median"=2, "Mode" =3)#, selected=1
    ##),
    #sliderInput("threshold", "Threshold value:", min=0, max=100, value=0,
     #           step=10),
    sidebarMenu(
      
      menuItem("Mean", tabName = "mean"),
      menuItem("Median", tabName="median"),
      menuItem("Mode", tabName="mode"),
      menuItem("URL Table", tabName="urls")
     # menuItem(bookmarkButton())
    )),
    dashboardBody(
      tabItems(
        tabItem("mean",
                tags$style(HTML("
                                
                                
                                .box.box-solid.box-primary>.box-header {
                                color:#fff;
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
                  box(width=12,status = "primary", solidHeader = TRUE,
                      title = "Number of tickets raised per service",d3Output("packagePlot_mean", width = "100%", height = 700)),
                  # ),
                  fluidRow(box(width =11, solidHeader=T,
                               # dateRangeInput('dateRange', label = h4('Date Range'), start = "2018-01-01", end = Sys.Date()), #Calendar for the user to input date
                               
                               DT::dataTableOutput("results_mean")
                  ))
                  #box(htmlOutput("x_value"),
                  #   verbatimTextOutput("selected_rows"))
                )
                ),
        
        tabItem("median",
                tags$style(HTML("
                                
                                
                                .box.box-solid.box-primary>.box-header {
                                color:#fff;
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
                  box(width=12,status = "primary", solidHeader = TRUE,
                      title = "Number of tickets raised per service",d3Output("packagePlot_med", width = "100%", height = 700)),
                #   ),
                  fluidRow(box(width =11, solidHeader=T,
                                #dateRangeInput('dateRange', label = h4('Date Range'), start = "2018-01-01", end = Sys.Date()), #Calendar for the user to input date
                               
                               DT::dataTableOutput("results_med")
                  ))
                  #box(htmlOutput("x_value"),
                  #   verbatimTextOutput("selected_rows"))
                )
                ),
        tabItem("mode",
                tags$style(HTML("
                                
                                
                                .box.box-solid.box-primary>.box-header {
                                color:#fff;
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
                  box(width=12,status = "primary", solidHeader = TRUE,
                      title = "Number of tickets raised per service",d3Output("packagePlot_mode", width = "100%", height = 700)),
                  # ),
                  fluidRow(box(width =11, solidHeader=T,
                               # dateRangeInput('dateRange', label = h4('Date Range'), start = "2018-01-01", end = Sys.Date()), #Calendar for the user to input date
                               
                               DT::dataTableOutput("results_mode")
                  ))
                  #box(htmlOutput("x_value"),
                  #   verbatimTextOutput("selected_rows"))
                )
                ),
        tabItem("urls",
                box(width =12, solidHeader=T,selectInput("Service","Service", 
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
#adds header and sidebar options - to add extra pages to the sidebar

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  options(digits=3)

        
   output$packagePlot_mean<- renderD3({  
  # write.csv(bub, "bub.csv", row.names=F)
    r2d3(data = read.csv("bub_mean.csv"), d3_version = 4, script = ("bubbles.js"))
    })
    output$packagePlot_med<- renderD3({  
      # write.csv(bub, "bub.csv", row.names=F)
      r2d3(data = read.csv("bub_med.csv"), d3_version = 4, script = ("bubbles.js"))
    })
    
    output$packagePlot_mode<- renderD3({  
      # write.csv(bub, "bub.csv", row.names=F)
      r2d3(data = read.csv("bub_mode.csv"), d3_version = 4, script = ("bubbles.js"))
    })
    
    
   
  #output$packagePlot<-renderD3({
   # if (input$radio==1){
     # r2d3(data = read.csv("bub1.csv"), d3_version = 4, script = ("bubbles.js"))
#}
    
  #  if (input$radio==2){
   #   r2d3(data = read.csv("bub2.csv"), d3_version = 4, script = ("bubbles.js"))
    #}
    
   # if (input$radio==3){
    #  r2d3(data = read.csv("bub3.csv"), d3_version = 4, script = ("bubbles.js"))
   # }
    
 # })
  #plot=r2d3(floor(runif(input$radio==1)),data = read.csv("bub.csv"), d3_version = 4,  script = ("bubbles2.js"))
  #plot=r2d3(floor(runif(input$radio==2)),data = read.csv("bub.csv"), d3_version = 4,  script = ("bubbles2.js"))
  #plot=r2d3(floor(runif(input$radio==3)),data = read.csv("bub.csv"), d3_version = 4,  script = ("bubbles2.js"))
  

  
  #output$packagePlot<- renderD3({
  #  r2d3(floor(runif(input$radio)),data = read.csv("bub.csv"), d3_version = 4,  script = ("bubbles.js"),container="svg",viewer=c("browser") )
    #})
  

#browser()
 
  #output$packagePlot=plot
  # output$packagePlot<-(p_plot)  
  #})
  
  
  # Service <-(join2$Service)
  #  output$packagePlot <- renderD3({""})
  #output$packagePlot <- renderD3({
  
  #   join2=read.csv("join2.csv", header=T)
  #  order <- unique(join2$Service)
  #  
  
  #  bub <- join2%>%
  #   group_by(Service, flags) %>%
  #   tally(hour_hits) %>%
  #  arrange(desc(n), tolower(Service)) %>%
  #     # Just show the top 60, otherwise it gets hard to see
  #  head(90)
  #  bub= bub %>%
  #  select(Service,n) %>%
  #  rename(id=Service,
  #  value=n)
  # bub=as.data.frame(bub)
  # write.csv(bub, "bub.csv", row.names=F)
  # r2d3(data = read.csv("bub.csv"), d3_version = 4, script = "bubbles.js")
  
  #bubbles=bubbles(bub$n, bub$Service, key = bub$Service, color=bub$flags, tooltip =(bub$n) )
  
  
  #   #bubbles(hourly_url_agg$Service, hourly_url_agg$Service, key = hourly_url_agg$Service)
  #  })
  
  
  
  Service <-as.factor(df_url$Service)
  #work out the mean for reported issues
  #mean_issues<-(hourly_url_agg$flag2)
  # output$table1<-
  # Filter data based on selections
  
  output$table <- DT::renderDataTable(DT::datatable(rownames=F,options=list(
    lengthMenu = list(c(3, 10, 15, 20), c('3', '10','15', '20' )),
    pageLength = 10,
    autoWidth=F,
    scrollX=T,
    
    # 
    initComplete = JS(
      
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#038fd2', 'color': '#fff'});",
      "}")
  ),
  #if all not selected, filter by the service                                             
  {
    df_url
    
    ###filter by radio buttons
    
    # if (input$radio==1){
    #   df_url= df_url %>%
    #     select(Service, Date, Mean, Total)
    #  }
    
    # df_url 
    
    # if (input$radio==2){
    #   df_url= df_url %>%
    #     select(Service, Date, Median, Total)
    # }
    
    # df_url
    #if (input$radio==3){
    #  df_url= df_url %>%
    #    select(Service, Date, Mode, Total)
    # }
    
    df_url
    #time.data$Service<-as.factor(time.data$Service)
    if (input$Service != "All"){
      df_url <- df_url[df_url$Service == input$Service,]
    }
    df_url
    
    df_url=df_url[df_url$Date>=(input$dateRange[1]) & df_url$Date<=(input$dateRange[2]),]
    df_url
    
  })
  
  )
  
  wc_click <- reactive({
    a <- input$clickedValue
    print(a)
  })
  data <- reactive({ 
    val <- wc_click()
    if(!is.null(val))
    {
      data <-filter(bub_urls,Service==val)
      data
      
    }
  })
  
  updateSelectInput(session, inputId=a)
  
  output$results_mean <- DT::renderDataTable(DT::datatable(rownames=F,options=list(
    lengthMenu = list(c(10, 20,30,40,50,60,70,80,90,100), c('10','20','30','40','50','60','70','80','90','100' )),
    pageLength = 10,
    autoWidth=F,
    scrollX=T,
    # 
    initComplete = JS(
      
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#038fd2', 'color': '#fff'});",
      "}")
  ),
  data()
  
  )
  )
  
  output$results_med <- DT::renderDataTable(DT::datatable(rownames=F,options=list(
    lengthMenu = list(c(10, 20,30,40,50,60,70,80,90,100), c('10','20','30','40','50','60','70','80','90','100' )),
    pageLength = 10,
    autoWidth=F,
    scrollX=T,
    # 
    initComplete = JS(
      
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#038fd2', 'color': '#fff'});",
      "}")
  ),
  data()
  
  )
  )
  
  output$results_mode <- DT::renderDataTable(DT::datatable(rownames=F,options=list(
    lengthMenu = list(c(10, 20,30,40,50,60,70,80,90,100), c('10','20','30','40','50','60','70','80','90','100' )),
    pageLength = 10,
    autoWidth=F,
    scrollX=T,
    # 
    initComplete = JS(
      
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#038fd2', 'color': '#fff'});",
      "}")
  ),
  data()
  
  )
  )
  
}
# Run the application 
shinyApp(ui = ui, server = server)#, enableBookmarking="url")
