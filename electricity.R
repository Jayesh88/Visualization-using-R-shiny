library(scales)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny) 
library(shinydashboard)
library(DataExplorer)
library(zoo)



electricity <- read.csv(file.choose(),stringsAsFactors = FALSE, header = TRUE)
factory<-read.csv(file.choose(),stringsAsFactors = FALSE, header = TRUE)
factory$SqFootage <- as.numeric(gsub(",","",factory$SqFootage))

str(electricity)

a<-mean(electricity$Electricity.Usage..kWh.)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Energy utilization Lavender-Feb", titleWidth=350)  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    
  )
)

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    
  )
)




frow1 <- fluidRow(
  valueBoxOutput("value1",width = 3)
  ,valueBoxOutput("value2",width = 3)
  ,valueBoxOutput("value3",width = 3)
  ,valueBoxOutput("value4",width = 3)
)


frow2 <- fluidRow(
  
  box(
    title = "Total square foot of each city"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("factorypersqfeet", height = "300px")
  )
  
  ,box(
    title = "Days of the month with highest Electricity consumption"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("peakday", height = "300px")
  ) 
  
  
)



# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='green')

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #some data manipulation to derive the values on the boxes on top
  
  avg.demand<-mean(electricity$Electricity.Usage..kWh.)
  avg.demand.per.sq.feet<-(1/factory[4,3])*mean(electricity$Electricity.Usage..kWh.)
  total.demand.for.month<-sum(electricity$Electricity.Usage..kWh.)
  peak.demand<-max(electricity$Electricity.Usage..kWh.)

  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(avg.demand, format="d", big.mark=',')
      ,'Average demand per month[kwh]'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "light-blue")
  })
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(avg.demand.per.sq.feet, format="d", big.mark=',')
      ,'Avg. demand per square feet[kwh]'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "navy")
  })
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(total.demand.for.month, format="d", big.mark=',')
      ,'Total Demand per month [kwh]'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")
  })
  
  output$value4 <- renderValueBox({
    
    valueBox(
      formatC(peak.demand, format="d", big.mark=',')
      ,'Peak Demand [kwh]'
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "red")
  })
  
  
  
  output$factorypersqfeet <- renderPlot({ 
    ggplot(factory, aes(x = factory$Factory, y = factory$SqFootage)) + 
      geom_bar(stat='identity') + 
      labs(title = "Factory per square foot",
           x = "Factory name",
           y = "Square footage") + theme_classic()
  }) 
  
  
  #converted the date series into quarter to understand which quarter had positive impact on the electricity bookings
  
  output$peakday <- renderPlot({
    ggplot(electricity, aes(electricity$Day,electricity$Electricity.Usage..kWh.)) +
      geom_line(stat = 'identity') +
      geom_point(stat = 'identity')+
      labs(title = "Peak time of the month",
           x = "Days",
           y = "Electricity usage")
    
  })   
  
  
  
  
}

shinyApp(ui, server)

