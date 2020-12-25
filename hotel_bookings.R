
library(scales)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny) 
library(shinydashboard)
library(DataExplorer)
library(zoo)



hotel <- read.csv(file.choose(),stringsAsFactors = FALSE, header = TRUE)

str(hotel)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Hotel Booking Demand", titleWidth=350)  

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
    title = "Total Income of hotels as per rooms"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("priceofroom", height = "300px")
  )
  
  ,box(
    title = "Total Bookings made in each Quarter of year"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("monthlybookingscount", height = "300px")
  ) 
 

)



# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='green')

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #some data manipulation to derive the values on the boxes on top
  total.people <- sum(hotel$adults)+sum(hotel$children, na.rm = TRUE)+sum(hotel$babies)
  
  total.canceled <- nrow(filter(hotel,reservation_status=='Canceled'))
  
  total.weekendnights<-sum(hotel$stays_in_weekend_nights)
  
  total.country <- nlevels(factor(hotel$country))
  
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(total.people, format="d", big.mark=',')
      ,'Total people lived at hotel'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "light-blue")
  })
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(total.weekendnights, format="d", big.mark=',')
      ,'Total Weekend nights Stayed'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "navy")
  })
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(total.canceled, format="d", big.mark=',')
      ,'Total Cancellation Made'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "green")
  })
  
  output$value4 <- renderValueBox({
    
    valueBox(
      formatC(total.country, format="d", big.mark=',')
      ,'Total Countries Guests visited from'
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "red")
  })
  

  hotel$adr2 <- hotel$adr/(hotel$adults+hotel$children)
  full_guest_data <- filter(hotel,is_canceled==0)
  room_prices <- full_guest_data %>% select(hotel,reserved_room_type, adr2)
  room_prices <- room_prices[order(room_prices$reserved_room_type),]
  
  output$priceofroom <- renderPlot({ 
    ggplot(room_prices, aes(x = reserved_room_type, y = adr2, fill = hotel)) + 
      geom_boxplot(position = position_dodge()) + 
      labs(title = "Average daily rate as per hotel rooms",
           x = "Type of room",
           y = "AVG ADR") + theme_classic()
  }) 
  
  
  #converted the date series into quarter to understand which quarter had positive impact on the hotel bookings
  
  hotel$reservation_status_date<-as.yearqtr(as.Date(hotel$reservation_status_date,"%Y-%m-%d"))
  
   output$monthlybookingscount <- renderPlot({
     ggplot(hotel, aes(factor(reservation_status_date))) +
       geom_line(stat = 'count', aes(group = hotel, colour = factor(hotel))) +
       geom_point(stat = 'count',aes(group = hotel, colour = factor(hotel)))+
       labs(title = "Booking Status by Every Quarter",
            x = "Quarters of year",
            y = "Count",color = "Hotel\n") +
       theme(axis.text.x = element_text(angle = 30, vjust = 0.7, hjust=0.70))
    
  })   
   
  
  
  
}

shinyApp(ui, server)

