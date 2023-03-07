library(shiny)
library(tidyverse)

ui <- fluidPage(
  #create multi-tab layout by using tabsetPanel
  tabsetPanel(
    #create "About" tab (first page). It contains the general information of 
    #the data, picture of the topics and the sample of our dataframe
    tabPanel("About", 
      sidebarPanel(h1("Vaccinations in the United States on 2/8/2023 Data"), 
                   p("Data are reported on ", strong("CDC COVID Data Tracker")),
                   p("The dataset contains 3283 observations and 57 variables"), 
                   img(alt = "vaccination", src = 
                         "https://www.news-medical.net/images/news/ImageForNews_734493_16717867507843586.jpg", 
                       height="80%", width="80%"), 
                   p("Here is small random sample of data: ")), 
      mainPanel(dataTableOutput("sample"))),
    
    #create "Plots" tab. It shows the relationship between the percent of 
    #people who completed primary series and the Percent of people completed 
    #primary series and booster based on the metro status. The metro status 
    #can be selected in the sidebar
    tabPanel("Plots", 
      sidebarPanel(p("You can analyze the relationship between the percent of 
                     people who completed primary series and the Percent of 
                     people completed primary series and booster based on ", 
                     strong("Metro status")), 
                   p("Select the metro status you are interested in"), 
        uiOutput("checkboxMetro")
      ), 
      mainPanel(plotOutput("plot"))), 
    
    #create "Tables" tab. It shows the average percent of the people who 
    #received primary series and the percent of the people who received both 
    #of the primary series and booster dose of the state you choose. The state 
    # can be chosen on the sidebar (can choose multiple states)
    tabPanel("Tables", 
      sidebarPanel(p("You can choose the ", strong("state "), "you are 
                     interested in"), 
                   p("The table on the right will show the average percent of 
                     the people who received primary series and the percent of 
                     the people who received both of the primary series and 
                     booster dose of the state you choose"), 
        uiOutput("checkboxState")
      ), 
      mainPanel(tableOutput("table")))
  )
)

server <- function(input, output) {
  #read the dataframe and filter out the NA values
  data <- read_delim("project_data.csv")
  data <- na.omit(data)
  
  #render the sample dataframe in the "About" tab
  output$sample <- renderDataTable({
    data %>% 
      sample_n(6)
    })
  
  #render the metro status selection in the input "checkbosMetro" in sidebar of 
  #"Plots" tab
  output$checkboxMetro <- renderUI({
    checkboxGroupInput("metro", "Choose Metro", 
                       choices = unique(data$Metro_status))
  })
  
  #create reactive function for metro status in the "Plots" tab
  select_data <- reactive({
    data %>% 
      filter(Metro_status %in% input$metro)
  })
  
  #use reactive function data and render the plot to the input "plot" in the 
  #"Plots" tab
  output$plot <- renderPlot({
    select_data() %>% 
      ggplot(aes(Series_Complete_Pop_Pct, Booster_Doses_Vax_Pct), col=input$metro) + 
      geom_point() + 
      labs(x = "Percent of people completed primary series", 
           y = "Percent of people completed primary series and booster") + 
      ggtitle("Percent of completed primary series versus both primary series and booster")
  })
  
  #render the state selection in the input "checkboxState" in sidebar of "Tables" 
  #tab
  output$checkboxState <- renderUI({
    checkboxGroupInput("states", "Choose State", 
                       choices = unique(data$Recip_State))
  })
  
  #create reactive function for state in the "Tables" tab
  select_state <- reactive({
    data %>% 
      filter(Recip_State %in% input$states)
  })
  
  #use reactive function data and render the plot to the input "table" in the 
  #"Tables" tab
  output$table <- renderTable({
    select_state() %>% 
      summarize(averge_primary_series  = mean(Series_Complete_Pop_Pct), 
                averge_booster_series  = mean(Booster_Doses_Vax_Pct))
      
  })
}

shinyApp(ui = ui, server = server)
