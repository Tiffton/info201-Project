library(shiny)
library(tidyverse)
covidVax <- read_delim("data/covid-data.csv")


ui <- fluidPage(
  titlePanel(title = h4("Covid Vaccination Data In The US", align = "center")),
  
  mainPanel(
    tabsetPanel(type = "tab",
                tabPanel("Introduction",
                         fluidRow(column(12,
                                         h1("Introducing The Data"),
                                         img(alt = "vaccination", src = 
                                               "https://www.news-medical.net/images/news/ImageForNews_734493_16717867507843586.jpg", 
                                             height="80%", width="80%"), 
                                         p("For this project, we are analyzing a data set from the",
                                           em("Center for Disease Control and Prevention"),
                                           " that tracks Covid 19 vaccinations across the United States. 
                                           The data provides information about the vaccines in counties across the fifty states. 
                                           Providing information such as if people completed the entire two-shot dose of the original covid vaccinations, one or the other,
                                           booster shots, etc."),
                                         br(),
                                         p("The data is reliable as it was taken from a respected and professional organization, the CDC. 
                                           The information in the data set makes sense and is organized."),
                                         br(),
                                         p("There are not any identifiable ethical issues as there is no identifiable information that is personal to an individual, so no private 
                                         information is breached. In addition, CDC gets its information from reports sent from clinics and hospitals."),
                                         br(),
                                         p("What we are doing in this project is analyzing the data set taken from the CDC, we want to organize it and 
                                         visualize the data so that it is easier for users to conceptualize what the information suggests."),
                                         br(),
                                         p("What we are doing in this project is analyzing the data set taken from the CDC, we want to organize it and visualize the
                                         data so that it is easier for users to conceptualize what the information suggests."),
                                         br(),
                                         br(),
                                         p("This data set is quite large, it has 3283 observations and 57 variables."),
                                         strong("Here is a sample of the data:"),
                                         br(),
                                         br(),
                                         tableOutput("sample")
                         )
                         
                         
                         )),
                
                tabPanel("State and Age",
                         fluidRow(sidebarPanel(width = 5,
                                               h4("Look Through The Age Groups & Different States"),
                                               helpText("This panel displays the number of individuals who completed their covid series by age in each County, State"),
                                               br(),
                                               helpText("Choose an age group for the table"),
                                               radioButtons("age_group", "Age Group",
                                                            choices = c("5Plus", "5-17", "12Plus", "18Plus", "65Plus"),
                                                            selected = c("5Plus")),
                                               br(),
                                               helpText("Choose which States you would like to include in the bar chart"),
                                               uiOutput("checkboxes")
                                               
                         ),
                         
                         br(),
                         column(7, 
                                plotOutput("Plot", width = "190%"),
                                strong(textOutput("age_text")),
                                tableOutput("dataTable")
                         )       
                         )),
                tabPanel("Metro vs Non-Metro", 
                         fluidRow(sidebarPanel(p("You can analyze the relationship between the percent of 
                     people who completed primary series and the Percent of 
                     people completed primary series and booster based on ", 
                                                 strong("Metro status")), 
                                               p("Select the metro status you are interested in"), 
                                               uiOutput("checkboxMetro")
                         ), 
                         mainPanel(plotOutput("plot"))), 
                ),
                tabPanel("Avgs: primary vs booster dose", 
                         sidebarPanel(p("You can choose the ", strong("state "), "you are 
                     interested in"), 
                                      p("The table on the right will show the average percent of 
                     the people who received primary series and the percent of 
                     the people who received both of the primary series and 
                     booster dose of the state you choose"), 
                                      uiOutput("checkboxState")
                         ), 
                         mainPanel(tableOutput("table")))
                
                #___ Add a comma above and add your tapPanel HERE
                
                
    )
  )
)

#Server inputs is below
#-----------------------------------------------------------
#Intro/showing sample of data
server <- function(input, output) {
  output$sample <- renderTable({
    covidVax %>% 
      head(10)
    
  })
  
  
  #--------------------------------------------------------------------------------------
  #Tiffany's Table and plot Tab for table that shows completed series in each state and select age
  
  datab <- reactive({
    covidVax %>% 
      group_by(Recip_State) %>% 
      select(Recip_State, Series_Complete_Yes, Series_Complete_Pop_Pct) %>% 
      filter(!is.na(Recip_State), !is.na(Series_Complete_Yes), !is.na(Series_Complete_Pop_Pct)) %>% 
      summarise(total = sum(Series_Complete_Yes)) %>% 
      filter(Recip_State %in% input$States)
  })
  
  output$checkboxes <- renderUI({
    checkboxGroupInput(
      "States", "Select States:",
      choices = unique(covidVax$Recip_State)
    )
  })
  
  output$Plot <- renderPlot({
    datab() %>%
      ggplot(aes(Recip_State, total, fill="blue")) +
      geom_col() +
      labs(title = "Total of Covid Doses Completed by State", 
           x ="State", 
           y="Number of People who Completed the Series",
           fill = "color" )
  })
  
  #--------------------------
  table_data <- reactive({
    if(input$age_group %in% "5Plus") {
      covidVax %>% 
        filter(!is.na(Recip_State)) %>% 
        group_by(Recip_State) %>% 
        select(Recip_County, Series_Complete_5Plus) %>% 
        filter(!is.na(Series_Complete_5Plus)) %>% 
        arrange(rank(Recip_State))
    } else 
      if (input$age_group %in% "5-17"){
        covidVax %>% 
          filter(!is.na(Recip_State)) %>% 
          group_by(Recip_State) %>% 
          select(Recip_County, Series_Complete_5to17) %>% 
          filter(!is.na(Series_Complete_5to17)) %>% 
          arrange(rank(Recip_State))
      }
    else
      if(input$age_group %in% "12Plus"){
        covidVax %>% 
          filter(!is.na(Recip_State)) %>% 
          group_by(Recip_State) %>% 
          select(Recip_County, Series_Complete_12Plus) %>% 
          filter(!is.na(Series_Complete_12Plus)) %>% 
          arrange(rank(Recip_State))
      }
    else
      if(input$age_group %in% "18Plus"){
        covidVax %>% 
          filter(!is.na(Recip_State)) %>% 
          group_by(Recip_State) %>% 
          select(Recip_County, Series_Complete_18Plus) %>% 
          filter(!is.na(Series_Complete_18Plus)) %>% 
          arrange(rank(Recip_State))
      }
    else
      if(input$age_group %in% "65Plus"){
        covidVax %>% 
          filter(!is.na(Recip_State)) %>% 
          group_by(Recip_State) %>% 
          select(Recip_County, Series_Complete_65Plus) %>% 
          filter(!is.na(Series_Complete_65Plus)) %>% 
          arrange(rank(Recip_State))
      }
  })
  
  output$dataTable <- renderTable({
    table_data()
    
  })
  
  output$age_text <- renderText({
    paste("You have selected:", input$age_group )
  })
  
  #------------------------------------------------------------------- --------------
  #Yvonne's plot coding
  data <- read_delim("project_data.csv")
  data <- na.omit(data)
  
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
  #--------------------
  #Table coding 
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
  
  #------------------------------------------------------------------------------------------------
  #Input youre next coding HERE
  
  
  
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)
