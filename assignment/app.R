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
                                         p("Our target audience is those who work in the healthcare, public health, and political fields for them to be able to visualize the data
                                           in an easy way that demonstrates the trends in vaccination rates for Covid 19."),
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
                                               helpText("This panel displays 2 different forms of information."),
                                               helpText("One is a bar graph that demonstrates total number of indivudals who completed the vaccination series by State."),
                                               helpText("The other is a Table that breaks down those who completed the vaccination series by age"),
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
                         mainPanel(tableOutput("table"))),
                tabPanel("Series Completed vs First Booster Dose",
                           sidebarLayout(
                             sidebarPanel(
                               h4("Analyze the Relation between Vaccination Series
                                  Complete and Receiving First Booster Dose in
                                  Different Age Range"),
                               p("You can choose a ", strong("State"), "you are interested
                                 in and choose the age range you want to analyze.\n"),
                               selectInput("plotstate", "Select State",
                                           choices = covidVax$Recip_State),
                               radioButtons("age", "Select Age Data",
                                            choices = c("5-12" = "05", "12-18" = "12",
                                                        "18-65" = "18", "65+" = "65")
                               ),
                             ),   
                             mainPanel(
                               textOutput("textcounty"),
                               plotOutput("plotcounty", width = "350%", height = "500px")
                             )
                           )  
                ),
                tabPanel("Age Groups taking a First Dose", 
                         fluidRow(sidebarPanel(p("Here you can anlayze how many of specifically first doses of the COVID vaccine have been taken by each individual age group. 
                                                 Each data set is grouped by", 
                                                 strong("State")), 
                                               p("Select the state you are interested in seeing"), 
                                               uiOutput("dropdownState")
                         ), 
                         mainPanel(plotOutput("plotState")))
                ),
                         
                tabPanel("Conclusion",
                         fluidRow(column(12, 
                                         h1("Conclusion"),
                                         br(),
                                         p("notable insight or pattern discovered in your project, link it to a chart/table/graph"),
                                         br(),
                                         p("broader implications of the insight"),
                                         br(),
                                         p("The dataset was resonable quality, it was organized and consistent. However, some issues were with
                                           the size of the dataset making it more challenging to work with."),
                                         p("This dataset gives unbiased results as it is focusing on reporting data about the number of individuals who are
                                         getting the different Covid-19 vaccinations across the US. Except for age,it does not contain identifiable information or
                                         factors like race. We do not think that this information will harm a specific population as the data is focused on the type of vaccine
                                           received and location. Also, the ages are widely ranged and is not singling out a specific age."),
                                         br(),
                                         p("Future ideas to adhance this project could be to get more specific on the data being used and portrayed to the user. 
                                           In addition to increasing interactions and better graphics such as adding an interactive map for the user.")
                                         
                                         ))
                )
                
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
      ggplot(aes(Recip_State, total, fill="salmon")) +
      geom_col() +
      labs(title = "Original Covid-19 Vaccine Series Completed by State", 
           x ="State", 
           y="Total Number of People who Completed the Series",
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
  #Maisie Plot Coding
  agecounty <- reactive({
    covidVax %>%
      filter(Recip_State == input$plotstate) %>%
      mutate(booster05 = Booster_Doses_5Plus - Booster_Doses_12Plus,
             booster12 = Booster_Doses_12Plus - Booster_Doses_18Plus,
             booster18 = Booster_Doses_18Plus- Booster_Doses_65Plus,
             booster65 = Booster_Doses_65Plus,
             series05 = Series_Complete_5Plus - Series_Complete_12Plus,
             series12 = Series_Complete_12Plus - Series_Complete_18Plus,
             series18 = Series_Complete_18Plus - Series_Complete_65Plus,
             series65 = Series_Complete_65Plus) %>%
      select(Recip_County, ends_with(input$age)) %>%
      drop_na()
  })
  output$plotcounty <- renderPlot({
    agecounty() %>% 
      ggplot(aes(x = !!sym(paste0("series", input$age)), 
                 y = !!sym(paste0("booster", input$age)),
                 col = Recip_County)) +
      geom_point() +
      labs(title = "The Relation between Completing the Series Vaccination and Receiving Booster Doses by County",
           x = "Series Completed", y = "Booster Doses", col = "County")
  })
  output$textcounty <- renderText({
    paste("The state you selected contains:",nrow(agecounty()),"counties.\n")
  })
  #------------------------------------------------------------------------------------------------
  #Ki Plot Coding
  covidVax$zero5 <- covidVax$Administered_Dose1_Recip - covidVax$Administered_Dose1_Recip_5Plus
  covidVax$five12 <- covidVax$Administered_Dose1_Recip_5Plus - covidVax$Administered_Dose1_Recip_12Plus
  covidVax$twelve18 <- covidVax$Administered_Dose1_Recip_12Plus - covidVax$Administered_Dose1_Recip_18Plus
  covidVax$eighteen65 <- covidVax$Administered_Dose1_Recip_18Plus - covidVax$Administered_Dose1_Recip_65Plus
  covidVax$sixty.five <- covidVax$Administered_Dose1_Recip_65Plus
  
  pd.sum <- covidVax %>%
    group_by(State = Recip_State) %>%
    summarize("Age 0-5" = sum(zero5, na.rm = TRUE), "Age 5-12" = sum(five12, na.rm = TRUE),
              "Age 12-18" = sum(twelve18, na.rm = TRUE), "Age 18-65" = sum(eighteen65, na.rm = TRUE),
              "Age 65+" = sum(sixty.five, na.rm = TRUE))
  pd.sum2 <- pd.sum %>% 
    pivot_longer(cols = -State, names_to = "Age Range", values_to = "Total")
  
  output$dropdownState <- renderUI({
    selectInput("state2", "Choose State", 
                choices = unique(pd.sum2$State))
  })
  
  select_data2 <- reactive({
    pd.sum2 %>% 
      filter(State %in% input$state2)
  })
  
  output$plotState <- renderPlot({
    select_data2() %>%
      ggplot(aes(x = `Age Range`, y = Total, fill = State)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "Age Range", y = "Total", fill = "State") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = "purple")
  })
  #------------------------------------------------------------------------------------------------
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)
