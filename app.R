require(shiny)
require(shinydashboard)
require(tidyverse)
require(DT)
 
# Reading Data set 1 for immigration
sampledata <- read.csv("data.csv")

# Modifying Data set 1
data_pie <- data.frame(col1 = character(), col2 = numeric(), col3 = numeric())
temp <- data.frame(col1 = character(), col2 = numeric(), col3 = numeric())
names(data_pie) <- c("Continent", "Population","Year")
print(data_pie)
for (i in 2011:2020){
  z = paste0("tot",as.character(i))
  y = sampledata[z]
  x <- aggregate(y,by=list(Category=sampledata$region), FUN=sum)
  names(x) <- c("Continent", "Population")
  temp = cbind(x,c(i,i,i,i,i,i))
  names(temp) <- c("Continent", "Population","Year")
  data_pie <- rbind(data_pie,temp)
}

# Reading Data set 2
sample2 <- read.csv("data2.csv")

# Modifying Data set 2
race <- c("White, non-Hispanic", "Black", "Hispanic", "Asian")
year <- c(2021,2020,2019,2018,2017,2016,2015,2014,2013,2012,2011, 2010, 2009)
sample2 <- sample2[sample2$Characteristic>=2009 & sample2$Characteristic<=2021,]
sample2 <- sapply(sample2, as.numeric)
data <- data.frame(rep(year,each=4),rep(race,times=13),c(t(sample2[,3:6])))
names(data) <- c("Year", "Ethnicity","Earning")
data$Ethnicity <- factor(data$Ethnicity, levels = c("Asian", "White, non-Hispanic", "Hispanic", "Black"))

#This will make sure when displaying options to select for Region and Earning
#It will not display duplicate values.
area_type_options2 <- unique(data$Ethnicity)
area_type_options <- unique(data_pie$Continent)


# Defining UI
ui <- dashboardPage(
  title="US immigration from 2011 to 2020 And Median Household Earning in the US by Different Ethnic Groups", 
  
  dashboardHeader(title = tags$a(href='https://worldpopulationreview.com/country-rankings/us-immigration-by-country',
                          tags$img(height="45", alt="NRS", src="flag.png"),
                          tags$script(
                            HTML(paste0('$(document).ready(function() {
                                         $("header").find("nav").append(\'<span class="myClass">',
                                         'US Immigration from 2011 to 2020 And Median Household Earning in the US by Different Ethnic Groups',
                                         '</span>\');
                                         })' 
                                        ))),
                          tags$head(tags$style(
                            HTML('.myClass {
                               font-size: 18px;
                               line-height: 50px;
                               text-align: left;
                               padding: 0 15px;
                               overflow: hidden;
                               color: white;
                               font-family: "Roboto", sans-serif !important; font-weight:400;
                               }')),
                            HTML('<link rel="stylesheet" href="http://fonts.googleapis.com/css?family=Roboto: 100,200,300,400,500,900">')))
                    ), #end of title
  
                    ## Creates the side menu of our project
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Earnings", tabName = "earning", icon = icon("line-chart")),
                        menuItem("Immigration", tabName = "immigration", icon = icon("line-chart")))
                      
                    ),

  # ------------------------------------------------------------------------------
  dashboardBody(
    
    #adding css file
    # tags$head(
    #   
    #   includeCSS("style.css")
    #   
    # ),
    # - Tab 1 - Earnings ------------------------------------------------------------------
    tabItems(
      tabItem(tabName = "earning",
              fluidPage(
                titlePanel("Earnings of Different Ethnic Groups"),
                
                br(),
                
                h4("The line graph demonstrats the",strong("Median Household Earnings of different Ethnic groups in the US from 2009 to 2020"), ". We can compare earning of each ethnic group one by one or all at once. Based on the data, we see that Asian Ethnic Groups have highest earnings compared to other Ethnic Groups."),
                br(),
                
                
                fluidRow(
                  # Selecting Region Type
                  column(4,
                         wellPanel(
                           helpText("Select your choice of indicators below:") ,
                           
                           br(),
                        #Check box allows the user to select multiple values, by default the selected value is Asian
                           checkboxGroupInput(inputId = "area_type2",
                                              label = "Ethnic Groups:",
                                              choices = area_type_options2,
                                              selected = "Asian"),
                           
                           br(),
                           # Slider for time range
                           sliderInput(inputId = "year_range2",
                                       label = "Year range:",
                                       min = 2009,
                                       max = 2021,
                                       value = c(2009,2021),
                                       step = NULL,
                                       sep = "", #This was to take out the comma in the year (eg. 2,004 = 2004)
                                       dragRange = TRUE),
                           
                           br(),
                           
                     
                           # Adding reference for the original data set in our project
                           h6("*The dataset has been taken from https://www.statista.com/statistics/1086359/median-household-income-race-us/")
                         )
                  ),
                  # - Plot ---------------------------------------------------------------
                  column(8,
                         wellPanel(
                           plotOutput("line_plot"),
                           br(),
                           DTOutput('tbl2')
                         )
                  ))
              )),
      
      
      
      # - Tab 2 - Immigration ------------------------------------------------------------------
      tabItem(tabName = "immigration",
              fluidPage(
                titlePanel("US Immigration from 2011 to 2020"),
                
                br(),
                
                h4("The line graph demonstrates",strong("Total Number of Immigrants from different regions Migrating to the US from 2011 to 2020"), ". Based on the visualization, the overall percentage of Asian immigrants has been higher compared to other continents."),
                br(),
                
                
                fluidRow(
                  
                  # - Selecting Region Type----------------------------------------------------------
                  column(4,
                         wellPanel(
                           helpText("Select your choice of indicators below:") ,
                           
                           br(),
                           #Check box allows the user to select multiple values, by default the selected value is North America.
                           checkboxGroupInput(inputId = "area_type",
                                        label = "Region type:",
                                        choices = area_type_options,
                                        selected = "North America"),
                           
                           br(),
                           # Slider for time range
                           sliderInput(inputId = "year_range",
                                       label = "Year range:",
                                       min = 2011,
                                       max = 2020,
                                       value = c(2011,2020),
                                       step = NULL,
                                       sep = "", #This was to take out the comma in the year (eg. 2,004 = 2004)
                                      dragRange = TRUE),
                           
                           br(),
                        
                        
                           # Adding reference for the original data set in our project
                           h6("The dataset has been taken from https://worldpopulationreview.com/country-rankings/us-immigration-by-country")
                         )
                  ),
                  # - Plot ---------------------------------------------------------------
                  column(8,
                         wellPanel(
                           plotOutput("line_plot_continent"),
                           br(),
                           DTOutput('tbl')
                           )
                  ))
              ))
    
    )
    ) #tabItems
  ) #dashboardBody
                    
  

# Define server
server <- function(input, output, session) {

  ## Data Selection for Immigration
  select_data <- reactive({ 
    data_pie %>%
      group_by(Continent, Population, Year) %>%
      filter(Continent %in% input$area_type &
               Year >= input$year_range[1] &
               Year <= input$year_range[2]) %>%
      droplevels()
  })
  ## Data Selection for Earnings
  select_data2 <- reactive({ 
    data %>%
      group_by(Ethnicity, Earning, Year) %>%
      filter(Ethnicity %in% input$area_type2 &
               Year >= input$year_range2[1] &
               Year <= input$year_range2[2]) %>%
      droplevels()
  })
  
  
  
  ## PLOT for Immigration
  output$line_plot_continent <- renderPlot({ 
      ggplot(data = select_data(),
             mapping = aes(x = Year, 
                           y = Population, 
                           colour = Continent)) +
        geom_line(size = 1.5) +
        geom_point(size = 4.5) +
        theme_minimal() +
        scale_x_continuous(breaks = 2011:2020) + ### This has to be changed manually
        #scale_y_continuous(label = comma) +
        expand_limits(y = 0) +
        theme(legend.position = "right",
              panel.grid.minor = element_blank())   
    
    
    })
  
  ## PLOT for Earning
  output$line_plot <- renderPlot({ 
    ggplot(data = select_data2(),
           mapping = aes(x = Year, 
                         y = Earning, 
                         colour = Ethnicity)) +
      geom_line(size = 1.5) +
      geom_point(size = 4.5) +
      theme_minimal() +
      scale_x_continuous(breaks = 2009:2021) + ### This has to be changed manually
      #scale_y_continuous(label = comma) +
      expand_limits(y = 0) +
      theme(legend.position = "right",
            panel.grid.minor = element_blank())   
    
    
  })
  # Displaying Data for Immigration
  output$tbl = renderDT(
    select_data(),
    extensions = 'Buttons',
    options = list(pageLength = 250,
                   dom = 'Bfrtip',
                   buttons = c('copy', 'csv', 'pdf', 'print')
    )
  )
  # Displaying Data for Earnings
  output$tbl2 = renderDT(
    select_data2(),
    extensions = 'Buttons',
    options = list(pageLength = 250,
                   dom = 'Bfrtip',
                   buttons = c('copy', 'csv', 'pdf', 'print')
    )
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)

