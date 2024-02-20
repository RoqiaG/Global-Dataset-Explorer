#####Libraries######
library(shiny)
library(shinydashboard)
library(plotly)
library(shinyjs)
library(bslib)
library(bsicons)
library(shiny)
library(ggplot2)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(stringr)
library(gridExtra)

######preprocessing############
# Read the dataset
dataset <- read.csv("D:/4th year/First Smester/IV/project/03-01-2021.csv")

# Summary and dimensions of the dataset
summary(dataset)
dim(dataset)

# Count duplicate rows
duplicate_count<-sum(duplicated(dataset))
print(duplicate_count)

# Seeing missing values
colSums(is.na(dataset))

# Remove unnecessary columns
dataset <- subset(dataset, select = -FIPS)
dataset <- subset(dataset, select = -Admin2)
dataset <- subset(dataset, select = -Lat)
dataset <- subset(dataset, select = -Long_)

# Replace NA values in specific columns
dataset$Incident_Rate <- replace(dataset$Incident_Rate , is.na(dataset$Incident_Rate),0)
dataset$Case_Fatality_Ratio  <- replace(dataset$Case_Fatality_Ratio  , is.na(dataset$Case_Fatality_Ratio ),0)

# Clean up 'Province_State' column
dataset$Province_State<- gsub("^\\s*$", "Missing", dataset$Province_State)
print(dataset$Province_State)

# Remove rows with specific values in 'Province_State'
dataset <- dataset[!(dataset$Province_State == "Missing"),]
dataset <- dataset[!(dataset$Province_State == "Unknown"),]

dataset$Last_Update <- as.Date(dataset$Last_Update)

##############################################################################

###########shiny###############


# Define the UI
ui <- dashboardPage(
  
  ######Dashboard Menu#####
  dashboardHeader(title = "Interactive Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Visualization", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Data Options", tabName = "data_options", icon = icon("sliders-h")),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      actionButton("toggle_button", "Dark /light  Mode")
    ),
   ## ####dark and light mode###########
    useShinyjs(),
    extendShinyjs(
      text = "shinyjs.toggleMode = function() { $('.skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper,.main-header,.skin-blue .main-header .logo,.skin-blue .main-header .navbar,.main-header .navbar .sidebar-toggle, .tab-pane active,.content-wrapper,.box,.box-header,.skin-blue .sidebar-menu>li.active>a, .skin-blue .sidebar-menu>li:hover>a').toggleClass('dark-mode'); }",
      functions = c("toggleMode")
    ),
    
    tags$style(
      HTML(
        "/* Dark Mode and Light Mode styles *\
        
          
         /* Dark Mode Styles */
         .skin-blue .left-side.dark-mode, .skin-blue .main-sidebar.dark-mode, .skin-blue .wrapper.dark-mode, .skin-blue .sidebar-menu>li:hover>a ,.skin-blue .main-header .logo.dark-mode,.skin-blue,.skin-blue,.skin-blue .main-header .navbar.dark-mode {
          background-color: #141010; /* Black color code */
          color: #ffffff; /* White text color */
         }
          /* Dark Mode Styles */
         .skin-blue .left-side.dark-mode, .skin-blue .main-sidebar.dark-mode, .skin-blue .wrapper.dark-mode, .skin-blue .sidebar-menu>li:hover>a ,.skin-blue .main-header .logo.dark-mode,.skin-blue ,.skin-blue .main-header .navbar.dark-mode{
          background-color: #ffffff; /* White color code */
          color: #141010; /* Black text color */
         }
         .main-header .navbar .sidebar-toggle.dark-mode{
           
          color: #141010; /* Black text color */
        }
        .content-wrapper.dark-mode{ 
         background-color: #ffffff; /* White color code */
        }
        .content-wrapper
        {
          background-color: #141010;/* Black color code */
        }
        .box
        {
          background-color:#222d32;/* dark blue color code */
          color: #ffffff; /* White text color */
        }
        .box.dark-mode
        {
          background-color: #ffffff; /* White color code */
          color: #141010; /* Black text color */
        }
        .box-header
        {
          color: #ffffff; /* White text color */
        }
        .box-header.dark-mode
        {
          color: #141010; /* Black text color */
        }
        
        .skin-blue .sidebar-menu>li.active>a,.skin-blue .sidebar-menu>li:hover>a.dark-mode
        {
          background-color: #ffffff; /* White color code */
          color: #141010; /* Black text color */
        }

        .skin-blue .sidebar-menu>li.active>a,.skin-blue .sidebar-menu>li:hover>a
        {
          background-color:#222d32;
          color: #ffffff; /* White text color */
        }
        .custom-tooltip {
            color: #A52A2A;/* brown color */
            font-size: 20px;/*increase size of icon*/
        }

         .tab-pane active
        {
        
        }
        
        "
      ),
    )
  ),
  

  
  dashboardBody(

    ########visualization section########
    tabItems(
      tabItem(
        tabName = "visualization",
        # Category Selection
        fluidRow(
          box(
            title = "Category Selection",
            selectInput("category", "Select Category", choices = unique(dataset$Country_Region))
          ) 
        ),
        # Comparison
        fluidRow(  
          box(
            title = "Comparison",
            checkboxGroupInput("comparison", "Choose Categories for Comparison",  choices = unique(dataset$Country_Region))
          )
        ),
        # Date Range
        fluidRow(
          box(
            title = "Date Range",
            dateRangeInput("date_range", "Select Date Range")
          ) 
        ),
        # Visualization Options
        fluidRow(
          box(
            title = "Visualization Options",
            checkboxGroupInput("visualizations", "Choose Visualizations to Show",
                               choices = c("scatter", "boxplot + facet", "horizontal bar",
                                           "stacked" ,"bubble" ,"pie" ))
          ) 
        ),
        # Data Type
        fluidRow(
          box(
            title = "Data Type",
            radioButtons("data_type", "Choose Data Type", choices = c("Daily", "Cumulative"))
          ) 
        ),
        fluidRow(
          # Data options plot outputs
          box(plotlyOutput("plot1"),width = 150) ,
          box(plotlyOutput("plot2"),width = 150) ,
          box(plotlyOutput("plot3"),width = 150) ,
          box(plotlyOutput("plot4"),width = 150) 
        ),
        # Visualization
        fluidRow(
          box(plotlyOutput("plot11"),width = 150) ,
          box(plotlyOutput("plot22"),width = 150) ,
          box(plotlyOutput("plot33"),width = 150) ,
          box(plotlyOutput("plot44"),width = 150) ,
          box(plotlyOutput("plot55"),width = 150) ,
          box(plotlyOutput("plot66"),width = 150) 
        ),
      ),
      
      ######Data Options#######
      tabItem(
        
        tabName = "data_options",
       
         ###Switch Units
        fluidRow(
          box(
            title = "Switch Units",
            radioButtons("unit_switch", "Choose Unit", choices = c("Percentage Change", "Absolute Values"))
          )
        ),
        
        fluidRow(
          ##helper tooltips
          tooltip(
            bsicons::bs_icon("info-circle", title = "provide number of deaths in each country and can switch the values of deaths", class = "custom-tooltip"),
            "histogram shows the switch of the values",placement = "right"
          ),
          # Plot for unit_switch
          box(plotlyOutput("plot21"),width = 160)
        ),
       
         
        fluidRow(
          ##=display specific Column
          box(
            title = "Column display",
            selectInput("column", "Select column", choices = c("Confirmed","Deaths","Recovered","Active","Incident_Rate","Case_Fatality_Ratio"))
          ),
          ########Slider for change bins
          box(
          sliderInput("bins_slider", "Number of Bins:", min = 1, max = 100, value = 30),height = 120
          )
        ),
        fluidRow(
          ##helper tooltips
          tooltip(
            bsicons::bs_icon("info-circle", title = "displays range of values for each data column", class = "custom-tooltip"),
            "histogram shows the switch of the values",placement = "right"
          ),
          # Plot for slider & specific Column
          box(plotlyOutput("plot31"),width = 200)
          
        ),
        
      ),
      ####About section
      tabItem(
        tabName = "about",
        fluidRow(
          box(      width = 12,
                    title = "Dachboard Info",
                    solidHeader = TRUE,
                    status = "info",
                    "Here are some clear instructions on how to use the dashboard effectively :",br(),
                    "1.Theme Selection:",br(),
                    " - On the sidebar of the dashboard,you'll find a  section with  button.",br(),
                    "By pressing the button, you can change from dark mode to light mode and vice versa",br(),
                    "2.Sidebar Content:",br(),
                    "- The sidebar of the dashboard contains various options",br(),
                    "first for : visualization ",br(),
                    "secound for : Data ptions",br(),
                    "third for : about",br(),
                    "3.Main Panel Content:",br(),
                    " - The main panel of the dashboard displays  visualizations",br(),
                    
                    "  - Each box or panel may have its own title and content.",br(),
                    "4.Interacting with the Dashboard:",br(),
                    "  - Depending on the specific implementation, you may be able to interact with the dashboard elements.",br(),
                    "   - Look for buttons, dropdowns, sliders, or other input widgets that allow you to manipulate the displayed data or change the visualization.",
                    br(),
                    "5. Customization:",br(),
                    "- Depending on the implementation, the dashboard may offer customization options.",br(),
                    "- Explore these options to tailor the dashboard to your preferences or to focus on specific data or visualizations.
",br(),"Finally, the specific instructions and functionality may vary depending on the dashboard implementation. The instructions above provide a general guideline, but it's essential to refer to any additional documentation or guidelines provided specifically for the dashboard you are using."
          ))
      )
    )
  )
)

#####Server######
# Shiny Server Function
server <- function(input, output, session){
  
  # Toggle button event observer
  observeEvent(input$toggle_button, {
    js$toggleMode()  # Call JavaScript function to toggle mode
  })
  #########plots for data options#######
  # Bar Plot
  output$plot21 <- renderPlotly({
    # Apply dynamic filters based on user input
    filtered_data <- applyDynamicFilters(dataset, input$unit_switch, input$filter_criteria)
    
    # Create a bar plot
    p1 <- ggplot(filtered_data, aes(x = Country_Region, y = Deaths, text = paste("Country: ", Country_Region, "<br>Deaths: ", Deaths))) + 
      geom_bar(stat = "identity", fill = "steelblue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 0.5))  # Adjust angle and hjust as needed
    
    # Convert ggplot to plotly and set hovermode to closest
    ggplotly(p1, tooltip = "text") %>% layout(hovermode = "closest")
  })
  
  # Histogram
  output$plot31 <- renderPlotly({
    # Create a histogram based on the selected column and the slider input
    p <- ggplot(dataset, aes(x = !!sym(input$column))) +
      geom_histogram(bins = input$bins_slider, fill = 'lightblue', color = 'blue', alpha = 0.7) +
      labs(title = paste('Histogram of ', input$column, ' of the dataset'), x = input$column)
    
    # Convert ggplot to plotly and set hovermode to closest
    ggplotly(p, tooltip = c(input$column)) %>% layout(hovermode = "closest")
  })
  
  
  ##########plots for visualization#######
  # 1-Histogram
  output$plot1 <- renderPlotly({
    # Filter the dataset based on the selected category
    filtered_data <- dataset[dataset$Country_Region == input$category, ]
    
    gg <- ggplot(filtered_data, aes(x = Deaths)) + geom_histogram(color = "red")
    
    ggplotly(gg, tooltip = "Deaths") %>% layout(hovermode = "closest")
  })
  
  #2- Bubble Chart
  output$plot2 <- renderPlotly({
    # Filter the dataset based on the selected category
    filtered_dat <- dataset[dataset$Country_Region == input$comparison, ]
    
    gg <- ggplot(filtered_dat, aes(x = Confirmed, y = Country_Region, size = Active, color = Active, text = paste("Country: ", Country_Region, "<br>Confirmed: ", Confirmed, "<br>Active: ", Active))) +
      geom_point(alpha = 0.7) +
      labs(title = "Active cases in country regions", x = "Confirmed cases", y = "Country regions", size = "Count", color = "Cases number") +
      scale_size_area(max_size = 20)
    
    ggplotly(gg, tooltip = c("text")) %>% layout(hovermode = "closest")
  })
  
  ## 3-Time Series Line Chart
  # Generate plot 3 based on user selections
  output$plot3 <- renderPlotly({
    # Check if date_range is not NULL
    if (!is.null(input$date_range)) {
      # Filter data based on the selected date range
      filtered_data <- dataset[dataset$Last_Update >= input$date_range[1] & dataset$Last_Update <= input$date_range[2], ]
      
      # Check if filtered_data is not empty
      if (nrow(filtered_data) > 0) {
        gg <- ggplot(filtered_data, aes(x = Last_Update, y = Incident_Rate)) + geom_line()
        
        ggplotly(gg, tooltip = c("Last_Update", "Incident_Rate")) %>% layout(hovermode = "closest")
      } else {
        # Handle the case where filtered_data is empty
        plot_ly()  # Empty plot
      }
      } else {
      # Handle the case where date_range is NULL
      plot_ly()  # Empty plot
    }
  })
  
  ## 4-Cumulative Time Series Line Chart
  output$plot4 <- renderPlotly({
    if (input$data_type == "Daily") {
      filtered_data <- dataset
    } else {
      # Calculate cumulative values
      filtered_data <- dataset
      filtered_data$Incident_Rate <- cumsum(filtered_data$Incident_Rate)
    }
    
    gg <- ggplot(filtered_data, aes(x = Last_Update, y = Incident_Rate)) + geom_line()
    
    ggplotly(gg, tooltip = c("Last_Update", "Incident_Rate")) %>% layout(hovermode = "closest")
  })
  
  
  
  #5- Scatter Plot
  output$plot11 <- renderPlotly({
    if ("scatter" %in% input$visualizations) {
      p <- ggplot(dataset, aes(x = Country_Region, y = Confirmed, text = paste("Country: ", Country_Region, "<br>Confirmed Cases: ", scales::comma(Confirmed)))) +
        geom_point() +
        labs(title = "Country Region vs. Confirmed cases", x = "Country Region", y = "Confirmed cases")
      
      ggplotly(p, tooltip = c("text")) %>% layout(hovermode = "closest")
    }
  })
  
  ###6-boxplot + facet
  output$plot22 <- renderPlotly({
    if ("boxplot + facet" %in% input$visualizations) {
      
      # Create a ggplot object with tooltips
      p <- ggplot(dataset, aes(x = Last_Update, y = Deaths, text = paste("Last Update: ", Last_Update, "<br>Deaths: ", Deaths))) +
        geom_boxplot() +
        facet_wrap(vars(Last_Update)) +
        labs(title = "Last Update vs. Deaths", x = "Last Update", y = "Deaths")
      
      # Convert ggplot to plotly and set hovermode to closest
      ggplotly(p, tooltip = c("text")) %>% layout(hovermode = "closest")
    }
  })
  
  
  #7- Horizontal Bar
  output$plot33 <- renderPlotly({
    if ("horizontal bar" %in% input$visualizations) {
      
      # Create a ggplot object with tooltips
      p <- ggplot(dataset, aes(x = Country_Region, y = Deaths, text = paste("Country: ", Country_Region, "<br>Deaths: ", Deaths))) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Country Region vs Deaths", x = "Country Region", y = "Deaths")
      
      # Convert ggplot to plotly and set hovermode to closest
      ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest")
     # ggplotly(p, tooltip = "all") %>% layout(hovermode = "closest")
    }
  })
  
  
  # 8-Stacked Bar Chart
  output$plot44 <- renderPlotly({
    if ("stacked" %in% input$visualizations) {
      
      # Reshape the data to long format
      df_long <- gather(dataset, key = "Variable", value = "Value", -Province_State, -Country_Region, -Last_Update,
                        -Confirmed, -Combined_Key, -Incident_Rate, -Case_Fatality_Ratio)
      
      # Create a ggplot object with tooltips
      p <- ggplot(df_long, aes(x = Value, y = Country_Region, fill = Variable, text = paste("Variable: ", Variable, "<br>Value: ", Value))) +
        geom_bar(stat = "identity", position = "stack") +
        labs(title = "Cases Among Country Regions",
             x = "Value",
             y = "Country_Region",
             fill = "Variable") +
        theme_minimal()
      
      # Convert ggplot to plotly and set hovermode to closest
      ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest")
      # ggplotly(p, tooltip = "all") %>% layout(hovermode = "closest")
    }
  })
  
  # 9-Bubble Chart
  output$plot55 <- renderPlotly({
    if ("bubble" %in% input$visualizations) {
      
      # Create a ggplot object with tooltips
      p <- ggplot(dataset, aes(x = Confirmed, y = Country_Region, size = Active, color = Active, text = paste("Country: ", Country_Region, "<br>Confirmed Cases: ", Confirmed, "<br>Active Cases: ", Active))) +
        geom_point(alpha = 0.7) +
        labs(title = "Active Cases in Country Regions", x = "Confirmed Cases", y = "Country Regions", size = "Count", color = "Cases Number") +
        scale_size_area(max_size = 20)
      
      # Convert ggplot to plotly and set hovermode to closest
      ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest")
      # ggplotly(p, tooltip = "all") %>% layout(hovermode = "closest")
    }
  })
  
  # 10-Pie Chart
  output$plot66 <- renderPlotly({
    if ("pie" %in% input$visualizations) {
      p <- plot_ly(
        dataset,
        labels = ~Country_Region,
        values = ~Recovered,
        type = "pie",
        text = ~paste("Country: ", Country_Region, "<br>Recovered Cases: ", Recovered)
      )
      
      ggplotly(p, tooltip = "text") %>% layout(hovermode = "closest")
    }
  })
}

# Helper function to apply dynamic filters
applyDynamicFilters <- function(data, unit_switch, filter_criteria) {
  
  # Apply unit switch
  if (unit_switch == "Percentage Change") {
    data$Deaths <- data$Deaths / max(data$Deaths) * 100
  }
  
  # Apply filter criteria (assuming it's a string match for simplicity)
  if (!is.null(filter_criteria) && filter_criteria != "") {
    data <- filter(data, str_detect(Country_Region, filter_criteria))
  }
  
  return(data)
}

# Create and run Shiny app
shinyApp(ui = ui, server = server)
