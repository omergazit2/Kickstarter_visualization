library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(countrycode)
library(DT)
library(MASS)

kickstarter_data <- read.csv("./data/processed_kikstarter_data.csv")

# Define the UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
          body {
            padding: 40px;
          }
        "))
  ),
  titlePanel("Final Project - Kickstarter Data Visualization"),
  
  # First row
  fluidRow(
    h3("Yearly Investments and Succession"),  
    fluidRow(
      column(12,
             sliderInput("year_range",
                         "Select years range:",
                         min = min(kickstarter_data$launched_year),
                         max = max(kickstarter_data$launched_year),
                         value = c(min(kickstarter_data$launched_year), max(kickstarter_data$launched_year)))
      ),
      column(12,plotlyOutput("linePlot")),
      column(12,plotlyOutput("barPlot"))
    )
  ),
  
  tags$hr(style="border-color:black; border-width:2px"),
  
  # Second row
  fluidRow(
    h3("Project Success Rate per Country"), 
    column(6, plotlyOutput("heatmap")), 
    column(6, DTOutput("projectStatsTable"))
  ),
  
  tags$hr(style="border-color:black; border-width:2px"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("category", "Choose Categories:", 
                         choices = unique(kickstarter_data$main_category), 
                         selected = unique(kickstarter_data$main_category)[1:2]
      ),
      width = 3
    ),
    mainPanel(
      h3("Project Duration Distribution by State and Main Category"),
      plotlyOutput("boxPlot")
    )
  )
  
)

# Define the server
server <- function(input, output) {
  
  data1 <- reactive({
    kickstarter_data %>% 
      filter(between(launched_year, input$year_range[1], input$year_range[2])) %>%
      group_by(launched_year, state) %>%
      summarise(total_investment = sum(usd_pledged_real, na.rm = TRUE) / 1000000,
                num_projects = n()) %>%
      ungroup()
  })
  data1_line <- reactive({
    kickstarter_data %>% 
      filter(between(launched_year, input$year_range[1], input$year_range[2])) %>%
      group_by(launched_year) %>%
      summarise(total_investment = sum(usd_pledged_real, na.rm = TRUE) / 1000000,
                num_projects = n()) %>%
      ungroup()
  })
  
  data2 <- reactive({
    kickstarter_data %>% 
      group_by(country) %>%
      summarise(success_rate = sum(state == "successful") / n()) %>%
      rename("Success Rate" = "success_rate") %>%
      ungroup()
  })
  
  data3 <- reactive({
    kickstarter_data %>%
      group_by(country_name) %>%
      summarise(project_count = n(),
                success_rate = paste0(round(sum(state == "successful") / n() * 100, 2), "%")) %>%
      ungroup() %>%
      arrange(desc(success_rate)) %>% 
      rename("Country" = "country_name", "Number of Projects" = "project_count", "Success Rate" = "success_rate")
  })
  data4 <- reactive({
    kickstarter_data %>%
      filter(main_category %in% input$category)
  })
  
  output$linePlot <- renderPlotly({
    p <- ggplot(data1_line(), aes(x=launched_year, y=total_investment)) +
      geom_line() +
      scale_x_continuous(breaks = round(seq(min(data1_line()$launched_year), max(data1_line()$launched_year), by = 1),0)) + 
      theme_minimal() +
      ylab("Total investment in M$") +
      xlab("Launched year") +
      ggtitle("Yearly Investments in Million USD")
    ggplotly(p)
  })
  
  output$barPlot <- renderPlotly({
    p <- ggplot(data1(), aes(x=launched_year, y=num_projects, fill=state)) +
      geom_bar(stat="identity") +
      scale_x_continuous(breaks = round(seq(min(data1()$launched_year), max(data1()$launched_year), by = 1),0)) +
      theme_minimal() +
      ylab("Number of projects") +
      xlab("Launched year") +
      ggtitle("Number of Projects per Year")
    p <- ggplotly(p)
    p <- layout(p, legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = 'center'), xaxis = list(autorange = TRUE))
    p
  })
  
  output$heatmap <- renderPlotly({
    plot_ly(data2(), z = ~`Success Rate`, locations = ~country, type = "choropleth", 
            color = ~`Success Rate`, colors = "Blues") %>%
      layout(legend=list(title="Success Rate")) %>%
      layout(geo = list(showframe = FALSE, showcoastlines = TRUE))
  })
  
  output$projectStatsTable <- renderDT({
    datatable(data3() , options = list(pageLength = 5),
              rownames = FALSE)
  })
  
  output$boxPlot <- renderPlotly({
    plot_ly(data = data4(), y = ~duration, color = ~state, x = ~main_category, type = "box",
            hoverinfo = "y+color", boxmean = TRUE, colors = c("#d35400", "#2980b9")) %>% 
      layout(yaxis = list(title = "Project Duration (days)"), 
             xaxis = list(title = "Main Category", tickangle = -45),  
             boxmode = "group", 
             legend = list(title = list(text = "<b>Status</b>")),  
             height = 450)
  })
  
}

shinyApp(ui = ui, server = server)
