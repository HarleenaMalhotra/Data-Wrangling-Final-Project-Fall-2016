library(shiny)
library(tidyverse)
library(ggthemes)
library(stringr)
library(readr)
library(choroplethr)
library(choroplethrMaps)
library(plotly)

CountryRatio <- read_csv("TidyData/TidyDF.csv")
CountrySlope <- read_csv("TidyData/TidyDF_unnested.csv")

l <- list(color = toRGB("Grey"), width = 0.2)

# specify map projection/options
g <- list(
  showframe = TRUE,
  showcoastlines = TRUE,
  projection = list(type = 'Mercator')
)
p <- plot_geo(CountrySlope) %>%
  add_trace(
    z = ~lm_slope2, color = ~lm_slope2, colors = 'Purples',
    text = ~Country, locations = ~CC, marker = list(line = l)
  ) %>%
  layout(
    title = '2000-2012 Global Ratio of Homicide vs. Unemployment',
    geo = g
  )



ui <- fluidPage(
  titlePanel("Homicide Relation to Unemployment"), 
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearInput", "Year", min = 2000, max = 2012, value = c(2000, 2012)),
      selectInput("countryInput", label = "Country", choices = unique(CountryRatio$Country))),
    mainPanel(
      plotlyOutput("main_plot"),
      plotlyOutput("results")
    )
  )
)

server <- function(input, output, session) {
  reduced_df <- reactive({
    filter(
      CountryRatio, 
      Country == input$countryInput, 
      Year >= input$yearInput[1] & Year <= input$yearInput[2]
    )
  })
  
  
  output$main_plot <- renderPlotly({
      p
    
    })
  output$results <- renderPlotly({ 
    
    h <- ggplot(data = reduced_df(), aes(UnemploymentRate, HomicideRate)) + 
      geom_smooth(se=FALSE) + ggtitle(input$countryInput) + theme_bw() + scale_y_continuous(limits = c(-10, 100)) 
    ggplotly(h)
    
  })
}

shinyApp(ui = ui, server = server)