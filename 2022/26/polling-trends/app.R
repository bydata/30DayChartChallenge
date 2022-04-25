library(shiny)
library(tidyverse)
library(lubridate)
library(ggtext)
library(here)

# Election date
election_date <- as_date("2021-09-26")

# Prepare data, set up custom ggplot2 theme
source("setup.R")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("LOWESS"),

    # Sidebar with a slider input for number of bins 
    # verticalLayout(
        sidebarPanel(
            sliderInput("bandwidth",
                        "Choose a bandwidth for smoothing",
                        min = 0.001,
                        max = 1,
                        value = 0.2),
            selectInput("party", "Choose one or more parties",
                        choices = unique(polls$party),
                        selected = unique(polls$party)[1:2],
                        multiple = TRUE
            ),
            # Date range selection
            dateInput("start_date", "Start date:",
                      value = election_date,
                      min = min(polls$date), max = max(polls$date),
                      weekstart = 1),
            dateInput("end_date", "End date:",
                      value = max(polls$date),
                      min = min(polls$date), max = max(polls$date),
                      weekstart = 1)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("pollsPlot", width = "100%")
        )
   # )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # Set the max value for the start date input according to the end date
    observe({
        updateDateInput(session, "start_date",
                        max = input$end_date - period("1 day")
        )
    })
    # Set the min value for the end date input according to the start date
    observe({
        updateDateInput(session, "end_date",
                        min = input$start_date + period("1 day")
        )
    })
    
    output$pollsPlot <- renderPlot({
        polls %>% 
            filter(party %in% input$party) %>% 
            filter(date >= input$start_date & date <= input$end_date) %>% 
            ggplot(aes(date, share)) +
            geom_point(aes(color = party), alpha = 0.2, size = 1, shape = 21) +
            geom_smooth(aes(color = party, fill = stage(party, after_scale = alpha(color, 0.2))), 
                        size = 0.5, se = TRUE, method = "loess", span = input$bandwidth, level = 0.95) +
            geom_text(data = ~filter(., date == max(date)),
                      aes(y = share, label = party, color = party),
                      stat = "summary", fun = mean, nudge_x = 1, hjust = 0, vjust = 0.5, 
                      family = "Lato", 
                      fontface = "bold", size = 4
            ) +
            scale_x_date(expand = expansion(mult = c(0.1, 0.2)), 
                         date_labels = "%b %Y") +
            scale_y_continuous(breaks = seq(0, 50, 10), sec.axis = dup_axis()) +
            scale_color_manual(values = party_colors) +
            coord_cartesian(ylim = c(0, NA), clip = "off") +
            guides(color = "none", fill = "none") +
            labs(
                title = "Aggregated Polls for the German Federal Election",
                subtitle = "",
                caption = "Source: Wahlrecht.de.",
                x = NULL,
                y = "Projected vote share (%)"
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
