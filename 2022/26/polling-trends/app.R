library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(ggtext)
library(here)

# Election date
election_date <- as_date("2021-09-26")

# Prepare data, set up custom ggplot2 theme
source("setup.R")
latest_date_available <- format(max(polls$date), "%B %d, %Y")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),

    # Application title
    titlePanel("Aggregated Polls for the German Federal Election"),
    
    p("Many polls from different institutes are published, especially in the run-up 
        to an federal election.
        Instead of looking at each individual published polling result, it is more valuable to
        observe general trends. The trend lines are fitted using LOWESS 
        (Locally Weighted Scatterplot Smoother): for every single 
        data point a local regression is calculated, taking into account only a certain
        range of neighboring data points. A weighting function ensures that the 
        importance of these points decreases with increasing distance.
        The ribbons around the lines indicate the 95% confidence intervals of the 
        point estimates. Individual poll results are shown as points.
        "),
    p("The bandwidth (α) controls the degree of smoothing.
      A smaller value (close to 0) means a small degree of smoothing and comes with 
      a risk of overfitting the individual polls.
      A larger value (close to 1) means a large degree of smoothing.",
      # br(),
        strong("Play around with the parameter to see how the smoothed lines change.")),
    br(),
    fluidRow(

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
            dateInput("start_date", "Select the start date:",
                      value = election_date,
                      min = min(polls$date), max = max(polls$date),
                      weekstart = 1),
            dateInput("end_date", "Select the end date:",
                      value = max(polls$date),
                      min = min(polls$date), max = max(polls$date),
                      weekstart = 1)
        ),
        
        mainPanel(
           plotOutput("pollsPlot", width = "90%")
        )
   ),
   h2("How to"),
   p(strong("1. Choose a bandwidth for smoothing with the slider"), br(), 
     "A longer period might require a larger bandwidth."),
   p(strong("2. Select parties"), br(),
     "When you click in the input field, a list of unselected parties will be shown. 
     You can also type party names. To unselect a party, click on it and press Delete. 
     The 6 major parties in Germany can be selected."),
   p(strong("3. Set the start and end date"), br(),
     "Select a period to show in the plot. Data is available from 2021 to", 
     latest_date_available
     ),
   br(),
   p("Source:", a("wahlrecht.de/", href = "https://www.wahlrecht.de/umfragen/"),
     ". Date: ", latest_date_available
     )
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
                title = "",
                subtitle = "",
                caption = "Source: Wahlrecht.de.",
                x = NULL,
                y = "Projected vote share (%)"
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
