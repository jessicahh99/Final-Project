#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


############

library(tidyverse)
library(hockeyR)
pbp_2021 <- load_pbp("2020-2021")
pbp_2122 <- load_pbp("2021-2022")
pbp_2223 <- load_pbp("2022-2023")
pbp_2324 <- load_pbp("2023-2024")

pbp_2021_1 <- pbp_2021 |>
  select(event_type) |>
  filter(event_type == "SHOT" | event_type == "BLOCKED_SHOT" |
           event_type == "MISSED_SHOT" | event_type == "GOAL") |>
  mutate(
    is_goal = ifelse(event_type == "GOAL",1,0),
    is_shot = ifelse(event_type == "SHOT",1,0),
    is_missed = ifelse(event_type == "MISSED_SHOT",1,0),
    is_blocked = ifelse(event_type == "BLOCKED_SHOT",1,0)
  )

corsi <- glm(is_goal ~ is_shot + is_missed + is_blocked, family = binomial, data = pbp_2021_1)


summary(corsi)
