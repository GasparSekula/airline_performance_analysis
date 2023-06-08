library(shiny)
library(dplyr)
library(ggplot2)

path <- paste(getwd(), "/airlines_most_flights.csv", sep="")
airlines_most_flights_df <- read.csv(path)

airlines_most_flights_df <- airlines_most_flights_df %>% mutate( Description = recode(Description, "US Airways Inc. (Merged with America West 9/05. Reporting for both starting 10/07.)" = "US Airways Inc.")) %>%
  mutate(Description = recode(Description, "America West Airlines Inc. (Merged with US Airways 9/05. Stopped reporting 10/07.)" = "America West Airlines Inc.")) 


ui <- fluidPage(
  titlePanel("10 największych przewoźników w USA w latach 1987-2008."),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Wybierz rok", choices = unique(airlines_most_flights_df$Year))
    ),
    mainPanel(
      plotOutput("chart")
    )
  )
)

server <- function(input, output) {
  output$chart <- renderPlot({
    year <- input$year
    selected_year_df <- subset(airlines_most_flights_df, Year == year)
    top_10_airlines <- head(selected_year_df[order(-selected_year_df$TotalAnnualFlights), ], 10)
    
    ggplot(top_10_airlines, aes(x = Description, y = TotalAnnualFlights)) +
      geom_bar(stat = "identity", fill = "tomato2") +
      labs(x = "przewoźnik", y = "sumaryczna liczba lotów", title = paste("10 największych przewoźników w USA w roku", year)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

shinyApp(ui, server)







