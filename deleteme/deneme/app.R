# Bu kodu bir app.R dosyasına yapıştırın ve R Studio'da çalıştırın.

library(shiny)
library(tidyverse)

#setwd("/Users/gozde.ugur/Documents/GitHub/mef07-GozdeUgurK/Shiny")
MovieData = read.csv("/Users/gozde.ugur/Documents/GitHub/mef07-GozdeUgurK/Shiny/movies.csv") 

# Prepare data
film_verisi <- 
  MovieData %>% 
  filter(Year >= 2000) %>%
  select(Genre, Year, `Audience.score..`) 

# Shiny uygulaması
ui <- fluidPage(
  titlePanel("Movie Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Genre", "Tür Seç:",
                  choices = unique(film_verisi$Genre),
                  multiple = TRUE),
      br(),
      sliderInput("year_range", "Yıl Aralığı Seç:",
                  min = min(film_verisi$Year), max = max(film_verisi$Year), 
                  value = c(min(film_verisi$Year), max(film_verisi$Year))),
      br(),
      sliderInput("score_range", "Puan Aralığı Seç:",
                  min = 0, max = 100, value = c(0, 100))
    ),
    mainPanel(
      plotOutput("genre_plot")
    )
  )
)

server <- function(input, output) {
  
  # Filmleri filtrele
  filtered_films <- reactive({
    filter(film_verisi, 
           Genre %in% input$Genre & 
             `Audience.score..` >= input$score_range[1] & 
             `Audience.score..` <= input$score_range[2])
  })
  
  # ggplot ile çubuk grafik çizimi
  output$genre_plot <- renderPlot({
    ggplot(filtered_films(), aes(x = Year, fill = Genre)) +
      geom_bar(position = "dodge") +
      labs(title = "Film Türüne Göre Yıl Bazında Dağılım",
           x = "Yıl",
           y = "Film Sayısı",
           fill = "Tür") +
      theme_minimal()
  })
}

shinyApp(ui, server)
