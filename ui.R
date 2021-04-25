library(shiny)
genreList <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci.Fi", "Thriller", "War", "Western")

shinyUI(fluidPage(
 includeCSS("basic.css"),
  titlePanel("--------------------------------Movie Recommendation System----------------------------"),
  fluidRow(
    
    column(4, h3("Select your favorite movie genres:"),
           wellPanel(
             selectInput("input_genre", "Genre 1",
                         genreList),
             selectInput("input_genre2", "Genre 2",
                         genreList),
             selectInput("input_genre3", "Genre 3",
                         genreList)
             
           )),
    column(4, h3("Select a movie you like from genres:"),
           wellPanel(
             uiOutput("ui"),
             uiOutput("ui2"),
             uiOutput("ui3")
           )),
    column(4,
           h3("You might like these movies too!"),
           tableOutput("table")
          
    )
  )))

