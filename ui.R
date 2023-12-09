## ui.R
library(shiny)
library(shinydashboard)
# library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          # add sidebar to allow toggel between the two recommender systems
          dashboardSidebar(
            sidebarMenu(
              menuItem("Recommender by Genre", tabName = "genre_tab", icon = icon("film")),
              menuItem("Recommender by Rating", tabName = "rate_tab", icon = icon("star"))
            )
          ),

          dashboardBody(includeCSS("css/movies.css"),
              tabItems(
                # Genre page
                tabItem(tabName = 'genre_tab',
                          fluidRow(
                            box(width = 12, title = "Step 1: Select your favorite genre", status = "info", solidHeader = TRUE, collapsible = TRUE,
                                selectInput("user_genre", 'select a single genre from the dropdown menu', all_genre)
                            )
                          ),
                          fluidRow(
                            useShinyjs(),
                            box(
                              width = 12, status = "info", solidHeader = TRUE,
                              title = "Step 2: Discover movies you might like",
                              br(),
                              withBusyIndicatorUI(
                                actionButton("genrebtn", "Click here to get your recommendations", class = "btn-warning")
                              ),
                              br(),
                              tableOutput("rec_genre_results")
                            )
                          )
                        ),
                # recommender by user ratings
                tabItem(tabName = 'rate_tab',
                        fluidRow(
                          box(width = 12, title = "Step 1: Rate at least 10 movies.", status = "info", solidHeader = TRUE, collapsible = TRUE,
                              div(id = 'user_rating',
                                  class = "rateitems",
                                  uiOutput('ratings')
                              )
                          )
                        ),
                        fluidRow(
                          useShinyjs(),
                          box(
                            width = 12, status = "info", solidHeader = TRUE,
                            title = "Step 2: Discover movies you might like",
                            br(),
                            withBusyIndicatorUI(
                              actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                            ),
                            br(),
                            tableOutput("results")
                          )
                        )
                )
              )
          )
    )
) 