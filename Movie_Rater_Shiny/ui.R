
library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = shinytheme("yeti"),
    
    tabsetPanel(
        tabPanel("Ratings Filter Tool",
                 
                 # Application title
                 titlePanel("Streaming Service Movie Rater"),
                 
                 # Sidebar with user input
                 sidebarLayout(
                     
                     sidebarPanel(
                         
                         #Side Panel Row One
                         fluidRow( 
                             tags$h2("Filter"),
                             
                             column(6,
                                 checkboxGroupInput("service", 
                                                    label = "Service", 
                                                    choices = list("Disney+" = "Disney+", 
                                                                   "Hulu" = "Hulu", 
                                                                   "Netflix" = "Netflix",
                                                                   "PrimeVideo" = "PrimeVideo"),
                                                    selected = c("Disney+","Hulu","Netflix",
                                                                 "PrimeVideo")
                                                    )                                    
                                    
                            ),
                            
                            column(6,
                                 checkboxGroupInput("ages", 
                                                    label = "Age Group",
                                                    choices = list("All ages" = "all", "7+" = "7+", 
                                                               "13+" = "13+", "16+" = "16+",
                                                               "18+" = "18+"),
                                                    selected = c("all","7+","13+","16+","18+")
                                                )                            
                            )),
                         
                         #Side Panel Row 2
                         fluidRow(
                             sliderInput("tscore", label = "Tomato Score", min = 0, 
                                         max = 100, value = c(0,100)
                                         ),
                             

                             
                             sliderInput("yr",label = "Release year",
                                         min = 1901,
                                         max = 2020,
                                         value = c(1901,2020),
                                         sep = "")                             
                             
                         ),
                         
                         tags$hr(),
                         
                         #Side Panel Row 3
                         fluidRow(
                             tags$h2("Visualization Features"),
                             sliderInput("bins",
                                         label = "Number of bins",
                                         min = 10,
                                         max = 30,
                                         value = 20),
                             
                             selectInput("color", label = "Color choice", 
                                         choices = list("Blue" = "slateblue2", 
                                                        "Red" = "firebrick3", 
                                                        "Green" = "seagreen4",
                                                        "Orange" = "sienna2",
                                                        "Purple" = "orchid4"), 
                                         selected = "orchid4")
                             
                         ),
                    
                         width = 3),
                     
                     # Show a plot of the generated distribution
                     mainPanel(
                         fluidRow(
                             plotOutput("histplot"),
                             tags$p("Histogram of IMDb Ratings among selected movies and TV shows.")
                         ),
                         fluidRow(
                             "Text in lower row"
                         )
                     )
                 )
            
        ),
        
        tabPanel("Ratings by Category",
                fluidRow(
                    tags$h3("Ratings by Streaming Service"),
                    "Movie and TV show ratings for each streaming service. Note that, since some",
                    "movies or shows might be available on more than one platform, these groupings",
                    "are not mutually exclusive. Overall, differences in IMDb Ratings across streaming",
                    "platforms appear to be small.",
                    plotOutput("service_plot")
                    
                ),
                fluidRow(
                    tags$h3("Ratings by Target Age Group")
                ),
                fluidRow(
                    tags$h3("Ratings by Average Tomatometer Score")
                )
            ),
        
        tabPanel("Score Prediction"
            
            ),
        
        tabPanel("About",
                 tags$p(
                     tags$h3("Background"),
                     "The purpose of this project was to both model and visualize the IMDb ",
                     "ratings of movies and TV shows currently available for streaming on",
                     "various online platforms. Based on ratings data from the International",
                     "Movie Database (", tags$a("IMDb",href = "https://www.imdb.com/"),
                     "), as well as streaming service data accessible for free on",
                     tags$a("Kaggle",href = "https://www.kaggle.com/ruchi798/movies-on-netflix-prime-video-hulu-and-disney"),
                     ", our team constructed a flexible model for predicting",
                     "overall ratings on a scale of 0 to 5 stars. These predictions depended on",
                     "a user-defined set of covariates, including such factors as a production's",
                     "streaming service(s), target age group, release year, Rotten Tomato's ",
                     tags$a("Average Tomatometer",href = "https://www.rottentomatoes.com/about#whatisthetomatometer"),
                     "score, and others."
                 ),
                 tags$p(
                     tags$h3("Streaming Service Movie Raters"),
                     "About the app, the model, etc"
                     
                 ),
                 tags$p(
                     tags$h3("Contributors"),
                     "This software was developed by Grace Bosma, Dylan Clark-Boucher, and Chris",
                     "Shin, all current students in the University of Michigan Department of Biostatistics,",
                     "as part of the course \"BIOSTAT 625: Big Data Computing.\" Additional information on",
                     "our model, visualizations, and code can be found on our ",
                     tags$a("GitHub repository",href ="https://github.com/gnbosma/Streaming_Service_Movie_Rater"),
                     "."
                 )
            )
        )
))
