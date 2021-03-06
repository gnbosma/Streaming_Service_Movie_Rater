#UI for Shiny App

library(shiny)
library(shinythemes)
library(DT)

shinyUI(fluidPage(
    theme = shinytheme("yeti"),
    
    #Split webpage into several tabs
    tabsetPanel(
        
        #Tab 1: Ratings filter tool
        tabPanel("Ratings Filter Tool",
                 
                 # Tab title
                 titlePanel("Distribution of Movie Ratings"),
                 

                 sidebarLayout(
                     
                     #Sidebar for user input
                     sidebarPanel(
                         
                        fluidRow(
                             tags$h3("Filter"),
                             
                             column(4,
                                    checkboxGroupInput("ages", 
                                                       label = "Age",
                                                       choices = list("All" = "All Ages", "7+" = "7+", 
                                                                      "13+" = "13+", "16+" = "16+",
                                                                      "18+" = "18+"),
                                                       selected = c("All Ages","7+","13+","16+","18+"))                            
                             ),
                             
                             column(4, offset = 1,
                                 checkboxGroupInput("service", 
                                                    label = "Service", 
                                                    choices = list("Disney+" = "disneyplus", 
                                                                   "Hulu" = "hulu", 
                                                                   "Netflix" = "netflix",
                                                                   "Prime Video" = "amazonprime"),
                                                    selected = c("disneyplus","hulu","netflix",
                                                                 "amazonprime")
                                                    )                                    
                                    
                            )),
                        
                         
                         fluidRow(

                        sliderInput("tscore", label = "Tomatometer Score", min = 0, 
                                         max = 100, value = c(0,100)
                                         ),
                             
                             sliderInput("yr",label = "Release year",
                                         min = 1901,
                                         max = 2020,
                                         value = c(1901,2020),
                                         sep = ""),
                    
                         width = 2.5)),
                     
                     # Main panel with plot and extra options
                     mainPanel(
                         fluidRow(
                             "This app was built for the purpose of modeling and visualizing the IMDb ratings",
                             "of movies on different streaming platforms. Let's get started. Adjust the parameters",
                             "in the tab to the left to view the ratings distributions of different sets of movies.",
                             "Even better, choose the options above the figure to change the color of histogram or the ",
                             "number of bins."
                         ),
                         tags$hr(),
                         fluidRow(
                             column(8,
                                    sliderInput("bins",
                                                label = "Number of bins",
                                                min = 10,
                                                max = 30,
                                                value = 20)
                             ),
                             column(4,
                                    selectInput("color", label = "Color choice", 
                                                choices = list("Blue" = "slateblue2", 
                                                               "Red" = "firebrick3", 
                                                               "Green" = "seagreen4",
                                                               "Orange" = "sienna2",
                                                               "Purple" = "orchid4"), 
                                                selected = "orchid4")
                             )
                         ),
                         
                         fluidRow(
                             plotOutput("histplot",height = "320px")
                             )

                     )
                 )
            
        ),
        
        #Tab 2: Ratings by category
        tabPanel("Ratings by Category",
                 
                #Several tabs within this page. One for each category.
                navlistPanel(
                    
                    #Streaming service tab
                    tabPanel("Streaming Service",
                             titlePanel("Ratings by Streaming Service"),
                             fluidRow(column(10, offset = 1, 
                                      "Below are the distributions of movie ratings for each streaming service.", 
                                      "Our dataset contained 287 movies from Disney+, 499 from Hulu, 1,629 from Netflix,", 
                                      "and 5,858 from Amazon Prime Video, for a total of 7,883 unique films. Note that, since some",
                                      "movies might be available on more than one platform, these groupings",
                                      "are not mutually exclusive. Overall, differences in IMDb Ratings across streaming",
                                      "platforms appear to be small.")),
                            tags$hr(),
                            fluidRow(column(10, offset = 1, plotOutput("service_plot")))
                            ),
                    
                    #Age rating tab
                    tabPanel("Age Rating",
                             titlePanel("Ratings by Age Groups"),
                             fluidRow(column(10, offset = 1,
                                      "The movies we looked at spanned all genres and all age groups.",
                                      "There were 325 designated for all ages, 790 for ages 7+,",
                                      "764 for ages 13+, 165 for ages 16+, and a massive 2,126 for ages 18+.",
                                      "The ratings distributions across these groups once again appear to be",
                                      "quite similar, though the 16+ group had the highest median score.")),
                             tags$hr(),
                             fluidRow(column(10, offset = 1, plotOutput("age_plot")))
                        
                    ),
                    
                    #Tomatometer tab
                    tabPanel("Tomatometer Score",
                             titlePanel("Ratings by Tomatometer Score"),
                             fluidRow(column(10, offset = 1,
                                             "The figure below shows the assocation between Tomatometer score",
                                             "and IMDb rating for movies on all of our streaming platforms.",
                                             "While IMDb rating, our outcome of interest, gives a glimpse",
                                             "at a movie's overall audience reception, the tomatometer score,",
                                             "on the x-axis, tells us what professional critics thought of the film.",
                                             "Unsurprisingly, we can spot a relatively strong positive correlation:",
                                             " movies with a high Tomatometer scores tend to have higher IMDb ratings.",
                                             "To see if that holds up for particular age groups or streaming platforms,",
                                             "make sure to check out the options included below the graph.")),
                             tags$hr(),
                             fluidRow(column(10, offset = 1, plotOutput("ts_plot"))),
                             tags$hr(),
                             fluidRow(
                                 column(3, offset = 1,
                                        checkboxGroupInput("ts_ages", 
                                                           label = "Age",
                                                           choices = list("All" = "All Ages", "7+" = "7+", 
                                                                          "13+" = "13+", "16+" = "16+",
                                                                          "18+" = "18+"),
                                                           selected = c("All Ages","7+","13+","16+","18+"))
                                 ),
                                 
                                 column(3,offset = 0,
                                        checkboxGroupInput("ts_service", 
                                                           label = "Service", 
                                                           choices = list("Disney+" = "disneyplus", 
                                                                          "Hulu" = "hulu", 
                                                                          "Netflix" = "netflix",
                                                                          "Prime Video" = "amazonprime"),
                                                           selected = c("disneyplus","hulu","netflix",
                                                                        "amazonprime"))                 
                                ))
                    
                    ),
                    
                    widths = c(2,10)
                )
                
            ),
        
        #Tab 3: Linear model
        tabPanel("Score Prediction",
                 titlePanel("Linear Model for Predicting IMDb Ratings"),
                 
                 sidebarLayout(
                     
                     #Side bar with user input
                     sidebarPanel(
                         checkboxGroupInput("include_vars",tags$strong("Variables"),
                                            choices = list(
                                                "Tomatometer Score" = "rt_score",
                                                "Age Rating" = "age_rating",
                                                "Release Year" = "year",
                                                "Cast Score 1" = "actor_score1",
                                                "Cast Score 2" = "actor_score2",
                                                "Cast Score 3" = "actor_score3"
                                            ),
                                            selected = c()),
                         width = 2
                         ),
                       
                     #Main panel with model output
                       mainPanel(
                           fluidRow("Included below is the output of a linear model",
                                    " for predicting IMDb ratings. By default, the model",
                                    "uses each of our streaming services as a binary predictor,",
                                    " providing insight to the mean difference in IMDb score between ",
                                    "moves that are available and unavailable on those platforms.",
                                    " We can see that all four estimates are significant,",
                                    " providing statistical evidence that movies on different platforms",
                                    " have different ratings on average. To add more variables to the model",
                                    "use the side tab to the left."),
                           tags$hr(),
                           fluidRow(DT::dataTableOutput("lm_table1"))
                           )
                               
                               
                 )
                 
            
            ),
        
        #Tab 4: About page
        tabPanel("About",
                 
                 tags$p(
                     tags$h3("Background"),
                     "The purpose of this project was to both model and visualize the IMDb ",
                     "ratings of movies and films currently available for streaming on",
                     "various online platforms. Based on ratings data from the International",
                     "Movie Database (", tags$a("IMDb",href = "https://www.imdb.com/"),
                     "), as well as streaming service data accessible for free on",
                     tags$a("Kaggle",href = "https://www.kaggle.com/ruchi798/movies-on-netflix-prime-video-hulu-and-disney"),
                     ", our team constructed a flexible model for predicting",
                     "overall ratings on a scale of 0 to 10 stars. These predictions depended on",
                     "a user-defined set of covariates, including such factors as a production's",
                     "streaming service(s), target age group, release year, Rotten Tomato's ",
                     tags$a("Average Tomatometer",href = "https://www.rottentomatoes.com/about#whatisthetomatometer"),
                     "score, and our own set of variables called \"Cast Scores\". Additional information",
                     " on our model, visualizations, and code can be found on our ",
                     tags$a("GitHub repository",href ="https://github.com/gnbosma/Streaming_Service_Movie_Rater"),
                     ", under the file StreamingServiceMovieRater.rmd."
                 ),
                 
                 tags$p(
                     tags$h3("Contributors"),
                     "This software was developed by Grace Bosma, Dylan Clark-Boucher, and Chris",
                     "Shin, all current students in the University of Michigan Department of Biostatistics,",
                     "as part of the course \"BIOSTAT 625: Big Data Computing.\""
                 )
            )
        )
))
