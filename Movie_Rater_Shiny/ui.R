#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = shinytheme("yeti"),
    
    tabsetPanel(
        tabPanel("Movies & TV Shows",
                 
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
        
        tabPanel("Actors",
            "Actors page in progress. Not sure what, if anything, we could have here about actors."
            ),
        
        tabPanel("About",
                 tags$p(
                     tags$h2("Background"),
                     "Why we chose this subject, why it's interesting, etc"
                 ),
                 tags$p(
                     tags$h2("Streaming Service Movie Raters"),
                     "About the app, the model, etc"
                     
                 ),
                 tags$p(
                     tags$h2("Contributors"),
                     "This software was developed by Grace Bosma, Dylan Clark-Boucher, and Chris",
                     "Shin, all current students in the University of Michigan Department of Biostatistics,",
                     "as part of the course \"BIOSTAT 625: Big Data Computing.\""
                 )
            )
        )
))
