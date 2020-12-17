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
shinyUI(fluidPage(theme = shinytheme("slate"),

    # Application title
    titlePanel("Streaming Service Movie Rater"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(

        sidebarPanel(
            checkboxGroupInput("service", label = "Streaming service", 
                           choices = list("Disney+" = "Disney+", 
                                          "Hulu" = "Hulu", 
                                          "Netflix" = "Netflix",
                                          "PrimeVideo" = "PrimeVideo"),
                           selected = c("Disney+","Hulu","Netflix",
                                        "PrimeVideo")
                           ),
            
            sliderInput("tscore", label = "Tomato Score", min = 0, 
                        max = 100, value = c(0,100)),
            
            checkboxGroupInput("ages", label = "Age Group", 
                        choices = list("All" = "all", "7+" = "7+", 
                                       "13+" = "13+", "16+" = "16+",
                                       "18+" = "18+"), 
                        selected = c("all","7+","13+","16+","18+")),
            
            sliderInput("yr",label = "Release year",
                        min = 1901,
                        max = 2020,
                        value = c(1901,2020),
                        sep = ""),
            
            sliderInput("bins",
                          label = "Number of bins",
                          min = 5,
                          max = 40,
                          value = 20),
            
            selectInput("color", label = "Color choice", 
                        choices = list("Blue" = "slateblue2", 
                                       "Red" = "firebrick3", 
                                       "Green" = "seagreen4",
                                       "Orange" = "sienna2",
                                       "Purple" = "orchid4"), 
                        selected = "Blue")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(tags$h2("IMBd Popular Score"),
            plotOutput("histplot")
        )
    )
))
