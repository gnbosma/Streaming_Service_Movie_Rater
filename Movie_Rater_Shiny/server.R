#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggpubr)

#Choose the location of data in your files. Too big to go on github
#load("/home/shared/biostat625w2020/data_clean.rda")

titles <- dat %>% 
    distinct(Title,.keep_all = T) %>% 
    mutate(RottenTomatoes = RottenTomatoes * 100) %>% 
    filter(!is.na(IMDb),!is.na(RottenTomatoes))

secondary_color = "gray60"

#Function to check whether inputted x falls into given range
    #Input: vector x, length-2 vector of lower, upper limit
    #Output: logical vector same length as x
inRange <- function(x,range){
    x <- as.numeric(x)
    range <- as.numeric(range)
    return(x >= range[1] & x <= range[2])
}


shinyServer(function(input, output) {
    
    output$histplot <- renderPlot({
        
        #Filter according to inputs
        right_service <- titles %>% 
            select(all_of(input$service)) %>% 
            rowSums() %>% as.logical()
            
         plot_data <- titles %>% 
             filter(right_service,
                    inRange(RottenTomatoes, input$tscore),
                    inRange(Year,input$yr),
                    Age %in% input$ages
                    )
        
        #Make sure bin count is fine
        nbins <- input$bins
        if(nbins > nrow(plot_data)){
            nbins <- nrow(plot_data)
        }
        
        #Create figure
        p1 <- ggplot(plot_data,aes(x=IMDb)) + 
            geom_histogram(bins = nbins, 
                           color = "gray60", 
                           fill = input$color)
        
        height <- max(ggplot_build(p1)$data[[1]]$count)
        
        p1 + geom_boxplot(mapping=aes(y = -height/10), 
                          width = height/10,
                          color = "gray60",
                          fill = input$color) + 
            labs(x = "IMDb Rating", y = "Frequency") +
            theme_minimal()
        
        

    })

})
