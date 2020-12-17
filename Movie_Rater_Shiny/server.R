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

#Choose the location of data in your files. Too big to go on github
#load("C:\Users\dc48b\Dropbox (University of Michigan)\BIOS 625 - Big Data Computing\FinalProject\data\data_clean.rda")

titles <- dat %>% 
    distinct(Title,.keep_all = T) %>% 
    mutate(RottenTomatoes = RottenTomatoes * 100) %>% 
    filter(!is.na(IMDb),!is.na(RottenTomatoes))

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
        
        #Create histogram
        ggplot(plot_data) + 
            geom_histogram(mapping = aes(x = IMDb), 
                           bins = nbins, color = "white",
                           fill = input$color) +
            labs(y = "Count", x = "Movie Rating") + 
            xlim(0,10) +
            theme_minimal()

    })

})
