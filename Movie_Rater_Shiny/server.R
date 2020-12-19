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
                           color = secondary_color, 
                           fill = input$color)
        
        height <- max(ggplot_build(p1)$data[[1]]$count)
        
        p1 + geom_boxplot(mapping=aes(y = -height/10), 
                          width = height/10,
                          color = secondary_color,
                          fill = input$color) + 
            labs(x = "IMDb Rating", y = "Frequency") +
            theme_minimal()
        
        

    })
    
    output$service_plot <- renderPlot({
        sp <-c()
        services <- c("Disney+","Hulu","Netflix","PrimeVideo")
        service_colors <- c("dodgerblue3","seagreen2","red2","deepskyblue1")
        names(service_colors) <- services
        
        for(service in services){
            right_service <- titles[[service]]==1
            p <- titles %>% 
                filter(right_service) %>% 
                ggplot(mapping = aes(y=IMDb)) +
                geom_boxplot(width = 1,
                             fill = service_colors[service],
                             color = secondary_color) +
                theme_minimal() +
                ylim(0,10) +
                theme(
                    axis.text.x = element_blank(),
                    panel.grid = element_blank(),
                    plot.title = element_text(hjust = 0.5)
                )
            if(service!="Disney+"){
                p <- p + theme(axis.text.y = element_blank()) + 
                    labs(y = "", title = service)
            
            }else{
                p <- p + theme(axis.line.y = element_line(color = secondary_color,
                                                          size = 1),) +
                    labs(y = "IMDb Rating",title = service)
            }
            sp[[service]] <- p
        }

        ggarrange(plotlist = sp, nrow = 1)
        
        
    })
    
    output$age_plot <- renderPlot({
        ap <- c()
        age_groups <- c("all","7+","13+","16+","18+")
        age_colors <- c("seagreen4","slateblue2","orchid4","firebrick3","sienna2")
        names(age_colors) <- age_groups
        
        for(age in age_groups){
            p <- titles %>% 
                filter(Age == age) %>% 
                ggplot(aes(y = IMDb)) +
                geom_boxplot(width = 1,
                             fill = age_colors[age],
                             color = secondary_color) + 
                theme_minimal() +
                theme(axis.text.x = element_blank(),
                      panel.grid = element_blank(),
                      plot.title = element_text(hjust = .5))
            
            if(age == "all"){
                p <- p + theme(axis.line.y = element_line(color = secondary_color,
                                                          size = 1),) +
                    labs(y = "IMDb Rating",title = "All Ages")
            }else{
                p <- p + labs(title = age, axis.text.y = element_blank())
            }
            ap[[age]] <- p
        }
        
        ggarrange(plotlist = ap, nrow = 1)
        
        
    })

})
