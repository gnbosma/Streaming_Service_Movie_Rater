library(shiny)
library(shinythemes)
library(tidyverse)
library(ggpubr)

#Load and clean movies data
#load("../movies.rda")

data_url <- "https://raw.github.com/gnbosma/Streaming_Service_Movie_Rater/main/movies.rda"
load(url(data_url))

titles <- movies_clean %>% 
    mutate(rt_score = 100 * rt_score,
           age_rating = ifelse(age_rating=="all","All Ages",age_rating)
           ) %>% 
    filter(!is.na(imdb_score),!is.na(rt_score))

#Create list documenting service names, labels, and colors
service_list <- c()
service_list$disneyplus <- c("Disney+","dodgerblue3")
service_list$hulu <- c("Hulu","seagreen2")
service_list$netflix <- c("Netflix","red2")
service_list$amazonprime <- c("Prime Video","deepskyblue1")
for(name in names(service_list)){
    names(service_list[[name]]) <- c("label","color")
}

#Set secondary color for plots
secondary_color = "gray60"

#Function to check whether inputted x falls into given range
inRange <- function(x,range){
    x <- as.numeric(x)
    range <- as.numeric(range)
    return(x >= range[1] & x <= range[2])
}

#The server
shinyServer(function(input, output) {
    
    output$histplot <- renderPlot({
        
        #Filter according to inputs
        right_service <- titles %>% 
            select(all_of(input$service)) %>% 
            rowSums() %>% as.logical()
            
         plot_data <- titles %>% 
             filter(right_service,
                    inRange(rt_score, input$tscore),
                    inRange(year,input$yr),
                    age_rating %in% input$ages
                    )
        
        #Make sure bin count is fine
        nbins <- input$bins
        if(nbins > nrow(plot_data)){
            nbins <- nrow(plot_data)
        }
        
        #Create figure
        p1 <- ggplot(plot_data,aes(x=imdb_score)) + 
            geom_histogram(bins = nbins, 
                           color = secondary_color, 
                           fill = input$color)
        
        height <- max(ggplot_build(p1)$data[[1]]$count)
        
        p1 + geom_boxplot(mapping=aes(y = -height/10), 
                          width = height/10,
                          color = secondary_color,
                          fill = input$color) + 
            labs(x = "Rating out of 10", 
                 y = "Frequency", 
                 title = "IMDb Movie Ratings: Histogram & Boxplot") +
            xlim(0,10) +
            theme_minimal() +
            theme(
                plot.title = element_text(size = 16, hjust = 0.5),
                axis.line.x = element_line(color = "darkgrey",size = 1)
            )
        
        

    })
    
    output$service_plot <- renderPlot({
        sp <-c()
        
        for(service in names(service_list)){
            right_service <- titles[[service]]==1
            
            p <- titles %>% 
                filter(right_service) %>% 
                ggplot(mapping = aes(y=imdb_score)) +
                geom_boxplot(width = 1,
                             fill = service_list[[service]]["color"],
                             color = secondary_color) +
                theme_minimal() +
                ylim(0,10) +
                theme(
                    axis.text.x = element_blank(),
                    panel.grid = element_blank(),
                    plot.title = element_text(hjust = 0.5)
                )
            if(service!="disneyplus"){
                p <- p + theme(axis.text.y = element_blank()) + 
                    labs(y = "", 
                         title = service_list[[service]]["label"])
            
            }else{
                p <- p + theme(axis.line.y = element_line(color = secondary_color,
                                                          size = 1),) +
                    labs(y = "IMDb Rating",
                         title = service_list[[service]]["label"])
            }
            
            sp[[service]] <- p
        }

        ggarrange(plotlist = sp, nrow = 1)
        
        
    })
    
    output$age_plot <- renderPlot({
        ap <- c()
        age_groups <- c("All Ages","7+","13+","16+","18+")
        age_colors <- c("seagreen4","slateblue2","orchid4",
                        "firebrick3","sienna2")
        names(age_colors) <- age_groups
        
        for(age in age_groups){
            p <- titles %>% 
                filter(age_rating == age) %>% 
                ggplot(aes(y = imdb_score)) +
                geom_boxplot(width = 1,
                             fill = age_colors[age],
                             color = secondary_color) + 
                ylim(0,10) +
                theme_minimal() +
                theme(axis.text.x = element_blank(),
                      panel.grid = element_blank(),
                      plot.title = element_text(hjust = .5))
            
            if(age == "All Ages"){
                p <- p + theme(axis.line.y = element_line(color = secondary_color,
                                                          size = 1),) +
                    labs(y = "IMDb Rating",title = "All Ages")
            }else{
                p <- p + labs(title = age, y = "") +
                    theme(axis.text.y = element_blank())
            }
            ap[[age]] <- p
        }
        
        ggarrange(plotlist = ap, nrow = 1)
        
        
    })
    
    output$ts_plot <- renderPlot({
        right_service <- titles %>% 
            select(all_of(input$ts_service)) %>% 
            rowSums() %>% as.logical()
        
        titles %>% 
            filter(right_service,
                   age_rating %in% input$ts_ages) %>% 
            ggplot(aes(x = rt_score, y = imdb_score)) +
            geom_point(color = "orchid4",
                       alpha = 0.4) +
            xlim(0,100) + 
            ylim(0,10) +
            theme_bw() +
            labs(x = "Tomatometer Score", y ="IMDb Ratings",
                 title = "IMDb Movie Ratings by Tomatometer Score")+
            theme(plot.title = element_text(size = 14))
        
        
    })

    
    
    
})
