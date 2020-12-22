library(shiny)
library(shinythemes)
library(tidyverse)
library(ggpubr)

#Load and clean movies data
#load("../movies.rda")

data_url <- "https://raw.github.com/gnbosma/Streaming_Service_Movie_Rater/main/lm_data.rda"
load(url(data_url))

titles <- lm_data %>% ungroup() %>% 
    mutate(age_rating = ifelse(age_rating=="all","All Ages",age_rating)
           ) %>% 
    filter(!is.na(imdb_score))

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
    
    #Primary histogram for first page
    output$histplot <- renderPlot({
        
        #Filter according to inputs
        right_service <- titles %>% 
            select(all_of(input$service)) %>% 
            rowSums() %>% as.logical()
            
         plot_data <- titles %>% 
             filter(right_service,
                    inRange(rt_score, input$tscore),
                    inRange(year,input$yr),
                    age_rating %in% input$ages,
                    !is.na(rt_score),!is.na(age_rating)
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
    
    #Boxplots for streaming services
    output$service_plot <- renderPlot({
        sp <-c()
        
        #Iterate through each service and create boxplot
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
        
        #Arrange plots
        ggarrange(plotlist = sp, nrow = 1)
        
        
    })
    
    #Boxplots for age ratings
    output$age_plot <- renderPlot({
        ap <- c()
        age_groups <- c("All Ages","7+","13+","16+","18+")
        age_colors <- c("seagreen4","slateblue2","orchid4",
                        "firebrick3","sienna2")
        names(age_colors) <- age_groups
        
        #Iterate through each age rating 
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
    
    #Scatterplot of IMDb ratings by rotten tomatoes ratings
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
    
    # #Table for model output 
    # output$lm_table <- renderDataTable({
    #     
    #     #Create linear model
    #     form <- "imdb_score ~ disneyplus + hulu + netflix + amazonprime"
    #     
    #     include_vars <- c("rt_score", "actor_score1")
    #     
    #     if(length(include_vars) > 0){
    #       
    #         form <- paste(form, paste(include_vars, collapse = " + "), sep = " + ")            
    #         
    #     }
    #     
    #     linear_model <- lm(formula = formula(form), data = titles)
    #     
    #     results <- as.data.frame(summary(linear_model)$coefficients)
    #     # results <- results %>% mutate(Variable = rownames(results)) %>% 
    #     #     select(Variable,everything())
    #    # results$var <- rownames(results)
    #     cbind(rownames(results),data.frame(results,row.names = NULL))
    # 
    #     
    #     
    # })

    
    
    #Fit linear model
    lm_result <- reactive({
        
         #Create formula based on input
         form <- "imdb_score ~ disneyplus + hulu + netflix + amazonprime"
     
         if(length(input$include_vars) > 0){
             
             form <- paste(form, paste(input$include_vars, collapse = " + "), sep = " + ")            
             
         }
         
         #Fit model and format output
         linear_model <- lm(formula = formula(form), data = titles)
         results <- as.data.frame(summary(linear_model)$coefficients)
         results <- cbind(rownames(results),round(data.frame(results,row.names = NULL),3))
         results  
    })
    
     
     #Produce Table for model
     output$lm_table1 <- DT::renderDataTable(
         
         DT::datatable(lm_result(), options = list(paging = FALSE, searching = FALSE))
         
     )
    
    
    
})
