#This file is just to prep some plots before they get added to the shiny, 
#since it's easier to play with ggplot without having to think about shiny
#at the same time.

library(tidyverse)
library(ggpubr)

#Choose the location of data in your files. Too big to go on github
load("/home/shared/biostat625w2020/data_clean.rda")

titles <- dat %>% 
  distinct(Title,.keep_all = T) %>% 
  mutate(RottenTomatoes = RottenTomatoes * 100) %>% 
  filter(!is.na(IMDb),!is.na(RottenTomatoes))

inRange <- function(x,range){
  x <- as.numeric(x)
  range <- as.numeric(range)
  return(x >= range[1] & x <= range[2])
}

#set default inputs for analysis
input1 <- c()
input1$service <- c("Disney+","Hulu","Netflix",
                   "PrimeVideo")
input1$ages <- c("all","7+","13+","16+","18+")
input1$tscore <- c(0,100)
input1$yr <- c(1901,2020)
input1$bins <- 20
input1$color <- "sienna2"

#Filter according to inputs
right_service <- titles %>% 
  select(all_of(input1$service)) %>% 
  rowSums() %>% as.logical()

plot_data <- titles %>% 
  filter(right_service,
         inRange(RottenTomatoes, input1$tscore),
         inRange(Year,input1$yr),
         Age %in% input1$ages
  )

#Make sure bin count is fine
nbins <- input1$bins
if(nbins > nrow(plot_data)){
  nbins <- nrow(plot_data)
}


#Create histogram
ph <- ggplot(plot_data) + 
  geom_histogram(mapping = aes(x = IMDb), 
                 bins = nbins, 
                 color = "gray60",
                 fill = input1$color) +
  labs(y = "Count", x = "") + 
  xlim(0,10) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_blank(),
    panel.grid.minor = element_blank()
  )

#Create boxplot
pb <- ggplot(plot_data) + 
  geom_boxplot(mapping = aes(x = IMDb), 
               color = "gray60",
               fill = input1$color) +
  labs(x = "IMDb Rating", y="") +
  xlim(0,10) +
  theme_minimal() + 
  theme(axis.text.y = element_text(color = "white"),
        panel.grid = element_blank(),
        axis.ticks.x = element_line(),
        axis.line.x.bottom = element_line(color = "gray27",
                                          size = .5))

ggarrange(ph,pb,ncol = 1, nrow = 2, heights = c(4,1))

ggplot(plot_data,aes(x=IMDb)) + 
  geom_histogram(bins = nbins, 
                 color = "gray60", 
                 fill = input1$color) + 
  geom_boxplot(mapping=aes(y = -60), 
               width = 50,
               color = "gray60",
               fill = input1$color) + 
  labs(x = "IMDb Rating", y = "Frequency") +
  theme_minimal() +
  ylim(-100,800)


