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