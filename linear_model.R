#Model building
library(tidyverse)

#load("/home/shared/biostat625w2020/FINAL.rda")

#Aggregate merged actor, movie data by grouping by imbdb_id
lm_data <- FINAL %>% 
  mutate(rt_score = as.numeric(str_remove(rt_score,"%"))) %>% 
  group_by(imdb_id,title,age_rating,
           hulu,disneyplus, netflix,
           amazonprime,year,rt_score,imdb_score) %>% 
  summarise(actor_score1 = mean(score1),
            actor_score2 = mean(score2),
            actor_score3 = mean(score3)
            
  )


model_form <- imdb_score ~ rt_score + year + disneyplus + hulu + 
  amazonprime + netflix + actor_score1 + actor_score2 + actor_score3
  
model1 <- lm(formula = model_form, 
             data = lm_data)
summary(model1)
  