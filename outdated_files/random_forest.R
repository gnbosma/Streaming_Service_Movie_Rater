library (dplyr)
library (tidyr)


load("lm_data.rda")



library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)   


set.seed(123)

m1 <- randomForest(
  formula = rt_score ~ age_rating + hulu + disneyplus + netflix + amazonprime +  year + imdb_score + actor_score1 + actor_score2 + actor_score3,
  data    = lm_data[complete.cases(lm_data),]
)
plot (m1)
varImpPlot(m1,type=2)

#from this plot we see that the IMDb rating is highly influential in determining the Rotten Tomatoes score, followed by actor_score1, year (of release), actor_score2, and actor_score3. Availability on streaming service was not found to be highly influential in determining IMDb rating.

