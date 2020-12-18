library (dplyr)
library (tidyr)

load(file = here::here("data/data_all.rda"))

#0: clean up titles dataset
#remove adult films and keep distinct entries
titles = titles %>% filter (isAdult == F) %>% distinct()

#1: add tv shows and movies to one joint dataset dat
tvshows <- tvshows %>%
  select (-1) %>% #remove column of row numbers
  rename (Type = type,
          RottenTomatoes = "Rotten Tomatoes",
          PrimeVideo = "Prime Video")

movies <- movies %>%
  select (-1) %>% #remove column of row numbers 
  rename (RottenTomatoes = "Rotten Tomatoes",
          PrimeVideo = "Prime Video") %>%
  select (names(tvshows))

dat <- rbind (tvshows, movies)

#2: filter for titles and metadata in dat and join with dat
titles1 <- titles %>%
  filter (primaryTitle %in% dat$Title | originalTitle %in% dat$Title) %>%
  select (tconst, titleType, primaryTitle,  runtimeMinutes, genres) %>%
  left_join (metadata, by = c("primaryTitle" = "title")) %>%
  filter (tconst == titleId) %>%
  mutate_if(is.character, list(~na_if(., "\\N")))




merged_regions = aggregate(titles1$region, list(titles1$primaryTitle), paste, collapse=",")
merged_languages = aggregate(titles1$language, list(titles1$primaryTitle), paste, collapse=",")
tomerge = inner_join (merged_regions, merged_languages, by = "Group.1")
titles1 = left_join (titles1, tomerge, by = c("primaryTitle" = "Group.1")) %>%
  select (-c(region, language)) %>%
  rename (region = x.x,
          language = x.y)

dat <- left_join (dat, titles1, by = c("Title" = "primaryTitle")) #need to check that this is accurate