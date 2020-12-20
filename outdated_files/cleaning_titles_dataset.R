library (dplyr)
library (tidyr)
library (fuzzyjoin)

# load(file = here::here("data/data_all.rda"))
# save (movies, titles, file = "data/movies_titles.rda")
load (file = "data/movies_titles.rda")



#1: clean up titles dataset
#remove adult films and keep distinct entries (by title and year)
titles <- titles %>% 
  filter (isAdult == F) %>% 
  select (tconst, titleType, primaryTitle, originalTitle, runtimeMinutes, genres, startYear) %>%
  filter (titleType %in% c("movie", "tvMovie"))


titles = rbind (titles %>% 
                  distinct (primaryTitle, tconst, .keep_all = T) %>%
                  select (-originalTitle) %>%
                  rename (title = primaryTitle),
                titles %>%
                  distinct (originalTitle, tconst, .keep_all = T) %>%
                  select (-primaryTitle) %>%
                  rename (title = originalTitle))


#2: merge movies from kaggle with imdb database using primaryTitle
movies <- movies %>%
  select (-1) %>% #remove column of row numbers 
  rename (RottenTomatoes = "Rotten Tomatoes",
          PrimeVideo = "Prime Video") 

movies$Title = as.character (movies$Title)

#first iteration-- direct match
movies1 = left_join (movies, titles, by = c("Title" = "title")) %>%
  distinct (Title, tconst, .keep_all = T)

nrow(movies1) - length (unique (movies1$Title)) #number of multiple matches to imdb_id

movies1 = rbind (  movies1 %>%
                     filter (!is.na(tconst)) %>%
                     group_by (Title) %>%
                     add_count() %>%
                     ungroup() %>%
                     filter (n == 1) %>%
                     mutate (diff_from_Year = abs (Year - startYear)) %>%
                     filter (diff_from_Year < 5) %>%
                     select (-n, -diff_from_Year),
                   movies1 %>%
                     filter (!is.na(tconst)) %>%
                     group_by (Title) %>%
                     add_count() %>%
                     ungroup() %>%
                     filter (n > 1) %>%
                     mutate (diff_from_Year = abs (Year - startYear)) %>%
                     arrange (Title, diff_from_Year) %>%
                     distinct (Title, Year, .keep_all = T) %>%
                     select (-n, -diff_from_Year)) %>%
  select (-ID, -startYear, -Type) %>%
  rename (imdb_genres = genres,
          imdb_runtimeMinutes = runtimeMinutes,
          kaggle_genres = Genres,
          kaggle_runtimeMinutes = Runtime,
          title = Title,
          year = Year,
          age_rating = Age,
          imdb_score = IMDb,
          rt_score = RottenTomatoes,
          directors = Directors,
          country = Country,
          language = Language,
          imdb_id = tconst,
          netflix = Netflix,
          hulu = Hulu,
          amazonprime = PrimeVideo,
          disneyplus = "Disney+"
  ) %>%
  distinct (title, imdb_id, .keep_all = T)



#only use movie titles that haven't already been matched
movies = movies %>% 
  filter (!(Title %in% movies1$title))

titles = titles %>%
  filter (! (tconst %in% movies1$imdb_id)) %>%
  mutate(bin = ntile(tconst, 10))



#second iteration-- fuzzy join
for (i in 1:10) {
  temp_movies = fuzzyjoin::stringdist_left_join (movies, 
                                                 titles %>%
                                                   filter (bin == i),
                                                 by = c("Title" = "title"),
                                                 max_dist = 1,
                                                 ignore_case = T,
                                                 distance_col = "distance") %>%
    distinct (Title, tconst, .keep_all = T)
  print (paste (i, "done"))
  if (i == 1) 
    movies2 = temp_movies
  else
    movies2 = rbind (movies2, temp_movies)
  
}

sum(!is.na(movies2$tconst))
nrow(movies2) - length (unique (movies2$Title)) #number of multiple matches to imdb_id

movies2 = movies2 %>%
  select (-title)
movies2 = rbind (  movies2 %>%
                     filter (!is.na(tconst)) %>%
                     group_by (Title) %>%
                     add_count() %>%
                     ungroup() %>%
                     filter (n == 1) %>%
                     mutate (diff_from_Year = abs (Year - startYear)) %>%
                     filter (diff_from_Year < 5) %>%
                     select (-n, -diff_from_Year),
                   movies2 %>%
                     filter (!is.na(tconst)) %>%
                     group_by (Title) %>%
                     add_count() %>%
                     ungroup() %>%
                     filter (n > 1) %>%
                     mutate (diff_from_Year = abs (Year - startYear)) %>%
                     arrange (Title, diff_from_Year) %>%
                     distinct (Title, Year, .keep_all = T) %>%
                     select (-n, -diff_from_Year)) %>%
  select (-ID, -startYear, -Type) %>%
  rename (imdb_genres = genres,
          imdb_runtimeMinutes = runtimeMinutes,
          kaggle_genres = Genres,
          kaggle_runtimeMinutes = Runtime,
          title = Title,
          year = Year,
          age_rating = Age,
          imdb_score = IMDb,
          rt_score = RottenTomatoes,
          directors = Directors,
          country = Country,
          language = Language,
          imdb_id = tconst,
          netflix = Netflix,
          hulu = Hulu,
          amazonprime = PrimeVideo,
          disneyplus = "Disney+"
  ) %>%
  distinct (title, imdb_id, .keep_all = T) %>%
  select (-bin, -distance)


movies_clean = rbind (movies1, movies2)
save (movies_clean, "movies.rda")