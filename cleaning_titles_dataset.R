library (dplyr)
library (tidyr)

load(file = here::here("data/data_all.rda"))

#0: clean up titles dataset
#remove adult films and keep distinct entries
titles <- titles %>% filter (isAdult == F) %>% 
  distinct(primaryTitle, .keep_all = T)

#1: merge tvshows and movies from kaggle
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

tvshows$Title <- as.character (tvshows$Title)
movies$Title = as.character (movies$Title)

dat <- as.data.frame(rbind (tvshows, movies))

#2: filter for titles only in the kaggle datasets and select the last season of a tvseries for the "year" (this is how the kaggle dataset decided on the rotten tomatoes score)
titles <- titles %>%
  filter (primaryTitle %in% dat$Title | originalTitle %in% dat$Title) %>%
  arrange (primaryTitle, -startYear) %>%
  distinct (primaryTitle, startYear, .keep_all = T) %>%
  select (tconst, titleType, primaryTitle,  originalTitle, runtimeMinutes, genres, startYear) 

#3: combine the primary and original titles into one column (primary title is the one used to publicize, original title is usually in the language in which the feature was released)
titles = rbind (titles %>%
                  select (-primaryTitle) %>%
                  rename (ptitle = originalTitle),
                titles %>%
                  select (-originalTitle) %>%
                  rename (ptitle = primaryTitle)) %>%
  distinct (tconst, ptitle, .keep_all = T)



#4: change any empty fields in titles or metadata into "NA"
titles[titles == "\\N"]<-NA
metadata[metadata == "\\N"]<-NA

#5: add a country of origin field (region is used to determine the primary title for a region, not necessarily the country in which the feature was made)
metadata1 = metadata %>%
  arrange(rowSums(is.na(.))) %>%
  distinct (titleId, .keep_all = T)

# true_region = metadata1 %>%
#   filter (isOriginalTitle == T) %>%
#   select (titleId, region) %>%
#   rename (country_of_origin = region)
# 
# metadata1 = metadata %>%
#   left_join (true_region)

#6: keep entries that have multiple languages as the one that is not "NA" (I manually checked these, and for duplicate entries with differing languages the languages were _ and NA)
# metadata1 = metadata1 %>%
#   arrange(titleId, language) %>% 
#   distinct(titleId, language, .keep_all = TRUE) 

#7: join titles and metadata datasets
titles = titles %>%
  left_join (metadata1, by = c("tconst" = "titleId")) %>%
  filter (ptitle == title ) %>%
  select (tconst, titleType, runtimeMinutes, genres, title, language)


#8: join imdb and kaggle datasets

dat <- left_join (as.data.frame(dat), as.data.frame(titles), by = c("Title" = "title")) #need to check that this is accurate

save(dat, file = "data/clean_dat.rda")
#ignore anything past this----------
View(dat)



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

movies$Title = as.character (movies$Title)
tvshows$Title = as.character (tvshows$Title)

dat <- as.data.frame(rbind (tvshows, movies))

#2: filter for titles and metadata in dat and join with dat
titles1 <- titles %>%
  filter (primaryTitle %in% dat$Title | originalTitle %in% dat$Title) %>%
  select (tconst, titleType, primaryTitle,  originalTitle, runtimeMinutes, genres, startYear) 

titles1 = rbind (titles1 %>%
                   select (-primaryTitle) %>%
                   rename (ptitle = originalTitle),
                 titles1 %>%
                   select (-originalTitle) %>%
                   rename (ptitle = primaryTitle)) %>%
  distinct (tconst, ptitle, .keep_all = T)




titles1[titles1 == "\\N"]<-NA

#filter metadata for US titles
metadata[metadata == "\\N"]<-NA

true_region = metadata %>%
  filter (types == "original") %>%
  select (titleId, region) %>%
  rename (country_of_origin = region)

metadata1 = metadata %>%
  left_join (true_region)

titles1 = titles1 %>%
  left_join (metadata1, by = c("tconst" = "titleId")) %>%
  filter (ptitle == title ) %>%
  select (tconst, titleType, runtimeMinutes, genres, title, country_of_origin, language)


titles1 = titles1 %>%
  arrange(tconst, is.na(language)) %>% 
  distinct(tconst, .keep_all = TRUE) 


dat <- left_join (as.data.frame(dat), as.data.frame(titles1), by = c("Title" = "title")) #need to check that this is accurate



merged_regions = aggregate(titles1$region, list(titles1$primaryTitle), paste, collapse=",")
merged_languages = aggregate(titles1$language, list(titles1$primaryTitle), paste, collapse=",")
tomerge = inner_join (merged_regions, merged_languages, by = "Group.1")
titles1 = left_join (titles1, tomerge, by = c("primaryTitle" = "Group.1")) %>%
  select (-c(region, language)) %>%
  rename (region = x.x,
          language = x.y)

dat <- left_join (dat, titles1, by = c("Title" = "primaryTitle")) #need to check that this is accurate