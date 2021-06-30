##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

#Adding more libraries
library(ggplot2)
library(lubridate)
library(knitr) 
library(kableExtra)
library(Matrix)
library(matrixStats)
library(dplyr)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

head(edx)
head(validation)

#unique users and movies in the edx dataset
edx %>%
  summarize(users = n_distinct(userId),
            movies = n_distinct(movieId),
            avg_rating = round(mean(edx$rating),2),
            uniq_genres = n_distinct(edx$genres))

#unique users and movies in the validation dataset
validation %>%
  summarize(users = n_distinct(userId),
            movies = n_distinct(movieId),
            avg_rating = round(mean(validation$rating),2),
            uniq_genres = n_distinct(validation$genres))


#Summary of the edx and validation data frames using kable
summary(edx) %>%  kable(caption = "Top rows of edx data frame") %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("scale_down", "HOLD_position"))


summary(validation) %>%  kable(caption = "Top rows of validation data frame") %>%
  kable_styling(font_size = 10, position = "center",
                latex_options = c("scale_down", "HOLD_position"))

# Distribution of ratings by movieId
edx %>%
  dplyr::count(movieId) %>%
  ggplot(aes(n)) +
  geom_histogram( bins=20, color = "black") +
  scale_x_log10() +
  ggtitle("Ratings by movieID") +
  labs(x="movieId" ,y="number of ratings")

# Distribution of ratings by userID
edx %>%
  dplyr::count(userId) %>%
  ggplot(aes(n)) +
  geom_histogram( bins=20, color = "black") +
  scale_x_log10() +
  ggtitle("Ratings by userID") +
  labs(x="userId" , y="number of ratings") 

#Star ratings
#options(scipen = 100) # To avoid scientific notation(..optional)
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_point()+
  labs(x="Star Rating", y="Number of ratings") +
  ggtitle("Number of ratings per star")

#the top ten title and genres
edx %>%
  group_by(title, genres) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

# the data frame top_title contains the top 15 movies which count the major number of ratings
top_title <- edx %>%
  group_by(title) %>%
  summarize(count=n()) %>%
  top_n(15,count) %>%
  arrange(desc(count))

#bar chart of top_title
top_title %>%
  ggplot(aes(x=reorder(title, count), y=count)) +
  geom_bar(stat="identity", fill="black") + coord_flip(y=c(0, 40000)) +
  labs(x="Title", y="Number of Ratings") +
  geom_text(aes(label= count), hjust=-0.1, size=3) +
  labs(title="Top 15 Movie Titles")


#movie ratings by genres
memory.limit(size=56000) #expanding memory allocated to R 
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


#Testing the correlation between ratings, userId and MovieID 

#Number of ratings per movie
movie_ratings <- edx %>% group_by(movieId) %>% summarize(n = n())

#Average Movie Rating 
avg_movie_rate <- edx %>% group_by(movieId) %>% summarize(avg_rate = mean(rating))

#correlation data
cor_dat <- edx %>% select(rating, movieId, userId) %>%
  left_join(movie_ratings, by = "movieId") %>%
  left_join(avg_movie_rate, by = 'movieId')
head(cor_dat)



edx$userId <- as.factor(edx$userId)
edx$movieId <- as.factor(edx$movieId)

#converting userID and movieId to numeric vectors to use the sparseMatrix function
edx$userId <- as.numeric(edx$userId)
edx$movieId <- as.numeric(edx$movieId)
sparse_rating <- sparseMatrix(i = edx$userId,
                               j = edx$movieId ,
                               x = edx$rating,
                               dims = c(length(unique(edx$userId)),
                                        length(unique(edx$movieId))),
                               dimnames = list(paste("u", 1:length(unique(edx$userId)), sep = ""),
                                               paste("m", 1:length(unique(edx$movieId)), sep = "")))

sparse_rating[1:20,1:20] #each row is a user and each column is a movie

#extracting the year as a column 
edx <- edx %>% mutate(year = as.numeric(str_sub(title,-5,-2)))
validation <- validation %>% mutate(year = as.numeric(str_sub(title,-5,-2)))

head(edx)
head(validation)

# Star Rating per Release Year
edx %>% group_by(year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year, rating)) +
  geom_point() +
  stat_smooth(se = TRUE,
              n = 80,
              span = 0.75,
              fullrange = FALSE,
              level = 0.95,
              method.args = list(),
              inherit.aes = TRUE)+
  ggtitle("Star Rating per Release Year : edx dataset")

# Star Rating per Release Year
validation %>% group_by(year) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(year, rating)) +
  geom_point() +
  stat_smooth(se = TRUE,
              n = 80,
              span = 0.75,
              fullrange = FALSE,
              level = 0.95,
              method.args = list(),
              inherit.aes = TRUE)+
  ggtitle("Star Rating per Release Year : validation dataset")




                          data_frame(method="Movie Effect Model",  
 

mu <- mean(edx$rating)
mu

#calculate b_i on the training dataset
movie_m <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
movie_m

predicted_ratings_bi <- mu + validation %>%
  left_join(movie_m, by=movieId’) %>%
  .$b_i

                                    RMSE = model_1_rmse ))

rmse_results %>% knitr::kable()



memory.limit(size=56000) #expanding memory allocated to R








  










'""


prpedicted_ratings_movie_norm$$pred $predicted_ratings_movie_norm$prpred