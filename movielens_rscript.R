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
library(anytime)
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

# the top 15 movies which count the major number of ratings
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

#transforming userID and movieId as factors for analysis
edx$userId <- as.factor(edx$userId) 
edx$movieId <- as.factor(edx$movieId) 
edx$genres <- as.factor(edx$genres) 
edx$timestamp <- as.POSIXct(edx$timestamp, origin = "1970-01-01") # Convert timestamp to POSIXct.

edx <- edx %>% 
  mutate(title = str_trim(title)) %>%
  extract(title, c("title_tmp", "year"), # extracting the release year of the movie from the title column
          regex = "^(.*) \\(([0-9 \\-]*)\\)$",
          remove = F) %>%
  mutate(year = if_else(str_length(year) > 4, #createing year column.
                        as.integer(str_split(year, "-",
                                             simplify = T)[1]),
                        as.integer(year))) %>%
  mutate(title = if_else(is.na(title_tmp), title, title_tmp)) %>%
  select(-title_tmp)  %>%
  mutate(genres = if_else(genres == "(no genres listed)",
                          `is.na<-`(genres), genres))

edx <- edx %>% mutate(year_rate = year(timestamp)) # creating a column for the year the movie was rated

# It extracts the year that the rate was given by the user.
edx <- edx %>% select(-timestamp) # removing the timsestamp column (optional)
edx$genres <- as.factor(edx$genres)

head(edx)

summary(edx$rating) #edx rating summary

#genres rating using the mean
edx %>% group_by(genres) %>% summarize(avg_rating = mean(rating)) %>% arrange(desc(avg_rating))

edx <- edx %>% select(userId, movieId, rating) #Three parameter of interest
# Create the index
test_index <- createDataPartition(edx$rating, times = 1, p = .2, list = F)
#Creating the train and test sets
train <- edx[-test_index, ] # The train set
test <- edx[test_index, ] # The test set
test <- test %>% # The same movieId and usersId appears in both set. (Not the same cases)
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

dim(train)
dim(test)

#generating the model
mu_0 <- mean(train$rating) # Mean rating on train set
RMSE_0 <- RMSE(test$rating,mu_0) # RMSE in the test set.
RMSE_0


# obtaining prediction using the mean from movie and user effect
mu <- mean(train$rating)

m_avgs <- train %>% 
  group_by(movieId) %>%
  summarize(mi = mean(rating - mu)) #movie effect

u_avgs <- test %>%
  left_join(m_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(ui = mean(rating - mu -mi)) #user effect

predicted_ratings <- test %>%
  left_join(m_avgs, by = "movieId") %>%
  left_join(u_avgs, by = "userId") %>%
  mutate(pred = mu +mi +ui) %>% .$pred

RMSE_1 <- RMSE(predicted_ratings, test$rating)
RMSE_1  


#prediction using the validation dataset
validation <- validation %>% select(userId, movieId, rating) #we are interested in userId, movieId, & rating
#treating userId & movieId as factors
validation$userId <- as.factor(validation$userId)
validation$movieId <- as.factor(validation$movieId)
validation <- validation[complete.cases(validation), ]

#The prediction
mu <- mean(train$rating)

m_avgs <- train %>%
  group_by(movieId) %>%
  summarize(mi = mean(rating - mu))

u_avgs <- test %>%
  left_join(m_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(ui = mean(rating - mu -mi))

predicted_ratings <- test %>%
  left_join(m_avgs, by = "movieId") %>%
  left_join(u_avgs, by = "userId") %>%
  mutate(pred = mu +mi +ui) %>% .$pred


predicted_val <- validation %>%
  left_join(m_avgs, by = "movieId") %>%
  left_join(u_avgs, by = "userId") %>%
  mutate(pred = mu +mi +ui) %>% .$pred

RMSE_VAL <- RMSE(predicted_val, validation$rating, na.rm = T)
RMSE_VAL

#regularizing test data

#with regularization, we evaluate different values for lambda, which a tuning parameter.

lambda_values <- seq(0, 10, .25)
t_RMSE_reg <- sapply(lambda_values, function(l){
  
  mu <- mean(train$rating)
  
 mi <- train %>%
    group_by(movieId) %>%
    summarize(mi = sum(rating - mu)/(n()+l)) #movie effect
  
 ui <- train %>%
    left_join(mi, by="movieId") %>%
    group_by(userId) %>%
    summarize(ui = sum(rating -mi - mu)/(n()+l)) #user effect
  
  predicted_ratings <- test %>%
    left_join(mi, by = "movieId") %>% 
    left_join(ui, by = "userId") %>%
    mutate(pred = mu +mi +ui) %>% .$pred
  
  return(RMSE(predicted_ratings, test$rating))
})

t_lambda <- lambda_values[which.min(t_RMSE_reg)]
t_lambda  #Lambda minimizing the RMSE

#regularization on validation dataset
val_RMSE_reg <- sapply(lambda_values, function(l){
  
  mu <- mean(train$rating)
  
 mi <- train %>%
    group_by(movieId) %>%
    summarize(mi = sum(rating - mu)/(n()+l)) #movie effect
  
 ui <- train %>%
    left_join(mi, by="movieId") %>%
    group_by(userId) %>%
    summarize(ui = sum(rating -mi - mu)/(n()+l)) #user effect
  
  predicted_val_reg <- validation %>%
    left_join(mi, by = "movieId") %>% 
    left_join(ui, by = "userId") %>%
    mutate(pred = mu +mi +ui) %>% .$pred
  
  return(RMSE(predicted_val_reg, validation$rating, na.rm = T))
})


val_lambda <- lambda_values[which.min(val_RMSE_reg)]
val_lambda #  Lambda minimizing the RMSE

min_rmse <- min(val_RMSE_reg) # Best RMSE
min_rmse

