###############################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(recommenderlab)) install.packages("recommenderlab", repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")

options(digits = 4)
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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




#### extract the year from the name column, adding a release_year column, creating a review time column,
#### rounding the review time to a week
edx_tidy <- edx %>% 
  mutate(release_year = (str_extract(title, "[(]\\d{4}[)]")),
         release_year = str_replace_all(release_year, "\\(|\\)", ""),
         title = str_remove(title, "[(]\\d{4}[)]"),
         review_time = as_datetime(timestamp), 
         review_time = round_date(review_time, unit = "week",
                                  week_start = getOption("lubridate.week.start", 7)))



#### Showing some exploratory data

edx_tidy %>% head()

###number of users that rated movies and the number of movies rated
edx_tidy %>% 
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

users <- sample(unique(edx_tidy$userId), 100)
edx_tidy %>% 
  filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")


#### Some movies are reated more than others
edx_tidy %>% 
  dplyr::count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")


### Some users are rating more than others
edx_tidy %>% 
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() +
  ggtitle("Users")


### Geners

edx_tidy %>% 
  group_by(genres) %>% 
  summarize(n = n())


## date reviewed on a scale y_log_10
edx_tidy %>% 
  group_by(review_time) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(review_time, n)) +
  geom_point(aes(review_time, n)) +
  scale_y_log10() +
  ggtitle("Review Time")


### Create train set and test set

#create data partition
test_index_1 <- createDataPartition(y = edx_tidy$rating, times = 1, p = 0.1, list = FALSE)

#create train and test sets
train_set <- edx_tidy[-test_index_1,]
test_set <- edx_tidy[test_index_1,]

#making sure there are the same movies and users
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>% 
  semi_join(train_set, by = "userId")



#The RMSE function

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings) ^ 2))
}

MSE <- function(true_ratings, predicted_ratings){
  mean((true_ratings - predicted_ratings) ^ 2)
}

MAE <- function(true_ratings, predicted_ratings){
  mean(abs(true_ratings - predicted_ratings))
}

#naive model based on avearage from the book

mu_hat_average <- mean(train_set$rating)
mu_hat_average



#building a rmse results table
evaluation_results <- tibble(method = "Simple average", RMSE = RMSE(test_set$rating, mu_hat_average),
                             MSE = MSE(test_set$rating, mu_hat_average), 
                             MAE = MAE(test_set$rating, mu_hat_average))


evaluation_results

#calculating the b_i - movie effect

movie_effect <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu_hat_average))

#modeling and predicting the movie effect

predicted_ratings <- mu_hat_average + test_set %>% 
  left_join(movie_effect, by = "movieId") %>% 
  pull(b_i)


###plotting the movie effect
movie_effect %>%
  qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))



### adding movie effect to evaluation table
evaluation_results <- bind_rows(evaluation_results, tibble(method = "Movie Effect", 
                                                           RMSE = RMSE(test_set$rating, predicted_ratings),
                                                           MSE = MSE(test_set$rating, predicted_ratings),
                                                           MAE = MAE(test_set$rating, predicted_ratings)))


evaluation_results


#adding the user effect

user_effect <- train_set %>% 
  left_join(movie_effect, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating - mu_hat_average - b_i))

#modeling and predicting the user effect

predicted_ratings <- test_set %>% 
  left_join(movie_effect, by = "movieId") %>% 
  left_join(user_effect, by = "userId") %>% 
  mutate(pred = mu_hat_average + b_i + b_u) %>% 
  pull(pred)



### plotting the user effect on the entire edx set
edx_tidy %>% 
  group_by(userId) %>%
  summarize(b_u = mean(rating)) %>%
  filter(n()>=100) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black")

### adding user effect to evaluation table
evaluation_results <- bind_rows(evaluation_results,
                                tibble(method = "Movie&User effect", 
                                       RMSE = RMSE(test_set$rating, predicted_ratings),
                                       MSE = MSE(test_set$rating, predicted_ratings),
                                       MAE = MAE(test_set$rating, predicted_ratings)))


#regularizing for small n's of users bias and movie bias

#defining parameter lambda
lambdas <- seq(0, 10, 0.25)

regularized_function <- function(l){
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu) / (n() + l))
  
  b_u <- train_set %>%
    left_join(b_i, by ="movieId") %>% 
    group_by(userId) %>% 
    summarize(b_u = sum(rating - b_i - mu) / (n() + l))
  
  predicted_ratings <- train_set %>% 
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>% 
    mutate(pred = mu + b_i + b_u) %>% 
    pull(pred)
  
  tibble(RMSE = RMSE(predicted_ratings,train_set$rating),
         MSE = MSE(predicted_ratings, train_set$rating),
         MAE = MAE(predicted_ratings,train_set$rating))
  
  
}

rmses <- sapply(lambdas, regularized_function)

#the optimal lambda is  
lambda <- lambdas[which.min(rmses)]
lambda

regularized_function(1.25)


#### creating a regularization function for the test set


regularized_function_test_set <- function(l){
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>% 
    summarize(b_i = sum(rating - mu) / (n() + l))
  
  b_u <- train_set %>%
    left_join(b_i, by ="movieId") %>% 
    group_by(userId) %>% 
    summarize(b_u = sum(rating - b_i - mu) / (n() + l))
  
  predicted_ratings <- test_set %>% 
    left_join(b_i, by = "movieId") %>% 
    left_join(b_u, by = "userId") %>% 
    mutate(pred = mu + b_i + b_u) %>% 
    pull(pred)
  
  tibble(RMSE = RMSE(predicted_ratings,test_set$rating),
         MSE = MSE(predicted_ratings, test_set$rating),
         MAE = MAE(predicted_ratings,test_set$rating))
  
  
}



#updating rmse results
### noting that since the target was to minimize the RMSE, the MAE and MSE are just descriptive

evaluation_results <- bind_rows(evaluation_results, 
                                tibble(method = "Regularized Movie and User effects", 
                                       RMSE = regularized_function_test_set(lambda)$RMSE, 
                                       MSE = regularized_function_test_set(lambda)$MSE,
                                       MAE = regularized_function_test_set(lambda)$MAE))

evaluation_results




#### matrix factorization using recosystem package

#### data memory specifies a data set from r objects
y_train <- with(train_set, data_memory(user_index = userId, item_index = movieId, rating = rating))
y_test <- with(test_set,  data_memory(user_index = userId, item_index = movieId, rating = rating))

### create a recosystem object

recomm_recosystem <- Reco()

### tuning the cross validation parameters (optinal part)

opts <- recomm_recosystem$tune(y_train, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                                    costp_l1 = 0, costq_l1 = 0,
                                                    nthread = 1, niter = 10))

### training the algorithm

recomm_recosystem$train(y_train, opts = c(opts$min, nthread = 1, niter = 20))

### prediction the over the test set
predict_recosystem <- recomm_recosystem$predict(y_test, out_memory())


### the first 20 predicted 
head(predict_recosystem, 20)

### Evaluation metrics
RMSE(predict_recosystem, test_set$rating)
MSE(predict_recosystem, test_set$rating)
MAE(predict_recosystem, test_set$rating)


### creating the validation set

y_validation <- with(validation, data_memory(user_index = userId, item_index = movieId, rating = rating))

validation_predict <- recomm_recosystem$predict(y_validation, out_memory())

validation_rmse <- RMSE(validation_predict, validation$rating)

