# coercing edx data into a realRatingMatrix
library(recommenderlab)



##### The following code is uploaded only for further knowledge and future analysis. The code runs very slow
##### on a commodity laptop (over 12h on mine).



y_edx <- edx %>% 
  select(userId, title, rating)

y_edx <- as(edx, "realRatingMatrix")

#### filtering the data for users who have rated at least 50 movies and movies that have been watched at least 100 times

y_edx_filtered <- y_edx[rowCounts(y_edx) > 50, colCounts(y_edx) > 100] 


### normalizing

### creating train and test set
test_set_index <- sample(x = c(TRUE, FALSE), size = nrow(y_edx_filtered), replace = TRUE, prob = c(0.1, 0.9))

#create train and test sets
y_train_set <- y_edx_filtered[-test_set_index,]
y_test_set <- y_edx_filtered[test_set_index,]


### IBCF recommender

recom_ibcf <- Recommender(y_train_set, method = "IBCF")

n_recommend <- 6
predict_ibcf <- recommenderlab::predict(recom_ibcf, newdata = y_test_set, type = "ratingMatrix")

calcPredictionAccuracy(predict_ibcf, y_test_set)


### UBCF recommender

recom_ubcf <- Recommender(y_train_set, method = "UBCF")

n_recommend <- 6
predict_ubcf <- recommenderlab::predict(recom_ubcf, newdata = y_test_set, type = "ratingMatrix")

calcPredictionAccuracy(predict_ubcf, y_test_set)

#### svd recommender

recommend_svd <- Recommender(y_train_set, method = "SVD")

predict_svd <- recommenderlab::predict(recommend_svd, newdata =  y_test_set, type = "ratingMatrix")

calcPredictionAccuracy(predict_svd, y_test_set)


