# ################ #
# Loading packages #
# ################ #

# First, we make sure that our working environment and memory are nice and clean 
# (i.e., empty).

remove(list = ls(all = T)); gc(T,T,T)

# Generating a blank function to load all desired packages
load_packages <- function(){

# Object of desired packages

desired.packages <- c(# Tidy coding paradigm
                      "tidyverse", "magrittr", 
                      # Data import
                      "readr",
                      # Data frames 
                      "tibble", "data.table",
                      # Data wrangling
                      "lubridate", 
                      # Graphics
                      "ggExtra", "ggrepel", "scales", 
                      # Machine Learning
                      "caret",
                      # Reporting
                      "knitr", "kableExtra") 

# Object which contains the desired packages as long as they are not already 
# installed. 

inst.packages <- desired.packages[!desired.packages %in% installed.packages()]

# Generating a for-loop

for (packages in inst.packages) {install.packages(packages, dependencies = T)}

sapply(desired.packages, require, character = T)}

# Finally, using the just generated function to load and/or install the desired
# packages.
load_packages() 

# ############### #
# Retrieving Data #
# ############### #

# Downloading and structuring the data

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", 
                             readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), 
                          "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")

# If using R 4.0 or later
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
# Setting the seed to ensure reproducibility

set.seed(1, sample.kind = "Rounding")

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, 
                                  list = F)

edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Making sure that userId and movieId in the validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Adding rows that are removed from the validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# ############## #
# Data Wrangling #
# ############## #

# Extracting the unix-timestamps according to the README information of the 
# MovieLens dataset. 

edx_star <- edx %>% 
  mutate(# Converting the given timestamps into dates with time 
    # information.
    rating_timestamp = as_datetime(timestamp,
                                   origin = "1970-01-01",
                                   tz = "GMT"),
    # Assigning the dates of the ratings. 
    rate_date = as_date(rating_timestamp),
    # Assigning the last 6 characters/strings from the titles
    # (i.e., the parentheses and the year numbers) to a new 
    # column.
    year =  str_sub(title, -6),
    # Removing the parentheses of the year information.
    release_year = gsub("[[:punct:]]", "", year),
    # Converting the release_year information into integers 
    # (since year information without month and day information 
    # cannot be considered as date).
    release_year = as.integer(release_year),
    # Removing the last 6 characters/strings from the
    # titles.
    title = str_sub(title, 1, nchar(title) - 6)) %>% 
  # Deselecting not needed/redundant columns. As there are multiple 
  # "select"-functions, we have to make sure to use the here the one 
  # from the dyplr-package which also belongs to the tidyverse.       
  dplyr::select(-c(rating_timestamp, timestamp, year))

# As the genre labels are collapsed in one row for each film, we should separate 
# them so that each genre label of a film gets its own row. Since this a time 
# consuming process, we separate this process form the code chunk above, to 
# accelerate this entire wrangling process. 

edx_star %<>% 
  separate_rows(genres, sep = "\\|")

# Exporting the retrieved, cleaned, and wrangled dataset as .csv-file in order
# to load the cleaned and wrangled dataset for faster access when rerunning the 
# analyses.

write_csv(edx_star, "edx_star.csv")

# As we later want to predict the ratings of the validation set based on the
# edx_star-ratings, we need, of course, apply this very cleaning and wrangling
# procedure to the validation set. Hence, the comments from above are
# here omitted as the process is exactly the same, except from the validation
# dataset).

valid_star <- validation  %>% 
  mutate(rating_timestamp = as_datetime(timestamp,
                                        origin = "1970-01-01",
                                        tz = "GMT"),
         
         rate_date = as_date(rating_timestamp),
         
         year =  str_sub(title, -6),
         
         release_year = gsub("[[:punct:]]", "", year),
         
         release_year = as.integer(release_year),
         
         title = str_sub(title, 1, nchar(title) - 6)) %>% 
  
  dplyr::select(-c(rating_timestamp, timestamp, year))

valid_star %<>% 
  separate_rows(genres, sep = "\\|")

write_csv(valid_star, "valid_star.csv")


# Importing the retrieved and cleand and wrangled datasets when rerunning the
# code for faster accessibility

edx_star <- read_csv("~/PLEASE/INSERT/YOUR/PATH/edx_star.csv")
valid_star <- read_csv("~/PLEASE/INSERT/YOUR/PATH/valid_star.csv")


# ################ #
# Plotting RATINGS #
# ################ #

#Mean and SD

mean(edx_star$rating); sd(edx_star$rating)

# Plotting the frequency distribution of movie ratings

ggplot(data = edx_star) +
  
  geom_histogram(aes(rating), bins = 10, fill = "navyblue", color = "#280000", 
                 alpha = 0.3) +
  
  scale_y_continuous(labels = label_number_si(accurarcy = NULL)) +
  
  labs(x = "\nMovie Ratings\n(Stars)", y = "Rating Counts\n") +
  
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.margin = unit(c(.66,.33,.66,.33), "cm")) 

ggsave("ratings.png", path = "~/PLEASE/INSERT/YOUR/PATH/Plots", 
       width = 21.33, height = 9, dpi = 320)

# ######################################################################### #
# EDA_H1: If the rating counts for movies are high, the average ratings for #
# movies are high as well. ################################################ #
# ######################################################################### #

# Visual exploration of H1

movie_avg_rating <- edx_star %>% 
  group_by(movieId) %>% 
  summarize(n = n(),
            movie_rating = mean(rating)) 

p_m <- ggplot(movie_avg_rating,aes(n, movie_rating)) + 
  
  geom_point(color = "navyblue", size = 0.3, alpha = 0.3) +
  
  geom_smooth(color = "darkred", size = 0.75) +
  
  scale_x_continuous(labels = label_number_si(accurarcy = NULL),
                     trans = "log10") +
  
  labs(x = "\nRating Counts per Movie (Log-scaled with base 10)",
       y = "Average Movie Ratings per Movie\n(Stars)\n") +
  
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.margin = unit(c(.66, .33, .66, .33), "cm"))

ggMarginal(p_m, type = "densigram", bins = 100, fill = "navyblue", 
           color = "#280000", alpha = 0.3)

ggsave("H1_movie_effects.png", path = "~/PLEASE/INSERT/YOUR/PATH/Plots", 
       width = 21.33, height = 9, dpi = 320)

# ########################################################################## #
# EDA_H2: If users rate many movies, the users tend to give on average lower #
# ratings. ################################################################# #
# ########################################################################## #

user_avg_rating <- edx_star %>% 
  group_by(userId) %>% 
  summarize(n = n(),
            movie_rating = mean(rating))

p_u <- ggplot(user_avg_rating, aes(n, movie_rating)) + 
  
  geom_point(color = "navyblue", size = 0.1, alpha = 0.3) + 
  
  geom_smooth(color = "darkred", size = 0.5) +
  
  scale_x_continuous(labels = label_number_si(accurarcy = NULL), 
                     trans = "log10") +
  
  labs(x = "\nRating Counts (Log-scaled with base 10)", 
       y = "Average Movie Ratings per User\n(Stars)\n") +
  
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.margin = unit(c(.66, .33, .66, .33), "cm"))

ggMarginal(p_u, type = "densigram", bins =  100, fill = "navyblue", 
           color = "#280000", alpha = 0.3)

ggsave("H2_user_effects.png", path = "~/PLEASE/INSERT/YOUR/PATH/Plots", 
       width = 21.33, height = 9, dpi = 320)

# ############################################################################ #
# EDA_H3: If movies have been released recently, the given average ratings for #
# these movies tend to be lower than for older movies.. ###################### #
# ############################################################################ #

# H3 release_year
release_year_avg_rating <- edx_star %>% 
  group_by(release_year) %>% 
  summarize(movie_rating = mean(rating))

p_y <- ggplot(release_year_avg_rating, aes(release_year, movie_rating)) +  
  
  geom_point(color = "navyblue", alpha = 0.3 ) + 
  geom_smooth(color = "darkred", size = 0.3, alpha = 0.2) +
  
  labs(x = "\nRelease Year",
       y = "Average Movie Ratings per Movie Release Year\n(Stars)\n") +
  
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.margin = unit(c(.66, .33, .66, .33), "cm"))

ggsave("H3_year_effects.png", path = "~/PLEASE/INSERT/YOUR/PATH/Plots", 
       width = 21.33, height = 9, dpi = 320)

# ############################################################################ #
# EDA_H4: If movies have been rated recently, the given average ratings for ## # 
# these movies tend to be lower than for movies that have been rated anciently.#
# ############################################################################ #


rate_date_avg_rating <- edx_star %>% 
  group_by(rate_date) %>% 
  summarize(date_rating = mean(rating))

ggplot(data = rate_date_avg_rating ) + 
  geom_point(aes(rate_date, date_rating), color = "navyblue", size = 0.66, 
             alpha = 0.3) +
  
  geom_smooth(aes(rate_date, date_rating), color = "darkred", size = 0.3, 
              alpha = 0.2) +
  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  
  labs(x = "\nRating Date", y = "Average Movie Ratings per Date\n(Stars)\n") +
  
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.margin = unit(c(.66, .33, .66, .33), "cm"))

ggsave("H4_date_effects.png", path = "~/PLEASE/INSERT/YOUR/PATH/Plots", 
       width = 21.33, height = 9, dpi = 320)

# ####################################################################### #
# EDA_H5: If movies are characterized by genre labels, which are rated ## #
# frequently, their average ratings tend to be lower than for movies with #
# genre labels that are seldom rated.#################################### #
# ######################################################################ä #

genre_avg_rating <- edx_star %>% 
  group_by(genres) %>% 
  summarize(counts = n(),
            movie_rating = mean(rating))

p_g <- ggplot(data = genre_avg_rating, aes(counts, movie_rating)) +
  
  geom_point(color = "navyblue", alpha = 0.3) +
  
  geom_smooth(color = "darkred", size = 0.3, alpha = 0.2) +
  
  scale_x_continuous(labels = label_number_si(accurarcy = NULL), 
                     trans = "sqrt") +
  
  geom_label_repel(aes(label = genres), xlim = c(7, NA), ylim = c(0.5, 5), 
                   point.padding = 0.1, min.segment.length = 0.1, 
                   box.padding = 0.1, direction = "y", seed = 1) +
  
  labs(x = "\nRating Counts (Square root scaling)",
       y = "Average Movie Ratings by Genre Rating Counts\n(Stars)\n") +
  
  theme_bw(base_size = 14) +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.margin = unit(c(.66, .33, .66, .33), "cm")) 

ggsave("H5_genre_effects.png", path = "~/PLEASE/INSERT/YOUR/PATH/Plots",
       width = 21.33, height = 9, dpi = 320)


# ########################################### #
# Analysis I: Creating Training and Test sets #
# ########################################### #

# Setting the seed to ensure reproducibilty

set.seed(1, sample.kind = "Rounding")

#  Creating the index data partition to split the actual training data into a 
#  sub-training set (= CV_train_set. 90% of the actual training data) and, 
#  respectively, a sub-test set (= CV_test_set, 10% of the actual training data) 
#  for experimental/cross-validation purposes.

CV_test_index <- createDataPartition(y = edx_star$rating, times = 1, p = .1, 
                                     list = F)

# The sub-training set (90% of the actual training data).
CV_train_set <- edx_star[-CV_test_index,]

# The sub-test set (10% of the actual training data).
CV_test_set <- edx_star[CV_test_index,]

# Ensuring that in both sets the same movies and users are included.
CV_test_set <- CV_test_set %>% 
  semi_join(CV_train_set, by = "userId") %>%
  semi_join(CV_train_set, by = "movieId")

# Loss function as the objective or, respectively, key performance indicator for
# the models at stake.

RMSE <- function(actual_ratings, predicted_ratings){
  sqrt(mean((actual_ratings - predicted_ratings)^2))}

# ####################################################################### #
# Analysis I: LS-Predictions (experimentation) with the CV_train_set and# #
# CV_test_set############################################################ #
# ####################################################################### #

# Constant a.k.a y-intercept
mu <- mean(CV_train_set$rating) 

# Naïve Rating Prediction 

RMSE_0 <- RMSE(CV_test_set$rating, mu)

# Calculating the bias effect of movies (i.e, their specific given information)
# on the given ratings.

movie_avgs <- CV_train_set %>% 
  group_by(movieId) %>% 
  summarize(b_m = mean(rating - mu))

pred_ratings_1 <- CV_test_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  mutate(pred =  mu + b_m, 
         # constraining the prediction to the range between 0.5
         # and 5 star units.
         CV_test_pred = ifelse(pred < 0.5, 0.5, 
                               ifelse(pred > 5, 5, pred))) %>% 
  .$pred

RMSE_1 <- RMSE(CV_test_set$rating, pred_ratings_1)

# Calculating the bias effect of users (i.e., their specific attitudes on the 
# rated films) on the given ratings.

user_avgs <- CV_train_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - (mu + b_m)))


pred_ratings_2 <- CV_test_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  mutate(pred =  mu + b_m + b_u,
         # constraining the prediction to the range between 0.5
         # and 5 star units.
         CV_test_pred = ifelse(pred < 0.5, 0.5, 
                               ifelse(pred > 5, 5, pred))) %>%
  .$CV_test_pred


RMSE_2 <- RMSE(CV_test_set$rating, pred_ratings_2)

# Calculating the bias effect of the release years (i.e., their specific effects 
# on the rated films) on the given rating.

release_year_avgs <- CV_train_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(release_year) %>%
  summarize(b_y = mean(rating - (mu + b_m + b_u)))

pred_ratings_3 <- CV_test_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(release_year_avgs, by = "release_year") %>%
  mutate(pred = mu + b_m + b_u + b_y, 
         # constraining the prediction to the range between 0.5
         # and 5 star units.
         CV_test_pred = ifelse(pred < 0.5, 0.5, 
                               ifelse(pred > 5, 5, pred))) %>%
  .$CV_test_pred

RMSE_3 <- RMSE(CV_test_set$rating, pred_ratings_3)

# Calculating the bias effect of the rating_dates (i.e., their specific effects 
# on the rated films) on the given rating.

rate_date_avgs <- CV_train_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(release_year_avgs, by = "release_year") %>%
  group_by(rate_date) %>%
  summarize(b_d = mean(rating - (mu + b_m + b_u + b_y)))

pred_ratings_4 <- CV_test_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(release_year_avgs, by = "release_year") %>%
  left_join(rate_date_avgs, by = "rate_date") %>% 
  mutate(pred =  mu + b_m + b_u + b_y + b_d,
         # constraining the prediction to the range between 0.5
         # and 5 star units.
         CV_test_pred = ifelse(pred < 0.5, 0.5, 
                               ifelse(pred > 5, 5, pred))) %>%
  .$CV_test_pred

RMSE_4 <- RMSE(CV_test_set$rating, pred_ratings_4)

# Calculating the bias effect of the genres (i.e., their specific framing
# on the rated films) on the given rating.

genres_avgs <- CV_train_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(release_year_avgs, by = "release_year") %>%
  left_join(rate_date_avgs, by = "rate_date") %>% 
  group_by(genres) %>%
  summarize(b_g = mean(rating - (mu + b_m  + b_u + b_y + b_d)))

# Finally, predicting the ratings of the sub-test set of the actual training 
# data with each lambda based on the regularized input variables. 

pred_ratings_5 <- CV_test_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(release_year_avgs, by = "release_year") %>%
  left_join(rate_date_avgs, by = "rate_date") %>% 
  left_join(genres_avgs, by = "genres") %>%
  mutate(pred = mu + b_m + b_u + b_y + b_d + b_g, 
         # constraining the prediction to the range between 0.5
         # and 5 star units.
         CV_test_pred = ifelse(pred < 0.5, 0.5, 
                               ifelse(pred > 5, 5, pred))) %>% 
  .$CV_test_pred

RMSE_5 <- RMSE(CV_test_set$rating, pred_ratings_5)

# ############################# #
# Analysis II: Cross-Validation #
# ############################# #

lambdas <- seq(0, 10, .2)

# Cross-validation of those lambdas by applying them each time on the
# respective predictor/input variable

rmse_validation <- sapply(lambdas, function(l){
  
# Benchmark 
mu <- mean(CV_train_set$rating) 

# Calculating the bias effect of movies (i.e, their specific given information)
# on the given rating by penalizing 

movie_avgs <- CV_train_set %>% 
  group_by(movieId) %>% 
  summarize(b_m = sum(rating - mu) / (n() + l))

# Calculating the bias effect of users (i.e., their specific attitudes on the 
# rated films) on the given rating by 

user_avgs <- CV_train_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - (mu + b_m)) / (n() + l))

# Calculating the bias effect of the release years (i.e., their specific effects 
# on the rated films) on the given rating.

release_year_avgs <- CV_train_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(release_year) %>%
  summarize(b_y = sum(rating - (mu + b_m + b_u)) / (n() + l))

# Calculating the bias effect of the rating_dates (i.e., their specific effects 
# on the rated films) on the given rating.

rate_date_avgs <- CV_train_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(release_year_avgs, by = "release_year") %>%
  group_by(rate_date) %>%
  summarize(b_d = sum(rating - (mu + b_m + b_u + b_y)) / 
              (n() + l))

# Calculating the bias effect of the genres (i.e., their specific framing
# on the rated films) on the given rating.

genres_avgs <- CV_train_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(release_year_avgs, by = "release_year") %>%
  left_join(rate_date_avgs, by = "rate_date") %>% 
  group_by(genres) %>%
  summarize(b_g = sum(rating - (mu + b_m + b_u + b_y + b_d)) /
              (n() + l))

# Finally, predicting the ratings of the sub-test set of the actual training 
# data with each lambda based on the regularized input variables. 

pred_ratings_CV <- CV_test_set %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(release_year_avgs, by = "release_year") %>%
  left_join(rate_date_avgs, by = "rate_date") %>% 
  left_join(genres_avgs, by = "genres") %>%
  mutate(pred = mu + b_m + b_u + b_y + b_d + b_g,
         # constraining the prediction to the range between 0.5
         # and 5 star units.
         CV_pred = ifelse(pred < 0.5, 0.5, 
                          ifelse(pred > 5, 5, pred))) %>%
  .$CV_pred 

# Getting the respective loss function values for each lambda. That is, we will 
# get 51 RMSE values.

return(RMSE(CV_test_set$rating, pred_ratings_CV))})

# ######################################## #
# Analysis II: Cross-Validation | Plotting #
# ######################################## #

# Retrieving the best value according to the loss function.

ggplot() +
  
# Scatter plot

geom_point(aes(lambdas, rmse_validation), color = "navyblue", size = 2, 
           alpha = 0.66) + 

# Indicating the optimal lambda with a vertical line

annotate(geom = "point", x = lambdas[which.min(rmse_validation)], y = 0.8553969, 
         size = 3,  shape = 21, fill = "darkgreen") +

geom_label(aes(x = lambdas[which.min(rmse_validation)], y = 0.8554069, 
               label = "Best λ\n(RMSE = 0.8553969)"), 
           color = "darkgreen", size = 3) +

# Customizing the breaks on the x-axis

scale_x_continuous(breaks = c(0, 2, lambdas[which.min(rmse_validation)], 4, 6, 
                              8, 10),
                   labels = c("0", "2", "3.6","4", "6", "8", "10" )) +

# Customizing the labels on both axes

labs(x = "\nλ", y = "RMSE\n") +

# Layout

theme_bw(base_size = 14) +
theme(axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      plot.margin = unit(c(.66,.33,.66,.33), "cm"))

# Saving the output 

ggsave("lambdas.png", path = "~/PLEASE/INSERT/YOUR/PATH/Plots",
       width = 21.33, height = 9, dpi = 320)

# ############################# #
# Analysis II: FINAL PREDICTION #
# ############################# #

# Using the lambda value which optimally minimizes the value of the loss 
# (see above).

l <- 3.6

## Running the Ridge Regression with the optimal lambda. Basically, the same 
## lines of code as in the cross-validation, however, with the actual training
## and validation datasets and an optimal lambda-value.

# Constant a.k.a y-intercept

mu <- mean(edx_star$rating) 

# Calculating the bias effect of movies (i.e, their specific given information)
# on the given rating 

movie_avgs <- edx_star %>% 
  group_by(movieId) %>% 
  summarize(b_m = sum(rating - mu) / (n() + l))

# Calculating the bias effect of users (i.e., their specific attitudes on the 
# rated films) on the given rating.

user_avgs <- edx_star %>% 
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - (mu + b_m)) / (n() + l))

# Calculating the bias effect of the release years (i.e., their specific effects 
# on the rated films) on the given rating.

release_year_avgs <- edx_star %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  group_by(release_year) %>%
  summarize(b_y = sum(rating - (mu + b_m + b_u)) / (n() + l))

# Calculating the bias effect of the rating_dates (i.e., their specific effects 
# on the rated films) on the given rating.

rate_date_avgs <- edx_star %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(release_year_avgs, by = "release_year") %>%
  group_by(rate_date) %>%
  summarize(b_d = sum(rating - (mu + b_m + b_u + b_y)) / 
              (n() + l))

# Calculating the bias effect of the genres (i.e., their specific framing
# on the rated films) on the given rating.

genres_avgs <- edx_star %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(release_year_avgs, by = "release_year") %>%
  left_join(rate_date_avgs, by = "rate_date") %>% 
  group_by(genres) %>%
  summarize(b_g = sum(rating - (mu + b_m  + b_u + b_y + b_d)) /
              (n() + l))

# Finally, predicting the ratings of the sub-test set of the actual training 
# dataset with each lambda based on the regularized input variables. 

pred_ratings <- valid_star %>% 
  left_join(movie_avgs, by = "movieId") %>%
  left_join(user_avgs, by = "userId") %>%
  left_join(release_year_avgs, by = "release_year") %>%
  left_join(rate_date_avgs, by = "rate_date") %>% 
  left_join(genres_avgs, by = "genres") %>%
  mutate(pred = mu + b_m + b_u + b_y + b_d + b_g,
         # constraining the prediction to the range between 0.5
         # and 5 star units.
         final_pred = ifelse(pred < 0.5, 0.5, 
                             ifelse(pred > 5, 5, pred))) %>%
  .$final_pred  

# The moment of truth: Result of the final prediction

RMSE_FINAL <- RMSE(valid_star$rating, pred_ratings)


# ### #
# END #
# ### #

sessioninfo::session_info()