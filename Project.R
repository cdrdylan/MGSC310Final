library("dplyr")
library("readr")
library('tidyr')


interactions <- read_csv("datasets/sentimentdataset.csv")

clean_interactions <- interactions %>% select(-Timestamp, -Sentiment, -Text, -User, -Hashtags)

clean_interactions <- clean_interactions %>% mutate(country_factor = as.factor(Country),  
                                              month_factor = as.factor(Month), 
                                              hour_factor = as.factor(Hour), day_factor = as.factor(Day)) %>% drop_na()


interactions_split <- initial_split(clean_interactions, prop = 0.75)
interactions_train <- training(interactions_split)
interactions_test <- testing(interactions_split)

mod <- lm(Likes ~ Platform + Country + month_factor + day_factor + hour_factor, 
          data = interactions_train)

summary(mod)

country_coefficients <- coef(mod)[grep("^Country", names(coef(mod)))]

country_data <- data.frame(country = gsub("^Country", "", names(country_coefficients)),
                           coefficient_value = country_coefficients, stringsAsFactors = FALSE)

combined_country_plot <- ggplot(country_data, aes(x = coefficient_value, y = country, label = country)) +
  geom_point(color = "blue") +
  geom_text(nudge_x = 0.1, hjust = 0, size = 5) +
  labs(title = "Country's Magnitude on Likes",
       x = "Effect on Likes",
       y = "Country") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5))

print(combined_country_plot)

mod2 <- lm(Retweets ~ Platform + Country + month_factor + day_factor + hour_factor, 
          data = interactions_train)

summary(mod2)

country_coefficients <- coef(mod2)[grep("^Country", names(coef(mod2)))]

country_data <- data.frame(country = gsub("^Country", "", names(country_coefficients)),
                           coefficient_value = country_coefficients, stringsAsFactors = FALSE)

combined_country_plot <- ggplot(country_data, aes(x = coefficient_value, y = country, label = country)) +
  geom_point(color = "RED") +
  geom_text(nudge_x = 0.1, hjust = 0, size = 5) +
  labs(title = "Country's Magnitude on Retweets",
       x = "Effect on Retweets",
       y = "Country") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5))

interactions_train$Country <- factor(interactions_train$Country)
interactions_test$Country <- factor(interactions_test$Country)


levels(interactions_train$Country)
levels(interactions_test$Country)

# calculate root mean squared error (RMSE)
get_rmse <- function(true, predictions){
  sqrt(mean((true - predictions)^2))
}

preds_train <- predict(mod, newdata = interactions_train)

preds_test <- predict(mod, newdata = interactions_test)

# calcualte RMSE in the testing and training sets
get_rmse(interactions_train$Retweets, preds_train)
get_rmse(interactions_test$Retweets, preds_test)

print(combined_country_plot)

results_train <- 
  tibble(
    `preds` = interactions_train$Likes,
    `true` = interactions_train$Likes,
    `type` = "train"
  )

results_test <- 
  tibble(
    `preds` = interactions_test$Likes,
    `true` = interactions_test$Likes,
    `type` = "test"
  )

results_df <- 
  bind_rows(results_train, results_test)

ggplot(results_df, aes(x = true, y = preds)) +
  geom_point(aes(color = type)) + 
  geom_abline(color = "red") +
  facet_wrap(~ type) +
  xlim(10,40) + ylim(10,40) +
  theme_minimal(base_size = 16) + 
  theme(legend.position="bottom")


#Insta + Sweden + June + 13th + from 11 - 11:59 PM You could expect a post to reach around 106 likes
30.4464 + 3.9836 + 32.7767 + 10.1978 + 17.3913 + 11.0877 = 105.8835

#Insta + Sweden + July + 13th + form 5 - 5:59 AM you could expect 42 retweets
15.5065 + 1.6594 +16.4145 + 6.4297 + 1.6453 = 41.6554
