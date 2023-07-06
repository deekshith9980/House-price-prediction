install.packages("tidyverse")
library("tidyverse")
install.packages("GGally")
library("GGally")
housing = read.csv("housing.csv", stringsAsFactors = F)
housing
glimpse(housing)
anyNA(housing)
colSums(is.na(housing))
housing = housing %>% 
  fill(total_bedrooms,.direction = "updown") %>% 
  mutate(ocean_proximity = as.factor(ocean_proximity)) 
anyNA(housing)
RNGkind(sample.kind = "Rounding")
set.seed(417)
idx <- sample(nrow(housing), nrow(housing)* 0.8)
housing_train <- housing[idx,]
housing_test <- housing[ -idx,]
housing_train
ggcorr(housing_train, label = T, hjust = 1, layout.exp = 3)
boxplot(housing_train$median_house_value)
model_all = lm(median_house_value ~ housing_median_age + total_rooms+total_bedrooms +households + median_income +ocean_proximity, data = housing_train)
summary(model_all)
housing_test$pred = predict(model_all, housing_test)
hist(housing_test$pred)
plot(model_all)
install.packages("MLmetrics")
library(MLmetrics)
RMSE(y_pred = housing_test$pred, y_true = housing_test$median_house_value)
hist(model_all$residuals)
shapiro.test(x = model_all$residuals[3:5000])
install.packages("car")
library(car)
vif(model_all)
