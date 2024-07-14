library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
set.seed(234)
data=read.csv("apollo.csv")
head(data)
#removing unnecessary col like date
data<-subset(data,select=-c(date))
head(data)
nrow(data)
x<-is.na(data)
print(x)
train_indices<-sample(1:nrow(data),0.7*nrow(data))
length(train_indices)
training_data<-data[train_indices,]
test_data<-data[-train_indices,]
nrow(training_data)
nrow(test_data)
model=lm(close ~ open + low + high +volume,training_data)
summary(model)
y<-predict(model,test_data)
print(y)
cor(data)
#after executing above line it is showing auto correlation among independent variables which leads to problem called multicollinearity
# error terms must be normally distributed so let's check of normality
error<-residuals(model)
shapiro.test(error)
#above test shows  that errors are not normally distributed

ggplot(training_data, aes(x = open, y = close)) +

  geom_point(aes(color = low), alpha = 0.5) +  
  geom_line(aes(y = high), linetype = "dashed") +  
  geom_bar(aes(x = open, fill = volume), stat = "identity") 
  

  labs(title = "Relationship between Closing Price and Other Factors",
       x = "Opening Price",
       y = "Closing Price",
       color = "Low Price",
       linetype = "High Price",
       fill = "Volume") +
    theme_bw() +
  theme(legend.position = "bottom")
  
  predicted_results<-predict(model,test_data)
  ggplot(predicted_vs_actual_data, aes(x = test_data$loss, y = predicted_results)) +
    geom_point() +  
    plot() +      
    abline(0, 1, col = "purple") +
    labs(title = "Observed Loss vs. Predicted Loss",
         x = "Observed Loss",
         y = "Predicted Loss") +
    theme_bw()
  
  head(predicted_results)
  predicted_train_values<-predict(model,training_data)
  rmse<-sqrt(mean(predicted_results-test_data$close)^2)
  
 cat("the rmse is ",rmse)
 summary(model)$r.squared
 #rmse is low indicated data fits very well to multiple linear regression model
 #r squared value is very high close to 1 it also indicates data fit well to linear regression model
 
 rmse_train<-sqrt(mean(predicted_train_values-training_data$close)^2)
 cat("The rmse of train is ",rmse_train)
 #rmse of train predicted values show signs of overfititng
 
 