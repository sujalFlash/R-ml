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
model=lm(training_data)
summary(model)
