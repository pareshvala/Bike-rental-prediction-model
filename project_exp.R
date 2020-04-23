
library(randomForest) 


# Import data
data <- read.csv("day.csv", header = TRUE, stringsAsFactors = FALSE)
head(data)

# transfrom into valid format
data$dteday <- as.Date(data$dteday,tryFormats = c("%Y-%m-%d", "%Y/%m/%d"), options=FALSE)
class(data$dteday)

data$weathersit <- as.factor(data$weathersit)
data$workingday <- as.logical(data$workingday)
data$holiday <- as.logical(data$holiday)
summary(data)

# Creating training and testing dataset
dat <- data[,-c(1,2, 14, 15)]
head(dat)
set.seed(9876)
data_set_size <- floor(nrow(dat)*0.80)
index <- sample(1:nrow(dat), size=data_set_size)
train_sample <- sample(1000, 900)
training <- dat[index,]
testing <- dat[-index,]
head(training)
testing_train <- data[-index,]
head(training)

# building randomForest model
rf=randomForest(cnt~., data=training, ntree=400)
rf
plot(rf)

varImpPlot(rf)


# Fitting the best model suitable for this dataset
oob.err=double(13)
test.err=double(13)
for(mtry in 1:13) 
{
  rf=randomForest(cnt ~ . , data = training ,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,testing) #Predictions on Test Set for each Tree
  test.err[mtry]= with(testing, mean( (cnt - pred)^2)) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}
# oob.err
# test.err

# evaluating difference between out of bag error and testing error
matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))


# building best model with mtry= 4 
rf_n=randomForest(cnt~., data=training, ntree=400, mtry=5)
rf_n
plot(rf_n)
pred<-predict(rf_n,testing)
dev.off()
plot(data$instant, data$cnt, type = "l", col = "red", xlab = "Day", ylab = "Number of Bike Users", main = "Random Forest Plot for Bike Users")
legend("topleft", c("Actual", "Predicted"), lty = c(1, 1), col = c("red", "blue"))
lines(testing_train$instant, pred, type = "l", col = "blue")

# Mean absolute error
mean(abs(testing$cnt-pred))
