library(party)
library(randomForest)

#import data
german <- read.csv("G:/download chrome/german_credit.csv", header = TRUE)
head(german)

#jadikan variabel kategorik sebagai faktor
german$Telephone <- as.factor(german$Telephone)
german$ForeignWorker <- as.factor(german$ForeignWorker)
german$Class <- as.factor(german$Class)

#Partisi data menjadi data training dan testing
library(caret)
#data train sebanyak 75%
inTrain <- createDataPartition(y=german$Class, p=0.75, 
                               list=FALSE)
train <- german[inTrain,]
test <- german[-inTrain,]

# buat model random forest.

output.forest <- randomForest(Class ~ ., data = german)
output.forest
importance(output.forest)

varImpPlot(output.forest)

#validasi model dengan data test
Prediksi <- predict(output.forest, test)

#confusion matrix
CM <- table(test$Class, Prediksi)
CM

#akurasi
accuracy <- (sum(diag(CM)))/sum(CM)
accuracy

#load library caret
library(caret)
# definisikan training control
train_control <- trainControl(method="cv", number=10)

# train the model
model <- train(Class~., data=german, trControl=train_control,
               method="rf")
# summarize results
print(model)

#LEAVE-ONE-OUT
# definisikan training control
train_control <- trainControl(method="LOOCV")
# atur parameter mtry pada random forrest
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
# train the model
model <- train(Class~., data=german, trControl=train_control,
               method="nb", tuneGrid=grid)
# summarize results
print(model)

#BOOTSTRAP
# definisikan training control
train_control <- trainControl(method="boot", number = 100)
# atur parameter mtry pada random forrest
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
# train the model
model <- train(Class~., data=german, trControl=train_control,
               method="nb", tuneGrid=grid)
# summarize results
print(model)

#PLOT ROC
#data train sebanyak 75%
inTrain <- createDataPartition(y=german$Class, p=0.7, 
                               list=FALSE)
train <- german[inTrain,]
test <- german[-inTrain,]

# buat model random forest.
output.forest <- randomForest(Class ~ ., data = german)

#prediksi data testing
prediksi <- as.data.frame(predict(output.forest, test,type = "prob"))


#PLOT ROC
#data train sebanyak 75%
inTrain <- createDataPartition(y=german$Class, p=0.75, 
                               list=FALSE)
train <- german[inTrain,]
test <- german[-inTrain,]

# buat model random forest.
output.forest <- train(Class ~ ., data = german,method="rf")

#prediksi data testing
prediksi <- predict(output.forest, test,type = "prob")

#Plot ROC
library(pROC)
german.roc <- roc(test$Class,prediksi$Bad)
plot.roc(german.roc,print.auc = TRUE)
#to get threshold and accuracy
result.coords <- coords(german.roc, "best",
                        best.method="closest.topleft",
                        ret=c("threshold", "accuracy"))
print(result.coords)
#Plot ROC2
library(caret)
library(pROC)
data(iris)

# kelas setosa tidak dilibatkan dalam contoh ini
# karena ROC hanya untuk kelas dengan 2 level
iris <- iris[iris$Species == "virginica"|iris$Species == "versicolor", ]
iris$Species <- factor(iris$Species)  

#split data train dan data test
samples <- sample(NROW(iris), NROW(iris) * .5)
data.train <- iris[samples, ]
data.test <- iris[-samples, ]

#buat model dengan random forrest
forest.model <- train(Species ~., data.train, method="rf")

#prediksi data test
result.predicted.prob <- predict(forest.model, data.test, type="prob") # Prediction

result.roc <- roc(data.test$Species, result.predicted.prob$versicolor) # Draw ROC curve.
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")
#tambahkan smoothed ROC
plot.roc(smooth(result.roc), add=TRUE, col="blue",
         print.auc=TRUE)
#to get threshold and accuracy
result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)
