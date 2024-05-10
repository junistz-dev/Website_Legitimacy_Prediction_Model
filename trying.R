install.packages("ipred")
library(tree)
library(e1071)
library(ROCR)
library(rpart)
library(rgl)
library(plyr)
library(adabag)
library(randomForest)

rm(list = ls())
Phish <- read.csv("PhishingData.csv")
set.seed(319946955) # Your Student ID is the random seed
L <- as.data.frame(c(1:50))
L <- L[sample(nrow(L), 10, replace = FALSE),]
Phish <- Phish[(Phish$A01 %in% L),]
PD <- Phish[sample(nrow(Phish), 2000, replace = FALSE),] # sample of 2000 rows

# Question 1
dim(PD)
table(PD$Class)
# we can get 0 is 1438 and 1 is 562

count_0 <- sum(PD$Class == 0)
count_1 <- sum(PD$Class == 1)

# checking the ratio
ratio_0 <- count_0 / length(PD$Class)
ratio_1 <- count_1 / length(PD$Class)
cat("Ratio of Class 0:", ratio_0, "\n")
cat("Ratio of Class 1:", ratio_1, "\n")

proportion <- count_1 / count_0
proportion

# making bar plot
barplot(c(ratio_0, ratio_1), names.arg = c("Class 0", "Class 1"), ylim = c(0,1),
        main = "Ratio of Class 0 and Class 1", ylab = "Ratio", col = c("skyblue", "salmon"))


PD_filtered <- PD[complete.cases(PD), ]

dim(PD_filtered)

# Question 2, pre-proessing
PD$Class <- as.factor(PD$Class)
PD_filtered <- PD[complete.cases(PD), ]

# Question 3
set.seed(319946955)
train.row = sample(1:nrow(PD_filtered), 0.7*nrow(PD_filtered))
PD.train = PD_filtered[train.row,]
PD.test = PD_filtered[-train.row,]

# Question 4

# Decision Tree
PD.tree = tree(Class ~., data = PD.train)
plot(PD.tree)
text(PD.tree)

# Naive Bayes
set.seed(319946955)
PD.nav.fit = naiveBayes(Class ~ . , data = PD.train)


set.seed(319946955)  
sub <- sample(1:nrow(PD.train), 750, replace = FALSE)

# Bagging 
PD.ibag.fit = bagging(Class ~ ., data = PD.train, mfinal = 10)

# Boosting
PD.iboost <- boosting(Class ~ ., data = PD.train, mfinal = 10)

# Random Forest
PD.randomforest <- randomForest(Class ~., data=PD.train)

# Question 5 - Accuracy

# Decision Tree
PD.tree.predict <- predict(PD.tree, PD.test, type = "class")
PD.tree.conf_matrix <- confusionMatrix(data = PD.tree.predict, reference = PD.test$Class)
PD.tree.accuracy <- PD.tree.conf_matrix$overall["Accuracy"]
print(paste("Decision Tree Accuracy:", PD.tree.accuracy))

# Naive Bayes
PD.nav.predict <- predict(PD.nav.fit, PD.test)
PD.nav.conf_matrix_nb <- confusionMatrix(data = PD.nav.predict, reference = PD.test$Class)
PD.nav.accuracy <- PD.nav.conf_matrix_nb$overall["Accuracy"]
print(paste("Naive Bayes Accuracy:", PD.nav.accuracy))

# Bagging
PD.ibag.predict <- predict.bagging(PD.ibag.fit, newdata = PD.test)
PD.ibag.predict_factor <- factor(PD.ibag.predict$class, levels = levels(PD.test$Class))
PD.ibag.conf_matrix <- confusionMatrix(data = PD.ibag.predict_factor, reference = PD.test$Class)
PD.ibag.accuracy <- PD.ibag.conf_matrix$overall["Accuracy"]
print(paste("Bagging Accuracy:", PD.ibag.accuracy))

# Boosting 
PD.iboost.predict <- predict.boosting(PD.iboost, newdata = PD.test)
PD.iboost.predict_factor <- factor(PD.iboost.predict$class, levels = levels(PD.test$Class))
PD.iboost.conf_matrix <- confusionMatrix(data = PD.iboost.predict_factor, reference = PD.test$Class)
PD.iboost.accuracy <- PD.iboost.conf_matrix$overall["Accuracy"]
print(paste("Boosting Accuracy:", PD.iboost.accuracy))

# Random forest
PD.randomforest.predict <- predict(PD.randomforest,PD.test)
PD.randomforest.conf_matrix <- confusionMatrix(data = PD.randomforest.predict, reference = PD.test$Class)
PD.randomforest.accuracy <- PD.randomforest.conf_matrix$overall["Accuracy"]
print(paste("Random Forest Accuracy:", PD.randomforest.accuracy))

# 결론적으로 각 모델의 정확도에 대해서 알았다. ~는 ~고 ~는 ~다. 또한, 가장 높은 정확도를 가진 모델은 Boosting 으로 나왔다.


# Question 6 - ROC week 8 video 3 material
# Calculate of predicting for each case
# Construct an ROC curve for each classifier.

# Decision Tree
PD.pred.tree <- predict(PD.tree,PD.test,type = "vector")
PDT.pred <- prediction( PD.pred.tree[,2], PD.test$Class) 
PDT.perf <- performance(PDT.pred,"tpr","fpr")
plot(PDT.perf, col = "lightpink")
abline(0,1)
title("Performance of Different Models")
# Naive Bayes
PD.pred.bayes <- predict(PD.nav.fit,PD.test,type = "raw")
PDN.pred <- prediction (PD.pred.bayes[,2], PD.test$Class)
PDN.perf <- performance(PDN.pred,"tpr","fpr")
plot(PDN.perf, add=TRUE, col = "blueviolet")

# Bagging 
PDBA.pred <- prediction(PD.ibag.predict$prob[,2], PD.test$Class)
PDBA.perf <- performance(PDBA.pred,"tpr","fpr")
plot(PDBA.perf, add=TRUE, col = "darkblue")

# Boosting
PD.pred.boost <- predict.boosting(PD.iboost,PD.test, mfinal = 10)
PDBO.pred <- prediction(PD.pred.boost$prob[,2], PD.test$Class)
PDBO.perf <- performance(PDBO.pred,"tpr","fpr")
plot(PDBO.perf, add=TRUE, col = "red")

# Random Forest
PD.pred.rf <- predict(PD.randomforest, PD.test, type="prob")
PDRF.pred <- prediction (PD.pred.rf[,2],PD.test$Class)
PDRF.perf <- performance(PDRF.pred, "tpr","fpr")
plot(PDRF.perf, add=TRUE, col = "darkgreen")

legend_text <- c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest")
legend("bottomright", legend=legend_text, col=c("lightpink", "blueviolet", "darkblue", "red", "darkgreen"), lty=1, lwd=2)

# Question 7
classifiers_name <- c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest")
accuracy_of_classifiers <- c(PD.tree.accuracy,
                             PD.nav.accuracy,
                             PD.ibag.accuracy,
                             PD.iboost.accuracy,
                             PD.randomforest.accuracy)
accuracy_of_classifiers_rounded <- round(accuracy_of_classifiers, 4)
classifier_table <- data.frame(Classifier = classifiers_name, Accuracy = accuracy_of_classifiers_rounded)

classifier_table

# 따라서, 간발의 차이로 Random Forest 가 정확도 0.8029로 가장 높은 점수를 받은걸 확인할 수 있었다.
# 하지만, Bagging 의 정확도가 0.805인걸 보면 둘이 굉장히 유사한 정확도를 가지고 있음을 알 수 있었다.
# 결론적으로, Random Forest 가 single "best" classifier 라고는 단정지을 수 없다고 생각한다.


# Question 8

 


