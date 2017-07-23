#------------------------------------Load and clean the data set-----------------------------------
data=read.csv("data.csv")
str(data)
data <- data[,-1]
data <- data[,-32]
contrasts(data$diagnosis)
str(data)

#------------------------------------Splitting the data into training and test sets---------

set.seed(22)
split=sample.split(data$diagnosis, SplitRatio = 0.80)

train=subset(data, split==TRUE)
test=subset(data, split==FALSE)

#-----------------------Logistic Regression------------------------

model.glm.0=glm(diagnosis~.,data=train, family = binomial)
predict.glm.0=predict(model.glm.0, type = 'response', newdata = test)
table(test$diagnosis, predict.glm.0 > 0.5)
tapply(predicttest, test$diagnosis, mean)


#Plot ROC curve using ROCR and calculation of auc 

pr <- prediction(predicttest, test$diagnosis)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
plot(prf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2, 1.7))

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# anova test 

anova(model.glm.0, test="Chisq")


#-------------------Variable Selection----------------------------

leaps <- regsubsets(diagnosis~., data=train, nbest = 2)
plot(leaps, scale="adjr2")
plot(leaps, scale="bic")

#------New Logistic Regression Model with the best subset of variables---------

model.glm.1=glm(diagnosis~smoothness_se+radius_worst+texture_worst+area_worst+concave.points_worst, data=train, family = "binomial")
predict.glm.1=predict(model.glm.1, type = 'response', newdata = test)
table(test$diagnosis, predict.glm.1 > 0.5)

pr <- prediction(predicttest, test$diagnosis)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
plot(prf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2, 1.7))

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

#--------PCA and Logistic Regression-----

pca_ver=prcomp(data[,2:31], center = TRUE, scale. = TRUE)
plot(pca_ver, type="l")

summary(pca_res)
pca_df <- as.data.frame(pca_res$x)
pca_df$diagnosis=data$diagnosis

train=subset(pca_df, split==TRUE)
test=subset(pca_df, split==FALSE)

model.glm.pca.0=glm(diagnosis~., data=train, family = "binomial")
predict.glm.pca.0=predict(model.glm.pca.0, type = 'response', newdata = test)
table(test$diagnosis, predict.glm.0 > 0.5)

model.glm.pca.1=glm(diagnosis~., data=train, family = "binomial")
predict.glm.pca.1=predict(model.glm.pca.1, type = 'response', newdata = test)
table(test$diagnosis, predict.glm.1 > 0.5)

model.glm.pca.2=glm(diagnosis~., data=train, family = "binomial")
predict.glm.pca.2=predict(model.glm.pca.2, type = 'response', newdata = test)
table(test$diagnosis, predict.glm.2 > 0.5)

#---------------------K-means Clustering-------------------

scale_ver=as.data.frame(scale(data[2:31]))

# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters

wss <- sapply(1:k.max, 
              function(k){kmeans(scale_ver, k, nstart=10 )$tot.withinss})

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
abline(v = 2, lty =2)


model.kmeans <- kmeans(scale_ver, 2, nstart = 20)
clusplot(data, model.kmeans$cluster)
table(data$diagnosis, model.kmeans$cluster)

#---------------------K nearest algorithm-----------------

train0=train[,-1]
test0=test[,-1]
train_lables=train$diagnosis

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)

#Finding the optimal knn parameters
fit.knn <- train(diagnosis~., data=train, method="knn", metric=metric, trControl=control)

summary(fit.knn)

model.knn <- knn(train = train0, test = test0, cl=train_lables, k=9)


#------------Tree based algorithms------------

fit.tree <- train(diagnosis~., data=train, method="rpart", metric=metric, trControl=control)
prp(tree)
predicttree=predict(tree, newdata = test, type = "class")
table(test$diagnosis, predicttree)

fit.ada <- train(diagnosis~., data=train, method="adaboost", metric=metric, trControl=control)
fit.treebag <- train(diagnosis~., data=train, method="treebag", metric=metric, trControl=control)
fit.rf <- train(diagnosis~., data=train, method="rf", metric=metric, trControl=control)

#-------------------------------------------

