
require(randomForest)
require(rpart)
require(rpart.plot)
require(rattle)
require(caret)

set.seed(8675309)

d01_training <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"))
d02_testing <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"))

d01_training$classe <- as.factor(d01_training$classe)

nameskeep <- names(d01_training)[sapply(d01_training, function (x) any(is.na(x) | x == ""))!=TRUE]
nameskeep <- nameskeep[-c(1:7)]

d01_trainingB <- d01_training[,nameskeep]


x <- createDataPartition(y = d01_trainingB$classe, p = 0.7, list=FALSE)
x1 <- d01_trainingB[x,]
x2 <- d01_trainingB[-x,]

modFit_rp <- train(classe ~ .,data = x1, method = 'rpart')
modPred_rp <- predict(modFit_rp, newdata=x2)
confusionMatrix(modPred_rp, x2$classe)

modFit_rf <- randomForest(classe ~. , data=x1)
modPred_rf <- predict(modFit_rf, newdata=x2)
confusionMatrix(modPred_rf, x2$classe)
z <- confusionMatrix(modPred_rf, x2$classe)
xtable(z$table)
1-z$overall[1]

xtable(predict(modFit_rf,newdata = d02_testing))

