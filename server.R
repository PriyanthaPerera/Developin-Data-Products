#rm(list=ls())
library(shiny)
library(ggplot2)
library(caret)
library(randomForest)
library(gbm)
library(dplyr)

#Generating random data
#The x values
xtrain <- seq(1, 2000, by = 5)
#Sinusoidal function values
a <- 20
b <- .06
ytrainsin <- a*sin(b*(xtrain)/(2*pi))
#Linear function
alpha <- 2
beta <- 0.1
ytrainline <- alpha +  beta*xtrain
#Error/Noise term
sigma <- 40
set.seed(12345)
errortrain <- rnorm(length(xtrain),0,sigma)
#Noise corrupted observations
ytrain <- ytrainsin+ytrainline+errortrain
#Keeping track of underlying non random function
ydeterministictrain <- ytrainsin+ytrainline
detplotdataAll <-as.data.frame(cbind(xtrain,ydeterministictrain))
head(detplotdataAll)
names(detplotdataAll) <- c("x", "y")

traindata <- as.data.frame(cbind(xtrain, ytrain))
#Splitting into train and test sets
set.seed(12345)
intrain <- createDataPartition(traindata$y, p =.75, list = FALSE)
training <- traindata[intrain,]
testing <- traindata[-intrain,]
names(training)<- c("x", "y")
names(testing)<- c("x", "y")


#Fitting all the model before hand
set.seed(12345)

#Linear Model
lmfit <- train(y~., data = training, method = "lm")
predlm <- predict(lmfit, newdata = testing)

#Random Forest
rffit <- train(y~., data = training, method = "rf")
predrf <- predict(rffit, newdata = testing)

#K-Nearest Neighbors
knnfit <- train(y~., data = training, method = "knn")
predknn <- predict(knnfit, newdata = testing)

#Gradient Boosting
gbmfit <- train(y~., data = training, method = "gbm")
predgbm <- predict(gbmfit, newdata = testing)



#RMSE's for all Models
RMSElm <- sqrt(sum((testing$y-predlm)^2)/length(predlm))
RMSErf <- sqrt(sum((testing$y-predrf)^2)/length(predrf))
RMSEknn <- sqrt(sum((testing$y-predknn)^2)/length(predknn))
RMSEgbm <- sqrt(sum((testing$y-predgbm)^2)/length(predgbm))
RMSE <-c(RMSElm,RMSErf,RMSEknn,RMSEgbm)
names(RMSE) <-c("RMSElm","RMSErf","RMSEknn","RMSEgbm")
#RMSElm;RMSErf;RMSEknn;RMSEgbm Need inside the server

# Setting up Plot Data
dettest <- detplotdataAll[-intrain,]
detdata <- as.data.frame(cbind(dettest))
names(detdata) <- c("x", "fity")
detplot <- ggplot(detdata, aes(x,fity)) + geom_point() + geom_line() + labs(list(title = "Underlying", x = "x", y = "y"))

obsdata <- as.data.frame(testing)
names(obsdata) <- c("x", "fity")
obsplot <- ggplot(obsdata, aes(x,fity)) + geom_point() + labs(list( title = "Observed", x = "x", y = "y"))

lmdata <- as.data.frame(cbind(testing$x, predlm))
names(lmdata) <- c("x", "fity")
lmplot <- ggplot(lmdata, aes(x,fity)) + geom_point() + labs(list( title = "Linear Regression", x = "x", y = "y"))


rfdata <- as.data.frame(cbind(testing$x, predrf))
names(rfdata) <- c("x", "fity")
rfplot <- ggplot(rfdata, aes(x,fity)) + geom_point() + labs(list( title = "Random Forest", x = "x", y = "y"))

knndata <- as.data.frame(cbind(testing$x, predknn))
names(knndata) <- c("x", "fity")
knnplot <- ggplot(knndata, aes(x,fity)) + geom_point() + labs(list(title = "K-Nearest Neighbors", x = "x", y = "y"))

gbmdata <- as.data.frame(cbind(testing$x, predgbm))
names(gbmdata) <- c("x", "fity")
gbmplot <- ggplot(gbmdata, aes(x,fity)) + geom_point() + labs(list(title = "Gradient Boosting", x = "x", y = "y"))

A<- cbind(detdata[,c(1:2)],"underlying")
names(A) <- c("x", "fity", "modelID")
B<- cbind(obsdata,"obs")
names(B) <- c("x", "fity", "modelID")
C<- cbind(gbmdata,"gbm")
names(C) <- c("x", "fity", "modelID")
Data <- rbind(B, C, A)
names(Data) <- c("x", "fity", "modelID")
cplot <- ggplot(Data, aes(x,fity))
compareplot <- cplot + geom_point() +geom_smooth(method = "lm" ) + facet_grid(.~modelID)




#head(Data)
#plotdata <-filter(Data, modelid == as.character("rf"))
#head(plotdata)
####SHINY SERVER####

shinyServer(
  function(input, output) {
    output$outplot <- renderPlot({
      if (input$model == "obs") {
        obsplot
      } else if (input$model == "lm") {
        lmplot
      } else if (input$model == "rf") {
        rfplot
      } else if (input$model == "knn") {
        knnplot
      } else if (input$model == "gbm") {
        gbmplot
      } else if (input$model == "det") {
        detplot
      }else if (input$model == "compare") {
        compareplot
      }

    })
    output$rmse <- renderPrint({
      if (input$model == "lm") {
        RMSElm
      } else if (input$model == "rf") {
        RMSErf
      } else if (input$model == "knn") {
        RMSEknn
      } else if (input$model == "gbm") {
        RMSEgbm
      }

    })


  })

