cat("\nSTART\n")
startTime = proc.time()[3]
startTime

library(nnet)

modelName <- "neuralNetwork"
InputDataFileName="E:/Tech MAhindra Project/DigitRecognition-master/mnist_train.csv"

trainDataset <- read.csv(InputDataFileName)      
trainDataset <- trainDataset[sample(nrow(trainDataset)),] 
View(trainDataset)
head(trainDataset)   
nrow(trainDataset)   
names(trainDataset)  

cat("\nStep 5: Choose Target Variable")
target  <- names(trainDataset)[1] #label name
target

cat("\nStep 6: Choose Inputs Variable")
inputs <- setdiff(names(trainDataset),target)
inputs
length(inputs)

cat("\nStep 7: Select training dataset")

head(trainDataset)  
nrow(trainDataset)    

X=trainDataset[,-1] 
Y=trainDataset[,1] 
Xreduced <- X/255

Xcov <- cov(Xreduced)
pcaX <- prcomp(Xcov)

Xfinal <- as.matrix(Xreduced) %*% pcaX$rotation[,1:45]
Y <- class.ind(Y)

cat("\nStep 8: Select testing dataset")
testDataset <- read.csv("E:/Tech MAhindra Project/DigitRecognition-master/mnist_test.csv")
View(testDataset)
head(testDataset)
nrow(testDataset)

test=testDataset[,-1]
testreduced <- test/255 
testreduced <- as.matrix(testreduced) %*% pcaX$rotation[,1:45]

cat("\nStep 9: Model Building -> ", modelName)



model <- nnet(Xfinal,Y,size=150,softmax=TRUE,maxit=130,MaxNWts =80000)
model


cat("\nStep 10: Prediction using -> ", modelName)
Predicted <- predict(model, testreduced,type="class")
Predicted <- as.data.frame(Predicted);
head(Predicted) 

cat("\nStep 11: Extracting Actual")
Actual <- as.double(unlist(testDataset[target]))
head(Actual)

cat("\nStep 12: Model Evaluation")

totalTime = proc.time()[3] - startTime
totalTime

result <- data.frame(modelName,accuracy, totalTime)[1:1,]
result

cat("\nStep 13: Writing to file")

write.csv(result, file=paste(modelName,"-Evaluation-Result.csv",sep=''), row.names=FALSE)


write.csv(data.frame(Actual,Predicted), file=paste(modelName,"-ActualPredicted-Result.csv",sep=''), row.names=FALSE)


cat("\nStep 14: Saving the Model ->",modelName)
save.image(file=paste(modelName,"-Model.RData",sep=''))

cat("\nDone")
cat("\nTotal Time Taken: ", totalTime," sec")