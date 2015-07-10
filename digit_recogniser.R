source('gradientDescent.R')
source('load_mnist.R')
source('predict.R')
source('randInitWeights.R')


digit_recogniser <- function(Hidden, epochs, alpha, lambda, epsilon,
                             training_set_size, cv_set_size, test_set_size) {

# Load dataset images and labels
cat("Loading training set...\n")
train_set <- loadImages(paste("data/train_images", sep=""))
train_labels <- loadLabels(paste("data/train_labels", sep=""))
cat("Loading test set...\n")
test_set <- loadImages(paste("data/test_images", sep=""))
test_labels <- loadLabels(paste("data/test_labels", sep=""))

# Reduce the size of the sets to the size
#  specified by the argument
size <- 1:train_set$n
TrainIndex <- sample(size, training_set_size)
TrainSet <- as.matrix(train_set$x[TrainIndex,])
TrainLabels <- train_labels[TrainIndex]

# For the cross-validation set, select those that are
#  NOT in the training set
CVset <- as.matrix(train_set$x[-TrainIndex,])[1:cv_set_size,]
CVlabels <- (train_labels[-TrainIndex])[1:cv_set_size]

size <- 1:test_set$n
TestIndex <- sample(size, test_set_size)
TestSet <- as.matrix(test_set$x[TestIndex,])
TestLabels <- test_labels[TestIndex]

# R converts vectors into a column matrix
#  correct this by assigning the lone
#  example to its transpose
if(training_set_size == 1) {
    TrainSet <- t(TrainSet)
}
if(cv_set_size == 1) {
    CVset <- t(CVset)
}
if(test_set_size == 1) {
    TestSet <- t(TestSet)
}


# Determine the network architecture from the dimensions of the data
In <- ncol(TrainSet)
Out <- length(levels(factor(train_labels)))
Architecture <- c(In, Hidden, Out)

# Remove loaded dataset to conserve memory
rm(train_set, train_labels, test_set, test_labels)

# Create identity matrix for the label of each training example
identity <- diag(Out)
Y <- matrix(0,length(TrainLabels),Out)
for(i in 1:length(TrainLabels)) {
    Y[i,] <- identity[TrainLabels[i]+1,]
}

# Create identity matrix for the label of each cross validation example
identity <- diag(Out)
CV_Y <- matrix(0,length(CVlabels),Out)
for(i in 1:length(CVlabels)) {
    CV_Y[i,] <- identity[CVlabels[i]+1,]
}

# Create identity matrix for the label of each test example
identity <- diag(Out)
Test_Y <- matrix(0,length(TestLabels),Out)
for(i in 1:length(TestLabels)) {
    Test_Y[i,] <- identity[TestLabels[i]+1,]
}

# Initialise Theta with random weights between +/-epsilon
Theta1 <- randInitWeights(In, Hidden, epsilon)
Theta2 <- randInitWeights(Hidden, Out, epsilon)
Theta <- as.matrix(c(Theta1, Theta2))

results <- gradientDescent(TrainSet, Y, Architecture, Theta, alpha, lambda, epochs)

Theta <- as.matrix(results[[1]])
J_hist <- as.matrix(results[[2]])

write.table(Theta, "weights/post_train_theta.csv", row.names=FALSE, col.names=F, sep=",")

png(filename="img/j.png")
plot(J_hist,type="l",xlab="Epoch",ylab="Error",ylim=c(0,max(J_hist)),
    panel.first=c(abline(v=(seq(-1000,epochs,epochs/10)),col="gray",
    lty=1),abline(h=(seq(-1000,max(J_hist),0.1)),col="gray",lty=1)))
title(main=paste("GradientDescentResults\n", Hidden,
    "hidden units,Learningrate:",alpha,"\n","Training
    examples:",training_set_size,"\n",Sys.time()))
dev.off()

# Get a set of predictions on the train set after training
cat("Getting predictions on training set...\n")
sink("train_predictions.txt")
correct=0
for(i in 1:training_set_size) {
    p <- prediction(Architecture, Theta, t(as.matrix(TrainSet[i,])))
    p = which.max(p)-1 #Subtract 1 because labels are off by 1
    cat(sprintf("Example %d: %d %d\n", i, TrainLabels[i], p))
    if(TrainLabels[i] == p) {
        correct = correct + 1
    }
}
acc <- (correct / training_set_size) * 100
cat(sprintf("Accuracy: %f\n", acc))
sink()

# Print training set error to file for plotting learning curves
write.table(cbind(tail(J_hist, 1), training_set_size), "train_error.txt", row.names=F, col.names=F, append=T)


# Print cross-validation data to file for plotting learning curves
cat("Getting cross-validation set predictions...\n")
CV_p <- matrix(0, nrow=cv_set_size, ncol=Out)
for(i in 1:cv_set_size) {
    CV_p[i,] <- prediction(Architecture, Theta, t(as.matrix(CVset[i,])))
}


# Get a set of predictions on the test set after training
cat("Getting test set predictions...\n")
sink("test_predictions.txt")
Test_p <- matrix(0, nrow=test_set_size, ncol=Out)
correct=0
for(i in 1:test_set_size) {
    Test_p[i,] <- prediction(Architecture, Theta, t(as.matrix(TestSet[i,])))
    p = which.max(Test_p[i,])-1    #Subtract 1 because labels are off by 1
    cat(sprintf("Example %d: %d %d\n", i, TestLabels[i], p))
    if(TestLabels[i] == p) {
        correct = correct + 1
    }
}
acc <- (correct / test_set_size) * 100
cat(sprintf("Accuracy: %f\n", acc))
sink()

}
