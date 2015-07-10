test.train <- function() {

    # The train function performs a forward propagation procedure
    # to determine what the neural network thinks the answers are
    # currently, then immediately backpropagates to find the error
    # in each layer, and calculates the changes to be made to the
    # weights

    # Does the function return the correct size output and set of
    # gradients for a contrived example?
    Arch <- c(5,5,3)
    y <- matrix(sample(0:1,9,T), nrow=3, ncol=Arch[3])
    theta1 <- randInitWeights(Arch[1], Arch[2], 0.1)
    theta2 <- randInitWeights(Arch[2], Arch[3], 0.1)
    Theta <- as.matrix(c(theta1,theta2))
    X <- matrix(runif(15, min=0, max=1), nrow=3, ncol=Arch[1])

    OUT <- train(Arch, Theta, X, y)
    hypothesis <- as.matrix(OUT[[1]])
    gradients <- as.matrix(OUT[[2]])

    # Is the hypothesis the same size as expected?
    # Does the size of the gradient match that of the weights?
    checkEquals(dim(hypothesis), c(3,3))
    checkEquals(dim(gradients), dim(Theta))
}
