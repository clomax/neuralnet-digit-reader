test.gradientDescent <- function() {

    # The gradient descent function takes a training set,
    # training labels, weights, and number of epochs,
    # returns a history of the cost of each epoch and
    # the trained weights

    Arch <- c(5,5,3)
    y <- matrix(sample(0:1,9,T), nrow=3, ncol=Arch[3])
    theta1 <- randInitWeights(Arch[1], Arch[2], 0.1)
    theta2 <- randInitWeights(Arch[2], Arch[3], 0.1)
    Theta <- as.matrix(c(theta1,theta2))
    X <- matrix(runif(15, min=0, max=1), nrow=3, ncol=Arch[1])
    epochs = 1

    OUT <- gradientDescent(X,y,Arch,Theta,0.1,0,epochs)
    t <- as.matrix(OUT[[1]])
    j <- as.matrix(OUT[[2]])

    # Does the size of the new weights set match the
    # proper weight set's size?
    checkEquals(dim(t), dim(Theta))

    # Does the history of the cost match the number of epochs?
    checkEquals(length(j), epochs)

}
