test.splitTheta <- function() {

    # splitTheta takes the concatenated Theta and splits
    # it according to the architecture of the neural network

    # Does the function return a pair of proportionate weights for
    # the connections between each layer?

    Arch <- c(5,5,3)
    theta1 <- randInitWeights(Arch[1], Arch[2], 0.1)
    theta2 <- randInitWeights(Arch[2], Arch[3], 0.1)
    Theta <- as.matrix(c(theta1,theta2))

    OUT <- splitTheta(Arch, Theta)
    t1 <- as.matrix(OUT[[1]])
    t2 <- as.matrix(OUT[[2]])

    # Add 1 due to bias term
    checkEquals(dim(t1), c(Arch[1]+1, Arch[2]))
    checkEquals(dim(t2), c(Arch[2]+1, Arch[3]))

}
