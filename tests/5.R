test.prediction <- function() {

    # The prediction function takes a set of examples
    # and a set of weights, returns a set of predictions
    # on the given examples

    Arch <- c(5,5,3)
    theta1 <- randInitWeights(Arch[1], Arch[2], 0.1)
    theta2 <- randInitWeights(Arch[2], Arch[3], 0.1)
    Theta <- as.matrix(c(theta1,theta2))
    X <- matrix(runif(15, min=0, max=1), nrow=3, ncol=Arch[1])

    OUT <- prediction(Arch, Theta, X)

    checkEquals(dim(OUT), c(3,3))
}
