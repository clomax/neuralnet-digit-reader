source('sigmoid.R')
source('splitTheta.R')

prediction <- function(Arch, Theta, X) {

    # Get Theta1/2 back
    t <- splitTheta(Arch, Theta)
    Theta1 <- as.matrix(t[[1]])
    Theta2 <- as.matrix(t[[2]])

    X <- as.matrix(cbind(1,X))
    h1 <- sigmoid(X %*% Theta1)

    h1 <- as.matrix(cbind(1,h1))
    h2 <- sigmoid(h1 %*% Theta2)

    return(h2)
}
