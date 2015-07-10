source('sigmoid.R')

train <- function(Arch, Theta, X, y) {

    m <- nrow(y)

    # Get Theta1/2 back, use them to initialise error counterpart matrices
    t <- splitTheta(Arch, Theta)
    Theta1 <- as.matrix(t[[1]])
    Theta2 <- as.matrix(t[[2]])

    DELTA2 <- matrix(0, nrow(Theta2), ncol(Theta2))
    DELTA1 <- matrix(0, nrow(Theta1), ncol(Theta1))


    # Forward propagation

    # Prepend bias term to each row in the input layer
    a1 <- as.matrix(cbind(1, X))
    z2 <- a1 %*% Theta1
    a2 <- sigmoid(z2)
    a2 <- as.matrix(cbind(1, a2))

    z3 <- a2 %*% Theta2
    a3 <- sigmoid(z3)


    # Backpropagation
    delta3 <- a3 - y
    delta2 <- (delta3 %*% t(Theta2[2:nrow(Theta2),])) * sigmoidGradient(z2)

    DELTA2 <- DELTA2 + (t(a2) %*% delta3)
    DELTA1 <- DELTA1 + (t(a1) %*% delta2)

    grad <- c(DELTA1,DELTA2)

    return(list(a3, grad))
}
