sigmoid <- function(z) {
    return(1.0 / (1.0 + exp(-z)))
}

sigmoidGradient <- function(z) {
    return(sigmoid(z) * (1 - sigmoid(z)))
}
