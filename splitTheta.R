splitTheta <- function(Arch, Theta) {

    In <- Arch[1]
    Hidden <- Arch[2]
    Out <- Arch[3]

    bias <- 1
    Theta1 <- matrix(Theta[1 : (Hidden * (In+bias))], nrow = In + bias, ncol = Hidden)
    Theta2 <- matrix(Theta[(Hidden * (In+bias)) +1 : length(Theta)], nrow = Hidden + bias, ncol = Out)

    return(list(Theta1, Theta2))
}
