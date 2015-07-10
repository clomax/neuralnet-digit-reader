source('cost.R')
source('train.R')

gradientDescent <- function(X,y,Arch,Theta,alpha,lambda,epochs) {

    m <- nrow(X)
    J_hist <- matrix(0,epochs,1)

    t <- splitTheta(Arch, Theta)
    Theta1 <- as.matrix(t[[1]])
    Theta2 <- as.matrix(t[[2]])

    for (i in 1:epochs) {
        cat(sprintf("Epoch: %s of %s\n", i, epochs))
        BP <- train(Arch,Theta,X,y)
        h <- BP[[1]]
        grad <- as.matrix(BP[[2]])

        # compute regularisation term
        Theta1_no_bias <- Theta1[2:nrow(Theta1),]
        Theta2_no_bias <- Theta2[2:nrow(Theta2),]
        reg <- ((lambda/(2*m)) * (sum(sum(Theta1_no_bias^2))
                + sum(sum(Theta2_no_bias^2))) )

        J_hist[i] <- cost(h,y) + reg
        Theta <- Theta - (alpha * 1/m) * grad
    }

    return(list(Theta, J_hist))
}
