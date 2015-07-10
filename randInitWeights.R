randInitWeights <- function(In, Out, epsilon) {
    bias <- 1
    return(array(runif(Out * (In+bias), -epsilon, epsilon), dim=c(In+bias,Out)))
}
