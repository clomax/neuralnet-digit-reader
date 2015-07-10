test.sigmoid <- function() {

    # Are the correct dimensions are returned?
    #   The logistic functions should return
    #   a datastructure of the same size as
    #   that which was passed in.

    # First check if a 1x1 returns a 1x1
    IN <- 1
    OUT <- sigmoid(IN)
    checkEquals(length(OUT), 1)


    # Do they work for matrices?
    IN <- matrix(runif(9,-1,1), nrow=3, ncol=3)
    OUT <- sigmoid(IN)
    checkEquals(dim(IN), dim(OUT))

}

test.sigmoidGradient <- function() {

    IN <- 1
    OUT <- sigmoidGradient(IN)
    checkEquals(length(OUT), 1)

    IN <- matrix(runif(9,-1,1), nrow=3, ncol=3)
    OUT <- sigmoidGradient(IN)
    checkEquals(dim(IN), dim(OUT))

}
