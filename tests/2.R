test.cost <- function() {

    # The cost function takes in a hypothesis, the labels,
    # and the number of examples in the set, and returns
    # a matrix of the Mean Squared Error between the hypotheses
    # and the labels (correct class) of each example given

    x = c(0.745, 0.123, 0.546, 0.432, 0.657, 0.931)
    y = c(0,1,0,1,1,0)

    # Does it return the correct value for a single contrived example
    # with two outputs?

    h <- t(matrix(x[1:3]))
    y <- t(matrix(y[1:3]))

    OUT <- cost(h, y)
    checkEquals(OUT,0.811135)

    # Does it return the correct class for multiple
    # examples?
    h <- matrix(x, nrow=2, ncol=3)
    y <- matrix(y, nrow=2, ncol=3)

    OUT <- cost(h, y)
    checkEquals(OUT, 0.698326)
}

