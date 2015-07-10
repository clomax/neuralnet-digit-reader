test.randInitWeights <- function() {

    # randInitWeights randomly generates a set of weights of a given size
    # with each value residing between +ve/-ve epsilon

    # Does the function return the right sized set of weights?
    input_size = 5
    output_size = 3
    epsilon = 1

    OUT <- randInitWeights(input_size, output_size, epsilon)
    # 6, not 5, due to concatenating the bias
    checkEquals(dim(OUT), c(6,3))
}
