cost <- function(h,y) {
    m <- nrow(h)
    return( (1/(2*m)) * sum((h-y)^2) )
}
