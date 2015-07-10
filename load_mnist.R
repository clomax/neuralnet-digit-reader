loadImages <- function(f) {
    r <- list()
    f <- file(f,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    r$n <- readBin(f,'integer',n=1,size=4,endian='big')
    row <- readBin(f,'integer',n=1,size=4,endian='big')
    col <- readBin(f,'integer',n=1,size=4,endian='big')
    x <- readBin(f,'integer',n=r$n*row*col,size=1,signed=F)
    r$x <- matrix(x, ncol=row*col, byrow=T)
    close(f)
    return(r)
}

loadLabels <- function(f) {
    f <- file(f, 'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n <- readBin(f,'integer',n=1,size=4,endian='big')
    y <- readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    return(y)
}
