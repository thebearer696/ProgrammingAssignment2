## Description of arguments and steps employed in makeCacheMatrix()
## 1. m is initialized and it saves the inverse matrix later, this is the one that is cached.
## 2. x is the raw invertible square matrix
## 3. setImatrix() assigns inverse of x to m 
## 4. getImatrix() caches the matrix m

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
    get <- function() x
    setImatrix <- function(Imatrix) m <<- Imatrix
    getImatrix <- function() m

    # return a list of functions as an R object
    list(get=get, setImatrix=setImatrix, getImatrix=getImatrix)
}


## Description of arguments and steps employed in cacheSolve()
## 1. This function computes the inverse of matrix x.  It first checks if the inverse matrix has been found; 
## 2. if yes, returns the result and quits. If not, then inverse of x is calculated, cached, and returned.
## 3. x here(in this function) must be cached, i.e. a list returned from calling makeCacheMatrix(x).

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
 m <- x$getImatrix()
    if(!is.null(m)){
        message("Cached data found. Getting result... Done.")
        return(m)
    }
    else {
        message("No cached data found. Calculating inverse matrix...")
        data <- x$get() # obtains matrix from object x
        m <- solve(data) # finds inverse matrix
        x$setImatrix(m) # assigns resulting inverse matrix to object x
        message("Done.")
        return(m)
    }
}
