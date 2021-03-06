<<<<<<< HEAD
makeCacheMatrix <- function(x = matrix()) {

# this function takes an invertible matrix, and will create a special matrix that can cache its inverse

# variable im is the solved(inverted matrix)

        im <- NULL                # initalize inverted matrix to NULL
        set <- function(y) {
                x <<- y           # set the matrix variable to the matrix passed
                                  # by the function call    

                im <<- NULL       # no inverted matrix at this point
        }
        get <- function() x
        setSolve <- function(solve) im <<- solve
        getSolve <- function() im
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}

# this function will calculate an inverse of a matrix created by 'makeCacheMatrix'
#  and calculate the inverse of the matrix the first time it is called for a specific matrix,
# but will return a cached copy of the inverse on subsequent calls for the same 
# matrix


cacheSolve <- function(x, ...) {
        im <- x$getSolve()        # check to see if Solve(inverse) exists
        if(!is.null(im)) {        # path if cached value is found
                message("getting cached data")   
                return(im)        # return cached value
        }
        data <- x$get()
        im <- solve(data, ...)    # solve(inverse) matrix
        x$setSolve(im)            # cache calculated inversion for next time
        im                        # return newly calculated inverse matrix

}

=======
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
>>>>>>> 7f657dd22ac20d22698c53b23f0057e1a12c09b7
