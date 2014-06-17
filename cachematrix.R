## The following functions creates a special object
## to be able to cache the inverse of a matrix
## so it doesn't have to be calculated repeatedly from
## w/in a program

## returns a list of functions to get and set the matrix
## and get and set the cached matrix

makeCacheMatrix <- function(x = matrix()) {
    c <- NULL
    set <- function(y) {
        x <<- y
        c <<- NULL ## this resets the cache b/c the matrix has been changed
    }
    get <- function() x
    setcache <- function(solve) c <<- solve
    getcache <- function() c
    list(set = set, get = get,
         setcache = setcache,
         getcache = getcache)
}


## This function checks to see if the inverse matrix
## has already been calculated and if not, it calculates
## it and sets it so it doesn't have to be done again

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    c <- x$getcache()
    if(!is.null(c)) {
        message("getting cached data")
        return(c)
    }
    matrix <- x$get()
    c <- solve(matrix, ...) #calculate inverse
    x$setcache(c)
    c
}
