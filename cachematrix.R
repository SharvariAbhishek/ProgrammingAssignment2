## makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

inv_mat <- NULL
set <- function(y){
x <<- y
inv_mat <<- NULL
}

get <- function() x
setInverse <- function(solve) inv_mat <<- solve(x)
getInverse <- function() inv_mat
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

##The following function calculates the inverse of the special matrix created with the above "makeCacheMatrix" function.## it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skip##s the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via##the setInverse function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

inv_mat <- x$getInverse()

if(!is.null(inv_mat)) {
message("getting cached data")
return(inv_mat)
}

data <- x$get()
inv_mat <- solve(data, ...)
x$setInverse(inv_mat)
inv_mat
}
