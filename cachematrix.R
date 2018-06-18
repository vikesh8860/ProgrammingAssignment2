# makeCacheMatrix method for caching purpose
makeCacheMatrix <- function( mat = matrix() ) {
        
        ## initialise the inv variable as null
        inv <- NULL
        set <- function( y ) {
                mat <<- y
                inv <<- NULL
        }
        get <- function()    mat
        setinverse <- function(inverse)    inv <<- inverse
        getinverse <- function()    inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## calculate inverse of the  matrix returned by "makeCacheMatrix"
cacheSolve <- function(x, ...) {
        mat <- x$getinverse()
        if( !is.null(mat) ) {
                message("getting cached data")
                return(mat)
        }
        data <- x$get()
        mat <- solve(data) %*% data
        x$setinverse(mat)
        mat
}
