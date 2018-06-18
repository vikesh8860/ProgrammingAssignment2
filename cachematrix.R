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
## and if inverse is calculated previously then it returns the cached inverse
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



## Steps to reproduce the solution
## Let the matrix is a 3X3 square matrix with elements as 
##  [1, 2, 3]
##  [4, 4, 6]
##  [7, 0, 9]
## mat<-matrix(c(1,2,3,4,4,6,7,0,9),3,3)
## m1 <- makeCacheMatrix(mat)
## cacheSolve(m1)
