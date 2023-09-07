## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialize the inverse property
        m <- NULL
        
        ## Method to set the matrix
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        ## Method the get the matrix
        get <- function() x
        
        ## Method to set the inverse of the matrix
        setinverse <- function(inverse) m <<- inverse
        
        ## Method to get the inverse of the matrix
        getinverse <- function() m
        
        ## Return a list of the methods
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'm'
        m <- x$getinverse()
        
        ## Just return the inverse if its already set
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        m <- solve(data, ...)
        
        ## Set the inverse to the object
        x$setinverse(m)
        
        ## Return the matrix
        m
}

