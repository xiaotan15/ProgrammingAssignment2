## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first matrix, makeCacheMatrix, creates a special matrix, 
##which is really a list containing function to: 
##1. set the value of the matrix; 2. get the value of the matrix; 
##3. set the value of the inverse of the matrix; 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

        matrix <- NULL
        SET <- function(y){
                x <<- y
                matrix <<- NULL
        }
        get <- function() x
        setinverse <- function (inverse) matrix <<- inverse 
        getinverse <- function() matrix
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Write a short comment describing this function

##The following function computes the inverse of the special matrix returned by makeCacheMatrix above;
##it first checks whether the inverse has already been calculated or not. 
##Otherwise, it calculates the inverse of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix <- x$getinverse()
        if(!is.null(matrix)){
                message{"getting cached data"}
                return(matrix)
        }
        data <- x$get()
        matrix <- solve(data,...)
        x$setinverse(matrix)
        matrix
        
}
