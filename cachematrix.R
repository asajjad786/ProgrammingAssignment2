## To call the function do the following:
## M <- matrix(c(4,3,3,2), nrow=2, ncol=2)
## cacheMatrix <- makeCacheMatrix(M)
## cacheSolve(cacheMatrix)

## This function computes the inverse of the matrix and caches the result
makeCacheMatrix <- function(x = matrix()) {
    matrixInverse <- NULL
    
    ## Sets a given matrix y to x
    set<-function(y) {
        x <<- y
        matrixInverse <<- NULL
    }

    ## Returns the original matrix
    get <- function() {
        x
    }

    ## sets the inversedMatrix to inverseMatrix. matrixInverse is the resultant matrix.
    setInverse <- function(inverse){
        matrixInverse<<-inverse
    }

    ## Returns the resultant Inverse of the matrix
    getInverse<-function() {
        matrixInverse
    }
    
    list(set=set, get=get, setInverse=setInverse,getInverse=getInverse)
}


## This function returns the Inverse of the given matrix. 
## If inverse of the matrix doesn't exist in the cache, it computes, saves it in the cache and returns it
## If inverse of the matrix exists then it simply returns it from the cache.
cacheSolve <- function(x, ...) {
  
    ## Get the matrixInverse
    matrixInverse <-x$getInverse()
    
    # If already exist return it
    if(!is.null(matrixInverse)) {
        print ("Get it from cache ...")
        return (matrixInverse)
    }

    ## If it doesn't exist get the original matrix
    originalMatrix <-x$get()

    ## Compute the inverse of the matrix
    matrixInverse <- solve(originalMatrix)

    ## Cache it by calling the setInverse function
    x$setInverse(matrixInverse)

    ## Return the inverse of the Matrix
    matrixInverse
}
