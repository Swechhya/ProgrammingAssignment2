## The functions below creates a special "matrix" and computes the inverse of 
## that special matrix which can be cached.


## Creates special "matrix" object that can cache its inverse.
## Here x is the matrix input given by the user and i is the 
## i is the inverse matrix computed. 

makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set<-function(y){
		x<<-y
		i<<-NULL
	}
	get <- function()x
	setinverse <- function(inverse) i<<-inverse
	getinverse <- function() i
	list(set=set, get=get,
		setinverse = setinverse,
		getinverse = getinverse)
}



## Computes the inverse of the special "matrix" returned by the 
## makeCacheMatrix above.If the already been calculated and the 
## matrix has not changed then the cacheSolve retrives the inverse 
## from cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	i<-x$getinverse()
	if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i

	

}
