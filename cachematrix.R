## Code based on example provided by John Hopkins R Programming course 
## for similar functionality using vectors and means, but for the inverse of matrices instead

## function that provides a list of functions for getting and setting a matrix and the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	
	set <- function(y){
		x<<-y
		m<<-NULL
	}
	
	get <- function() x
	
	setinverse <- function(solve) m <<- solve
	
	getinverse <- function() m
	
	return(list(set=set, get=get, setinverse=setinverse, getinverse=getinverse))
}


## function to calculate the inverse of a matrix, caching that inverse matrix so that if called again
## it doesn't need to recalculate

cacheSolve <- function(x, ...) {
		
    m<-x$getinverse()
	
    if(!is.null(m)){
		message("getting cached data")
		return(m)
    }
	
    matrix <- x$get()
    
	m <- solve(matrix, ...)
    
	x$setinverse(m)
    
	return(m)		
}
