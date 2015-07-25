
## The makeCacheMatrix function creats a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set<-function(y){
		x<<-y
		i<<-NULL
	}
	get<-function() x
	
	setInverse <-function(inverse){
		i<<-inverse
	}
	getInverse<-function() i
	
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated, then this function retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	i<-x$getInverse()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	m <- x$get()
	i<-solve(m, ...)
	x$setInverse(i)
	i
}
